module Database.Concelo.Ignis
  ( ignis ) where

import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Protocol as P

data Auth = Auth { _authPrivate :: ByteString
                 , _authPublic :: ByteString
                 , _authRequest :: Int
                 , _authSent :: Bool }

data Ignis = Ignis { }

data Credentials = PrivateKey { _privateKey :: ByteString }
                 | EmailPassword { _email :: Text
                                 , _password :: Text }

ignis = Ignis

privateKeySizeInBytes = 32

authenticate (PrivateKey private) =
  authenticateWithPrivateKey private

authenticate (EmailPassword email password) =
  authenticateWithPrivateKey
  $ C.derivePrivate privateKeySizeInBytes password email

authenticateWithPrivateKey private =
  request <- get ignisNextRequest
  set ignisAuth $ Just $ Auth private (C.derivePublic private) request False
  nextRequest

nextRequest = do
  n <- get ignisNextRequest
  update ignisNextRequest (+1)
  return n

with lens f =
  (result, new) <- get lens >>= f
  set lens new
  return result

sign private challenge =
  with ignisPRNG $ C.sign private challenge

bind f x = do
  x' <- x
  case x' of
    Nothing -> return Nothing
    Just x'' -> f x''

bind2 f x y = do
  x' <- x
  y' <- y
  case x' of
    Nothing -> return Nothing
    Just x'' ->
      case y' of
        Nothing -> return Nothing
        Just y'' ->
          f x'' y''

maybeAuthenticate =
  bind2 try (get ignisAuth) (get ignisChallenge) where
      
    try auth challenge =
      if get' authSent auth then return Nothing else do
        set ignisAuth $ Just $ set' authSent True auth
        
        sign (get' authPrivate auth) challenge
          >>= P.serialize
          . P.Auth (get' authRequest auth) (get' authPublic auth)

getAuth = get (authPublic . ignisAuth)

unAuth = set ignisAuth Nothing

update update atomicUpdate = do
  head <- get ignisHead >>= return . update
  let atomicHead = atomicUpdate head
        
  eitherDiff <- makeDiff head atomicHead

  case eitherDiff of
    Right diff -> do
      set ignisHead head
      set ignisDiff diff
      return $ Right ()
      
    Left error ->
      return $ Left error

nextMessage = do
  maybeAuth <- get ignisAuth
  case maybeAuth of
    Nothing -> return Nothing
    Just _ -> do
      maybeMessage <- maybeAuthenticate

      case maybeMessage of
        Just message ->
          return $ Just message

        Nothing -> do
          maybeUpdateSync
      
          maybeNack <- get ignisNacks >>= return . T.firstPath
      
          case maybeNack of
            Just nack -> do
              update ignisNacks $ T.subtract nack
              update ignisAcks $ T.union nack
              return $ T.first nack

            Nothing -> return Nothing

mapPair f (x, y) = (f x, f y)

byACL path value = T.super (get' valueACL value) path

withSerializer f = do
  maybeAuth <- ignisAuth
  case maybeAuth of
    Nothing -> error "cannot encrypt without private key"
    Just auth -> 
      with ignisPRNG (f . MySerializer (get' authPrivate auth))

updateACLSyncTrees (obsolete, new) = do
  with ignisACLSyncTrees \trees ->
    withSerializer \serializer ->
      foldr update (((T.empty, T.empty), trees), serializer)
            $ T.union (T.keys obsolete) (T.keys new) where
    
    update acl (((obsoletes, news), trees), serializer) =
      (((T.union obsolete' obsoletes,
         T.union new' news),
        if ST.null tree' then
          T.delete acl trees else
          T.insert acl tree' trees),
       serializer') where
        (tree', serializer', obsolete', new') =
          ST.update (fromMaybe ST.empty $ T.find acl trees) serializer
          (T.sub acl obsolete) (T.sub acl new)

updateMySyncTree (obsolete, new) = do
  with ignisMySyncTree \tree ->
    withSerializer \serializer ->
      let (tree', serializer', obsolete', new') =
            ST.update tree serializer ? ? in
      (((T.union obsolete' obsolete,
         T.union new' new),
        tree'),
       serializer')

maybeUpdateSync = do
  maybeDiff <- get ignisDiff
  case maybeDiff of
    Nothing -> return ()
    Just diff ->
      updateACLSyncTrees $ mapPair (T.indexPaths byACL) diff
      >>= updateMySyncTree
      >>= updateRootSyncTree
      >>= updateAcksAndNacks

validUpdate path head = do
  me <- getAuth
  base <- get ignisBase
  rules <- get ignisBase rules    

  error "todo: apply rules and return Right ACL if valid or Left error otherwise"

makeDiff head atomicHead = do
  base <- get ignisBase
  foldM apply (Right (T.empty, T.empty)) $ T.diff base atomicHead where

    apply (operation, path) diff = do
      acl <- validateUpdate path atomicHead
      return (apply' <$> diff <*> acl) where
    
        apply' (obsolete, new) acl =
          Right case operation of
            T.Remove -> (T.union path obsolete, new)
            T.Add -> (obsolete, T.union (fmap (set' valueACL acl) path) new)


-- 1. compute diff (as sets of removes and adds)
  
-- 2. remove chunks from base for each remove in diff and add orphans (any value that's in a removed chunk but not in the set of removes) to set of adds

-- (2.5.) find remaining chunk with the most space available
  
-- 3. if there's a remaining chunk into which the entire set of adds will fit, remove that chunk and create a new one with the union of orphans and adds
  
-- 4. otherwise, add a new chunk and fill it with as many adds as possible, then go back to 3, or go to 5 if no adds remain

-- 5. now we have a set of added chunks and a set of removed chunks.  Start again at 2, except at the level of chunks and groups of chunks.  Repeat this process (for groups of groups, groups of groups of groups, etc.) until the new root is calculated and collect everything that's new in reverse dependency order

-- NB: compute the new acks as the intersection of the set generated in 5 and old acks, and the new nacks as the difference between the two (i.e. new set minus new acks)

-- NB: if a nack is received (or an obsolete root), move the specified item from acks to nacks

-- NB: subscriber should send the last complete root it received periodically to ensure we're in sync

-- NB: each writer must segregate values into chunks according to permissions (i.e. all values in a given chunk must share the exact same read and write Access).  However, groups may contain chunks with various Accesses.
