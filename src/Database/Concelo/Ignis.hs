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
  set ignisAuth $ Just $ Auth private (C.derivePublic private) request
  nextRequest

nextRequest = do
  n <- get ignisNextRequest
  update ignisNextRequest (+1)
  return n

sign private challenge = do
  prng <- get ignisPRNG
  let (raw, prng') = C.sign prng private challenge
  set ignisPRNG prng'
  return raw

maybeAuthenticate = do
  auth <- get ignisAuth

  if get' authSent auth then
    Nothing else
    challenge <- get ignisChallenge

    authenticate <$> challenge <*> auth where
      
      authenticate challenge auth =
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
    Right diff ->
      set ignisHead head
      set ignisDiff diff
      return $ Right ()
      
    Left error ->
      return $ Left error

nextMessage = do
  maybeMessage <- maybeAuthenticate

  case maybeMessage of
    Just message ->
      return $ Just message

    Nothing ->
      maybeUpdateSync
      get ignisNacks >>= return . T.first

maybeUpdateSync =
  error "todo: index diff according to ACL, update sync trees, serialize groups, and sort them into acked and nacked messages"

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
