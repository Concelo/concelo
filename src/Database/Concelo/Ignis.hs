module Database.Concelo.Ignis
  ( ignis ) where

import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Protocol as P

import Database.Concelo.Lens (get, set, getThenUpdate, with)

data Cred = Cred { getCredPrivate :: ByteString
                 , getCredPublic :: ByteString
                 , getCredRequest :: Int
                 , getCredSent :: Bool }

data Ignis = Ignis { getIgnisCred :: Maybe Cred
                   , getIgnisNextRequest :: Int
                   , getIgnisPRNG :: C.PRNG }

ignisCred = L.lens getIgnisCred (\x v -> x { getIgnisCred = v })
ignisNextRequest =
  L.lens getIgnisNextRequest (\x v -> x { getIgnisNextRequest = v })
ignisPRNG = L.lens getIgnisPRNG (\x v -> x { getIgnisPRNG = v })

data Credentials = PrivateKey { getPKPrivateKey :: ByteString
                              , getPKPassword :: ByteString }
                 | EmailPassword { getEPEmail :: ByteString
                                 , getEPPassword :: ByteString }

ignis seed = Ignis Nothing 1 (C.makePRNG seed)

authenticate = \case
  PrivateKey key password ->
    authenticateWithPrivateKey $ C.decryptPrivate password key

  EmailPassword email password ->
    authenticateWithPrivateKey $ C.deriveKey password email

authenticateWithPrivateKey private = do
  request <- get ignisNextRequest
  set ignisCred $ Just $ Cred private (C.derivePublic private) request False
  nextRequest

nextRequest = getThenUpdate ignisNextRequest (+1)

sign private challenge = with ignisPRNG $ C.sign private challenge

-- tbc

bind f x =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> f x'

bind2 f x y =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> y >>= \case
      Nothing -> return Nothing
      Just y' -> f x' y'

maybeAuthenticate =
  bind2 try (get ignisCred) (get ignisChallenge) where

    try cred challenge =
      if getCredSent cred then return Nothing else do
        set ignisCred $ Just $ L.set credSent True cred

        sign (getCredPrivate cred) challenge
          >>= P.Cred (getCredRequest cred) (getCredPublic cred)

getPublic =
  get ignisCred >>= \case
    Nothing -> return Nothing
    Just cred -> return $ getCredPublic cred

unAuth = set ignisCred Nothing

update now update atomicUpdate = do
  head <- fmap update $ get ignisHead
  let atomicHead = atomicUpdate head
  makeDiff head atomicHead >>= set ignisDiff
  set ignisUpdateTime now
  set ignisHead head

receive = \case
  P.Challenge challenge -> do
    set ignisChallenge $ Just challenge
    get ignisCred >>= \case
      Nothing -> return ()
      Just cred -> set ignisCred $ Just $ L.set credSent False cred

  nack@(P.Nack {}) ->
    with ignisPublisher $ Pub.receive nack

  message ->
    with ignisSubscriber $ Sub.receive message

ping = (:[] . P.Published) <$> get ignisPublished

nextMessages now =
  get ignisCred >>= \case
    Nothing -> return []
    Just _ ->
      maybeAuthenticate >>= \case
        Just message ->
          return [message]

        Nothing ->
          PubSub.nextMessages now ignisPublisher ignisSubscriber ignisPubSent
          ignisLastPing ping

mapPair f (x, y) = (f x, f y)

byACL path value = T.super (getValueACL value) path

withSerializer constructor f =
  ignisCred >>= \case
    Nothing -> error "cannot encrypt without private key"
    Just cred ->
      with ignisPRNG (f . constructor (getCredPrivate cred))

updateACLTrees (obsoleteValues, newValues) = do
  with ignisACLTrees \trees ->
    withSerializer ValueSerializer \serializer ->
      foldr update (((T.empty, T.empty, T.empty, T.empty), trees), serializer)
            $ T.union (T.keys obsoleteValues) (T.keys newValues) where

    update acl (((allObsolete, allNew, obsoleteTrees, newTrees),
                 trees), serializer) =

      (((T.union obsolete' allObsolete,
         T.union new' allNew,

         if ST.null (getAnnotatedValue tree) then
           obsoleteTrees else
           T.insert acl tree obsoleteTrees,

         if ST.null tree' then
           newTrees else
           T.insert acl (Annotated tree' acl) newTrees),

        if ST.null tree' then
          T.delete acl trees else
          T.insert acl tree' trees),

       serializer') where

        tree = fromMaybe ST.empty $ T.find (T.key acl) trees

        (tree', serializer', obsolete', new') =
          ST.update (annotatedValue tree) serializer
          (T.sub acl obsoleteValues) (T.sub acl newValues)

whoAmI = fromMaybe $ error "I don't know who I am!"

updateMyTree (allObsolete, allNew, obsoleteTrees, newTrees) = do
  cred <- get ignisCred >>= whoAmI
  challenge <- get ignisChallenge >>= whoAmI
  revision <- getThenUpdate ignisNextRevision (+1)
  time <- get ignisUpdateTime
   -- all trees from other writers using the same keypair we're using
   -- except for the most recent published revision, if applicable:
  obsoleteTrees <- get ignisPeerTrees

  with ignisMyTree \tree ->
    -- NB: the acl serializer will enumerate the key encrypted for all authorized readers
    withSerializer ACLTreeSerializer \serializer ->
      let (tree', serializer', obsolete', new') =
            ST.update (annotatedValue tree) serializer obsoleteTrees
            newTrees
          annotated = (Annotate tree'
                       (Stamp
                        (getIgnisPublic cred) challenge revision time)) in
      (((T.union obsolete' allObsolete,
         T.union new' allNew,

         if ST.isEmpty (getAnnotatedValue tree) then
           obsoleteTrees else
           T.insert key tree obsoleteTrees,

         if ST.isEmpty tree' then
           T.empty else
           T.singleton revision annotated),

        annotated),
       serializer')

updateRootTree (allObsolete, allNew, obsoleteTrees, newTrees) = do
  with ignisRootTree \tree ->
    withSerializer WriterTreeSerializer \serializer ->
      let (tree', serializer', obsolete', new') =
            ST.update tree serializer obsoleteTrees newTrees in
      (((T.union obsolete' allObsolete,
         T.union new' allNew),
        tree'),
       serializer')

maybeUpdateTrees =
  get ignisDiff >>= \case
    Nothing -> return ()
    Just diff ->
      updateACLTrees $ mapPair (T.indexPaths byACL) diff
      >>= updateMyTree
      >>= updateRootTree
      >>= \diff -> with ignisPublisher $ Pub.update diff

data Context = Context { getContextMe :: PublicKey
                       , getContextRevision :: Integer
                       , getContextRoot :: Trie Key (Value a)
                       , getContextEnv :: Trie Key Key
                       , getContextBase :: Trie Key (Value a)
                       , getContextHead :: Trie Key (Value a) }

updateACL context acl acls rules =
  if (getRulesValid rules) context then
    Right $ intern
    (L.over aclWrite ((getRulesWrite rules) context)
     $ L.over aclRead ((getRulesRead rules) context) acl) acls else
    Left "validation failed"

validate context rules diff path acl
  result@(tries@(obsoleteTrie, newTrie), acls) =
    (if T.empty diff then
       result else
       case fromMaybe (Right (acl, acls))
            (T.value rules >>= Just $ updateACL context acl acls) of
         Right (acl', acls') ->
           case T.value diff of
             Just (obsoleteValue, newValue) ->
               if aclWriter (getContextMe context) acl' then
                 Right $ foldr fold
                 ((maybe obsoleteTrie \v ->
                     T.union (singleton path v) obsoleteTrie,

                   maybe newTrie \v ->
                     T.union (singleton path $ L.set valueACL acl' v) newTrie),

                  acls')
                 $ T.keys diff else
                 Left "write access denied"

             Nothing -> Right foldr fold (tries, acls') $ T.keys diff

         Left error -> Left error) where

      fold key result =
        let rules' = T.sub key rules
            context' = L.over contextBase (T.sub key)
                       $ L.over contextHead (T.sub key)
            diff' = T.sub key diff
            path' = key : path
            result' = validate context' rules' diff' path' acl result in

        if T.null rules' then
          case T.value rules >>= getRulesWildCard of
            Just (name, rules') ->
              validate (L.over contextEnv (T.insert name key) context') rules'
              diff' path' acl result

            Nothing ->
              result' else
          result'

makeDiff head = do
  me <- getPublic >>= whoAmI
  base <- get ignisBase
  rules <- get ignisRules
  revision <- get ignisNextRevision
  emptyACL <- get ignisEmptyACL

  with ignisACLs \acls ->
    validate (Context me revision base T.empty base head) rules
    (T.diff base head) [] emptyACL ((T.empty, T.empty), acls)

validUpdate obsoletePath newPath = do
  me <- getPublic >>= whoAmI
  base <- get ignisBase
  valid <- get ignisValid
  if valid obsoletePath newPath base me then do
    readWrite <- get ignisReadWrite
    trees <- get ignisACLTrees
    let acl = readWrite obsoletePath newPath base trees
    if writer acl me then
      return $ Right acl else
      return $ Left "write access denied" else
    return $ Left "validation failed"

makeDiff head atomicHead = do
  base <- get ignisBase
  foldM apply (Right (T.empty, T.empty)) $ T.diff base atomicHead where

    apply (obsoletePath, newPath) diff = do
      acl <- validateUpdate obsoletePath newPath
      return (apply' <$> diff <*> acl) where

        apply' (obsoletePaths, newPaths, newACLs) acl =
          (T.union obsoletePath obsoletePaths,
           T.union (fmap (L.set valueACL acl) newPath) newPaths)
