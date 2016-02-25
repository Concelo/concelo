module Database.Concelo.Ignis
  ( ignis ) where

import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Revision as R
import qualified Database.Concelo.Pipe as Pipe

import Database.Concelo.Control (get, set, getThenUpdate, with, bindMaybe,
                                 bindMaybe2)

data Cred = Cred { getCredPrivate :: ByteString
                 , getCredPublic :: ByteString
                 , getCredRequest :: Int
                 , getCredSent :: Bool }

data Ignis = Ignis { getIgnisCred :: Maybe Cred
                   , getIgnisChallenge :: Maybe ByteString
                   , getIgnisNextRequest :: Int
                   , getIgnisHead :: R.Revision
                   , getIgnisDiff :: R.ValueTrie
                   , getIgnisPipe :: Pipe.Pipe
                   , getIgnisPRNG :: C.PRNG }

ignisCred = L.lens getIgnisCred (\x v -> x { getIgnisCred = v })

ignisChallenge = L.lens getIgnisChallenge (\x v -> x { getIgnisChallenge = v })

ignisNextRequest =
  L.lens getIgnisNextRequest (\x v -> x { getIgnisNextRequest = v })

ignisHead = L.lens getIgnisHead (\x v -> x { getIgnisHead = v })

ignisDiff = L.lens getIgnisDiff (\x v -> x { getIgnisDiff = v })

ignisPipe = L.lens getIgnisPipe (\x v -> x { getIgnisPipe = v })

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

  set ignisCred $ Just
    $ Cred private (C.derivePublic private) Nothing request False

  nextRequest

nextRequest = getThenUpdate ignisNextRequest (+1)

sign private challenge = with ignisPRNG $ C.sign private challenge

maybeAuthenticate =
  bindMaybe2 (get ignisCred) (get ignisChallenge) where
    try cred challenge =
      if getCredSent cred then
        return $ Just []
      else do
        set ignisCred $ Just $ L.set credSent True cred

        sign (getCredPrivate cred) challenge
          >>= Just
          [P.Cred P.version (getCredRequest cred) (getCredPublic cred)]

getPublic = bindMaybe getCredPublic $ get ignisCred

unAuth = set ignisCred Nothing

update now update atomicUpdate = do
  head <- update <$> L.set revisionUpdateTime now <$> get ignisHead

  makeDiff head (atomicUpdate head) >>= set ignisDiff

  set ignisHead head

receive = \case
  P.Challenge { P.getChallengeProtocolVersion = v
              , P.getChallengeBody = body } ->
    if v /= P.version then
      error "unexpected protocol version: " ++ show v
    else do
      set ignisChallenge $ Just body

      bindMaybe (set ignisCred . Just . L.set credSent False) (get ignisCred)

      maybeAuthenticate

  nack@(P.Nack {}) ->
    with (pipePublisher . ignisPipe) $ Pub.receive nack

-- tbc
  message ->
    with (pipeSubscriber . ignisPipe) $ Sub.receive message

nextMessages now =
  maybeAuthenticate >>= \case
    Nothing -> return []

    Just [] ->
      with ignisPipe
      $ Pipe.nextMessages now
      ((:[]) . P.Published
       <$> get (publisherPublished . pipePublisher . ignisPipe))

    Just messages -> return messages

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
