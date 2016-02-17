module Database.Concelo.Ignis
  ( ignis ) where

import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Protocol as P

data Cred = Cred { _credPrivate :: ByteString
                 , _credPublic :: ByteString
                 , _credRequest :: Int
                 , _credSent :: Bool }

data Ignis = Ignis { }

data Credentials = PrivateKey { _privateKey :: ByteString }
                 | EmailPassword { _email :: Text
                                 , _password :: Text }

ignis rules = R.parse rules >>= Right . Ignis

authenticate (PrivateKey private) =
  authenticateWithPrivateKey private

authenticate (EmailPassword email password) =
  authenticateWithPrivateKey $ C.derivePrivate password email

authenticateWithPrivateKey private =
  request <- get ignisNextRequest
  set ignisCred $ Just $ Cred private (C.derivePublic private) request False
  nextRequest

nextRequest = do
  getThenUpdate ignisNextRequest (+1)

with lens f =
  (result, new) <- get lens >>= f
  set lens new
  return result

sign private challenge =
  with ignisPRNG $ C.sign private challenge

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
          >>= P.serialize
          . P.Cred (getCredRequest cred) (getCredPublic cred)

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

maybeUpdateForest lens hash sequence = do
  get lens >>= \case
    Nothing ->
      set lens $ Just $ Forest hash sequence

    Just forest@(Forest _ latest) ->
      if sequence > latest then
        set lens $ Just
        $ L.set forestSequence sequence
        $ L.set forestHash hash forest else
        return ()

  checkForests

isComplete hash = fmap (null . BM.find hash) (get ignisMissing)

addMissingToGroups member group missing =
  foldr (addMissingToGroups member) (BM.insert group member missing)
  $ BM.reverseFind group missing

addMissing group members = do
  received <- get ignisReceived

  let fold member missing = case T.find member received of
        Nothing -> addMissingToGroups member group result

        Just (P.Group { P.getGroupMembers = members }) ->
          foldr fold result members

        Just _ -> result

  update ignisMissing (\missing -> foldr fold missing members)

updateMissing member =
  update ignisMissing $ BM.reverseRemove member

findNewChunks oldChunks newChunks newRoot =
  visit newRoot (T.empty, T.empty) where
    visit name (new, newLeaves, found) =
      case M.find name oldChunks of
        Just chunk -> return (new, newLeaves, M.insert name chunk found)

        Nothing -> find name newChunks >>= \chunk ->
          -- we do not allow a given chunk to have more than one
          -- parent since it confuses the diff algorithm
          if M.member name new then
            fail else
            foldM visit (M.insert name chunk new,
                         if chunkIsLeaf chunk then
                           M.insert name chunk newLeaves else
                           newLeaves,
                         found) (chunkMembers chunk)

findObsoleteChunks oldChunks oldRoot found =
  visit oldRoot T.empty where
    visit name result@(obsolete, obsoleteLeaves) =
      if M.member name found then
        result else
        find name oldChunks >>= \chunk ->
          foldM visit (M.insert name chunk obsolete,
                       if chunkIsLeaf chunk then
                         M.insert name chunk obsoleteLeaves else
                         obsoleteLeaves) (chunkMembers chunk)

diffChunks oldChunks oldRoot newChunks newRoot = do
  (new, newLeaves, found) <- findNewChunks oldChunks newChunks newRoot
  (obsolete, obsoleteLeaves) <- findObsoleteChunks newChunks oldRoot found
  update (receiverObsolete . ignisReceiver) (T.union obsolete)
  update (receiverNew . ignisReceiver) (T.union new)
  return (obsoleteLeaves, newLeaves)

updateACL currentACL (obsoleteLeaves, newLeaves) = do
  subset <- foldM remove currentACL obsoleteLeaves
  foldM add subset newLeaves where
    remove leaf acl =
      case leaf of
        P.Leaf { P.getLeafBody = body } ->
          return case P.deserializeTrie body of
            Just trie -> T.subtract trie acl
            Nothing -> acl

        _ -> patternFailure

    add leaf acl =
      case leaf of
        P.Leaf { P.getLeafSignature = signature
               , P.getLeafBody = body } -> do

          get ignisAdministrators >>= verify signature

          return case P.deserializeTrie body of
            Just trie -> T.union trie acl
            Nothing -> acl

        _ -> patternFailure

updateUnsanitizedDiff key acl (obsoleteUnsanitized, newUnsanitized)
  (obsoleteLeaves, newLeaves) = do
  obsolete <- foldM (visit False) obsoleteUnsanitized obsoleteLeaves
  new <- foldM (visit True) newUnsanitized newLeaves
  return (obsolete, new) where
    visit needVerify leaf result =
      case leaf of
        P.Leaf { P.getLeafSignature = signature
               , P.getLeafBody = body } -> do

          when needVerify $ verify signature acl

          return case P.deserializeTrie (C.decryptSymmetric key body) of
            Just trie -> unionUnsanitized trie result
            Nothing -> result

        _ -> patternFailure

updateTrees currentTrees (obsoleteTrees, newTrees) = do
  subset <-  remove currentTrees obsoleteTrees
  foldM add (subset, (T.empty, T.empty)) newTrees where
    newByACL = M.index treeByACL newTrees

    remove tree trees =
      case tree of
        P.Tree { P.getTreeACL = acl } -> do
          when (not $ M.member acl newByACL) failReceiver
          return $ M.delete acl trees

        _ -> patternFailure

    add tree (trees, (obsoleteUnsanitized, newUnsanitized)) =
      case tree of
        P.Tree { P.getTreeRevision = revision
               , P.getTreeSignature = signature
               , P.getTreeName = name
               , P.getTreeACL = acl
               , P.getTreeLeaves = leaves } -> do

          let currentTree = fromJust emptyTree $ M.find acl currentTrees

          when (revision < getTreeRevision currentTree) badForest

          when (M.member acl trees) badForest

          received <- get (receiverReceived . ignisReceiver)

          -- kind of silly to do a diff, since the current trie will
          -- only either be empty or unchanged, but it does the job:
          aclTrie <-
            diffChunks (getTreeChunks currentTree) (getTreeACL currentTree)
            received acl
            >>= updateACL (getTreeACLTrie currentTree)

          adminACL <- get ignisAdminstratorACL

          when (not (adminACL `T.isSuperSetOf` aclTrie)) badForest

          verify signature aclTrie

          cred <- get ignisCred >>= whoAmI

          let trees' =
                M.insert acl (L.set treeACLTrie aclTrie currentTree)
                trees

          case T.find (T.super WriteKey $ T.key $ getCredPublic cred)
               aclTrie of
            Just encryptedKey -> do
              addMissing name [leaves]
              assertComplete name

              unsanitizedDiff' <-
                diffChunks (getTreeChunks current) (getTreeLeaves current)
                received leaves
                >>= updateUnsanitizedDiff
                (C.decryptAsymmetric (getCredPrivate cred) encryptedKey)
                aclTrie unsanitizedDiff

              return (trees', unsanitizedDiff')

            Nothing ->
              -- we don't have read access to this tree, so we ignore
              -- the leaves
              return (trees', unsanitized)

        _ -> patternFailure

updateUnsanitized currentUnsanitized (obsolete, new) =
  foldr unionUnsanitized
  (foldr subtractUnsanitized currentUnsanitized (T.paths obsolete))
  (T.paths new)

visitDirty context acl rules key
  result@(remainingDirty, sanitized, dependencies) =
    if null $ getContextDirty context then
      result else
      visitValues result possibleValues where
        rules' = T.sub key rules
        dirty' = T.sub key $ getContextDirty context

        context' =
          L.over contextHead (T.sub key)
          $ L.set contextDirty remainingDirty context

        possibleValues =
          fromMaybe M.empty (L.get elementMap <$> T.value dirty')

        visitValues result@(remainingDirty, sanitized, dependencies)
          firstValid values =
          let value = maybeHead values

              context'' = L.set contextValue value context'

              (readACL, readDependencies) =
                fromMaybe (Right acl, T.empty)
                ((\r -> (getRulesRead r) context'' acl) <$> T.value rules')

              acl' = either (const acl) id readACL

              (writeACL, writeDependencies) =
                fromMaybe (Right acl', T.empty)
                ((\r -> (getRulesWrite r) context'' acl') <$> T.value rules')

              readWriteDependencies =
                T.union readDependencies writeDependencies

              (validateResult, validateDependencies) =
                fromMaybe (Right (), T.empty)
                (value >>
                 ((\r -> (getRulesValidate r) context'') <$> T.value rules'))

              dependencies' =
                BT.insertTrie path readWriteDependencies
                $ BT.insertTrie path validateDependencies
                dependencies

              remainingDirty' = T.subtract path remainingDirty

              next value = case maybeTail values of
                Just tail ->
                  visitValues (remainingDirty, sanitized, dependencies')
                  (firstValid <|> value) tail

                Nothing ->
                  (remainingDirty',
                   case firstValid of
                     Nothing -> T.subtract path sanitized
                     Just v -> T.union (fmap (const v) path) sanitized,
                   dependencies') in

          if null $ T.intersect readWriteDependencies remainingDirty'
          then
            case readACL >> writeACL of
              Left _ -> next Nothing
              Right acl'' ->
                if maybe True (\v -> aclWriter (valueSigner v) acl'') value
                then
                  if null $ T.intersect validateDependencies remainingDirty'
                  then
                    case validateResult of
                      Left _ -> next Nothing
                      Right _ -> next value
                  else
                    foldr (visitDirty acl'' context' rules' dirty')
                    result $ T.keys dirty'
                else
                  next Nothing
          else
            foldr (visitDirty context' acl rules' dirty') result
            $ T.keys dirty'

updateSanitized revision acl currentSanitized updatedUnsanitized
  updatedRules currentDependencies (obsoleteUnsanitized, newUnsanitized) =

    clean (Context nobody revision T.empty currentSanitized dirty)
    (dirty, currentSanitized, remainingDependencies) where

      dirty =
        foldr findDirty T.empty
        $ T.paths $ T.union obsoleteUnsanitized newUnsanitized

      remainingDependencies = BT.subtract dirty currentDependencies

      findDirty path (dirty, dependencies) =
        foldr findDirty
        (T.union
         (fromMaybe (Element T.empty) (T.find path updatedUnsanitized)
          <$> path)
         dirty)
        (T.paths $ fromMaybe T.empty $ BT.reverseFind path dependencies)

      clean context result@(dirty, sanitized, dependencies)
        | null dirty = (sanitized, dependencies)
        | otherwise =
          clean context
          (foldr (visitDirty context acl updatedRules) result $ T.keys dirty)

rulesKey = ".rules"

updateForest current name = do
  received <- get (receiverReceived . ignisReceiver)
  case T.find name received of
    Just (P.Forest { P.getForestRevision = revision
                   , P.getForestSignature = signature
                   , P.getForestAdminRevision = adminRevision
                   , P.getForestAdminSignature = adminSignature
                   , P.getForestACL = acl
                   , P.getForestTrees = trees }) -> do

      adminACL <- get ignisAdministratorACL

      verify adminSignature adminACL

      when (revision <= getForestRevision current) badForest
      when (adminRevision <= getForestAdminRevision current) badForest

      aclTrie <-
        diffChunks (getForestChunks current) (getForestACL current)
        received rules
        >>= updateACL (getForestACLTrie current)

      verify signature aclTrie

      -- todo: recompute/purge interned ACLs if ACL has changed

      (treeTrie, unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized)) <-
        diffChunks (getForestChunks current) (getForestTrees current)
        received trees
        >>= updateTrees (getForestTreeTrie current)

      chunks <-
        fmap (updateChunks $ getForestChunks current)
        (get (receiverDiff . ignisReceiver))

      let unsanitized =
            updateUnsanitized (getForestUnsanitized current)
            unsanitizedDiff

          forest = Forest chunks revision adminRevision acl aclTrie trees
                   treeTrie unsanitized

      if T.member rulesKey obsoleteUnsanitized
         || T.member rulesKey newUnsanitized then do

        let (sanitizedRules, _) =
              updateSanitized
              0
              adminACL
              (T.sub rulesKey (getForestSanitized current))
              (T.sub rulesKey unsanitized)
              T.empty
              BT.empty
              ((T.sub rulesKey obsoleteUnsanitized),
               (T.sub rulesKey newUnsanitized))

        rules <- compileRules sanitizedRules

        let (sanitized, dependencies) =
              updateSanitized
              revision
              emptyACL
              T.empty
              unsanitized
              rules
              BT.empty
              (T.empty, unsanitized)

        return $ forest rules dependencies sanitized

        else
        let rules = getForestRules current

            (sanitized, dependencies) =
              updateSanitized
              revision
              emptyACL
              (getForestSanitized current)
              unsanitized
              rules
              (getForestDependencies current)
              unsanitizedDiff in

        return $ forest rules dependencies sanitized

    _ -> patternFailure

updateChunks chunks (obsoleteChunks, newChunks) =
  foldr T.union (foldr T.subtract chunks (T.paths obsoleteChunks))
  (T.paths newChunks)

updateReceiver (obsoleteChunks, newChunks) receiver =
  L.set receiverReceived (updateChunks (getReceiverReceived receiver)) receiver

filterDiff (obsoleteChunks, newChunks) = do
  persisted <- get (forestChunks . ignisPersisted)
  published <- get (forestChunks . ignisPublished)
  return (foldr removeLive obsoleteChunks $ M.keys obsoleteChunks, newChunks)
    where removeLive key result =
            if M.member key persisted || M.member key published then
              M.remove key result else
              result

checkForest revision name =
  let resetDiff = set (receiverDiff . ignisReceiver) (M.empty, M.empty)
      resetReceiver = get ignisCleanReceiver >>= set ignisReceiver in

  do assertComplete name

     updateM revision $ updateForest name

     diff <- get (receiverDiff . ignisReceiver) >>= filterDiff

     update ignisCleanReceiver $ updateReceiver diff

     resetReceiver

  `catchError` \case
    MissingChunks -> resetDiff
    BadForest -> resetReceiver
    error -> throwError error

checkIncomplete =
  get ignisIncomplete >>= mapM_ \case
    P.Persisted name -> checkForest ignisPersisted name
    P.Published name -> checkForest ignisPublished name
    _ -> patternFailure

receiveChunk chunk name members = do
  update ignisReceived $ T.insert name chunk
  addMissing name members
  removeMissing name
  checkIncomplete

receive = \case
  P.Challenge challenge -> do
    set ignisChallenge $ Just challenge
    get ignisCred >>= \case
      Nothing -> return ()
      Just cred -> set ignisCred $ Just $ L.set credSent False cred

  leaf@(P.Leaf { P.getLeafName = name }) ->
    receiveChunk leaf name []

  group@(P.Group { P.getGroupName = name
                 , P.getGroupMembers = members }) ->
    receiveChunk group name members

  tree@(P.Tree { P.getTreeName = name
               , P.getTreeACL = acl }) ->
    receiveChunk tree name [acl]

  forest@(P.Forest { P.getForestName = name
                   , P.getForestTrees = trees
                   , P.getForestRules = rules
                   , P.getForestACL = acl }) ->
    receiveChunk forest name [trees, rules, acl]

  p@(P.Persisted forest) -> do
    update ignisIncomplete $ T.insert (T.super PersistedKey $ T.value forest p)
    checkIncomplete

  p@(P.Published forest) -> do
    update ignisIncomplete $ T.insert (T.super PublishedKey $ T.value forest p)
    checkIncomplete

  P.Nack path -> do
    fmap (T.findPath path) (get ignisAcks) >>= \case
      Just nack -> do
        update ignisNacks $ T.union nack
        update ignisAcks $ T.subtract nack
        return ()

      Nothing -> return ()

nextMessage =
  get ignisCred >>= \case
    Nothing -> return Nothing
    Just _ ->
      maybeAuthenticate >>= \case
        Just message ->
          return $ Just message

        Nothing -> do
          maybeUpdateTrees

          fmap T.firstPath (get ignisNacks) >>= \case
            Just nack -> do
              update ignisNacks $ T.subtract nack
              update ignisAcks $ T.union nack
              return $ T.first nack

            Nothing -> return Nothing
            -- todo: request missing
            -- todo: send current root periodically

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

updateAcksAndNacks (allObsolete, allNew) = do
  acks <- updateThenGet ignisAcks $ T.intersect allNew . T.subtract allObsolete
  update ignisNacks T.union (T.subract acks allNew) . T.subtract allObsolete

maybeUpdateTrees =
  get ignisDiff >>= \case
    Nothing -> return ()
    Just diff ->
      updateACLTrees $ mapPair (T.indexPaths byACL) diff
      >>= updateMyTree
      >>= updateRootTree
      >>= updateAcksAndNacks

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
