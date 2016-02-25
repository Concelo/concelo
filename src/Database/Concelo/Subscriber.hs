module Database.Concelo.Subscriber
  ( receive ) where

import Database.Concelo.Control (patternFailure)

import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as Path

receive = \case
  leaf@(P.Leaf { P.getLeafName = name }) ->
    receiveChunk leaf name T.empty

  group@(P.Group { P.getGroupName = name
                 , P.getGroupMembers = members }) ->
    receiveChunk group name members

  tree@(P.Tree { P.getTreeName = name
               , P.getTreeACL = acl }) ->
    receiveChunk tree name acl

  forest@(P.Forest { P.getForestName = name
                   , P.getForestTrees = trees
                   , P.getForestACL = acl }) ->
    receiveChunk forest name $ T.union trees $ T.union acl T.empty

  p@(P.Persisted forest) -> updateIncomplete forest p

  p@(P.Published forest) -> updateIncomplete forest p

  _ -> patternFailure

updateIncomplete key name message = do
  update subscriberIncomplete
    $ T.union $ Path.super key $ const message <$> name
  checkIncomplete

-- tbc
isComplete hash = null . BM.find hash <$> get subscriberMissing

addMissingToGroups member group missing =
  foldr (addMissingToGroups member) (BM.insert group member missing)
  $ BM.reverseFind group missing

addMissing group members = do
  received <- get subscriberReceived

  let fold member missing = case T.find member received of
        Nothing -> addMissingToGroups member group result

        Just (P.Group { P.getGroupMembers = members }) ->
          foldr fold result members

        Just _ -> result

  update subscriberMissing (\missing -> foldr fold missing members)

updateMissing member =
  update subscriberMissing $ BM.reverseRemove member

findNewChunks oldChunks newChunks newRoot =
  visit newRoot (T.empty, T.empty) where
    visit name (new, newLeaves, found) =
      case M.find name oldChunks of
        Just chunk -> return (new, newLeaves, M.insert name chunk found)

        Nothing -> find name newChunks >>= \chunk ->
          -- we do not allow a given chunk to have more than one
          -- parent since it confuses the diff algorithm
          if M.member name new then
            badForest
          else
            foldM visit (M.insert name chunk new,
                         if chunkIsLeaf chunk then
                           M.insert name chunk newLeaves
                         else
                           newLeaves,
                         found) (chunkMembers chunk)

findObsoleteChunks oldChunks oldRoot found =
  visit oldRoot T.empty where
    visit name result@(obsolete, obsoleteLeaves) =
      if M.member name found then
        result
      else
        find name oldChunks >>= \chunk ->
          foldM visit (M.insert name chunk obsolete,
                       if chunkIsLeaf chunk then
                         M.insert name chunk obsoleteLeaves
                       else
                         obsoleteLeaves) (chunkMembers chunk)

diffChunks oldChunks oldRoot newChunks newRoot = do
  (new, newLeaves, found) <- findNewChunks oldChunks newChunks newRoot
  (obsolete, obsoleteLeaves) <- findObsoleteChunks newChunks oldRoot found
  update subscriberObsolete (T.union obsolete)
  update subscriberNew (T.union new)
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
        P.Leaf { P.getLeafSigned = signed
               , P.getLeafBody = body } -> do

          get subscriberAdministratorACL >>= verify signed

          return case P.deserializeTrie body of
            Just trie -> T.union trie acl
            Nothing -> acl

        _ -> patternFailure

updateUnsanitizedDiff key acl (obsoleteUnsanitized, newUnsanitized)
  (obsoleteLeaves, newLeaves) = do
  obsolete <- foldM visit obsoleteUnsanitized obsoleteLeaves
  new <- foldM visit newUnsanitized newLeaves
  return (obsolete, new) where
    visit needVerify leaf result =
      case leaf of
        P.Leaf { P.getLeafSigned = signed
               , P.getLeafBody = body } -> do

          return case P.deserializeTrie (C.decryptSymmetric key body) of
            Just trie -> unionUnsanitized trie result
            Nothing -> result

        _ -> patternFailure

verifyLeafDiff acl result@(_, newLeaves) = do
  mapM_ visit newLeaves
  return result where
    visit = \case
      P.Leaf { P.getLeafSigned = signed } -> verify signed acl
      _ -> patternFailure

updateTrees currentTrees (obsoleteTrees, newTrees) = do
  subset <-  remove currentTrees obsoleteTrees
  foldM add (subset, (T.empty, T.empty)) newTrees where
    newByACL = M.index treeByACL newTrees

    remove tree trees =
      case tree of
        P.Tree { P.getTreeACL = acl } -> do
          when (not $ M.member acl newByACL) failSubscriber
          return $ M.delete acl trees

        _ -> patternFailure

    add tree (trees, unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized)) =
      case tree of
        P.Tree { P.getTreeRevision = revision
               , P.getTreeSigned = signed
               , P.getTreeName = name
               , P.getTreeACL = acl
               , P.getTreeLeaves = leaves } -> do

          let currentTree = fromJust emptyTree $ M.find acl currentTrees

          when (revision < getTreeRevision currentTree) badForest
          when (revision > getForestRevision currentForest) badForest

          when (M.member acl trees) badForest

          received <- get subscriberReceived

          -- kind of silly to do a diff, since the current trie will
          -- only either be empty or unchanged, but it does the job:
          aclTrie <-
            diffChunks (getTreeChunks currentTree) (getTreeACL currentTree)
            received acl
            >>= updateACL (getTreeACLTrie currentTree)

          adminACL <- get subscriberAdminstratorACL

          when (not (adminACL `T.isSuperSetOf` aclTrie)) badForest

          verify signed aclTrie

          publicKey <- get subscriberPublicKey

          let trees' =
                M.insert acl (L.set treeACLTrie aclTrie currentTree)
                trees

              leafDiff =
                diffChunks (getTreeChunks current) (getTreeLeaves current)
                received leaves
                >>= verifyLeafDiff

          if public == nobody then do
            addMissing name [leaves]
            assertComplete name
            leafDiff

            return (trees', unsanitizedDiff)

            else

            case T.find (T.super ReadKey $ T.key $ getCredPublic cred)
               aclTrie of
              Just encryptedKey -> do
                addMissing name [leaves]
                assertComplete name

                unsanitizedDiff' <-
                  leafDiff >>= updateUnsanitizedDiff
                  (C.decryptAsymmetric (getCredPrivate cred) encryptedKey)
                  aclTrie unsanitizedDiff

                return (trees', unsanitizedDiff')

              Nothing ->
                -- we don't have read access to this tree, so we ignore
                -- the leaves
                return (trees', unsanitizedDiff)

        _ -> patternFailure

updateUnsanitized currentUnsanitized (obsolete, new) =
  foldr unionUnsanitized
  (foldr subtractUnsanitized currentUnsanitized (T.paths obsolete))
  (T.paths new)

visitDirty context acl rules key
  result@(remainingDirty, sanitized, dependencies) =
    if null $ getContextDirty context then
      result
    else
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
  received <- get subscriberReceived
  case T.find name received of
    Just (P.Forest { P.getForestRevision = revision
                   , P.getForestSigned = signed
                   , P.getForestAdminRevision = adminRevision
                   , P.getForestAdminSigned = adminSigned
                   , P.getForestACL = acl
                   , P.getForestTrees = trees }) -> do

      adminACL <- get subscriberAdministratorACL

      verify adminSigned adminACL

      when (revision <= getForestRevision current) badForest
      when (adminRevision <= getForestAdminRevision current) badForest

      aclTrie <-
        diffChunks (getForestChunks current) (getForestACL current)
        received rules
        >>= updateACL (getForestACLTrie current)

      verify signed aclTrie

      -- todo: recompute/purge interned ACLs if ACL has changed

      (treeTrie, unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized)) <-
        diffChunks (getForestChunks current) (getForestTrees current)
        received trees
        >>= updateTrees (getForestTreeTrie current)

      chunks <- updateChunks (getForestChunks current) <$> get subscriberDiff

      let forest = Forest chunks revision adminRevision acl aclTrie trees
                   treeTrie unsanitized

      publicKey <- subscriberPublicKey

      if public == nobody then
        return $ forest T.empty BT.empty T.empty
        else

        let unsanitized =
              updateUnsanitized (getForestUnsanitized current)
              unsanitizedDiff in

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

updateSubscriber (obsoleteChunks, newChunks) subscriber =
  L.set subscriberReceived (updateChunks (getSubscriberReceived subscriber)) subscriber

filterDiff (obsoleteChunks, newChunks) = do
  persisted <- get (forestChunks . subscriberPersisted)
  published <- get (forestChunks . subscriberPublished)
  return (foldr removeLive obsoleteChunks $ M.keys obsoleteChunks, newChunks)
    where removeLive key result =
            if M.member key persisted || M.member key published then
              M.remove key result
            else
              result

checkForest revision name =
  let resetDiff = set subscriberDiff (M.empty, M.empty)
      resetSubscriber = get subscriberClean >>= State.set in

  do assertComplete name

     updateM revision $ updateForest name

     diff <- get subscriberDiff >>= filterDiff

     update subscriberClean $ updateSubscriber diff

     resetSubscriber

  `catchError` \case
    MissingChunks -> resetDiff
    BadForest -> resetSubscriber
    error -> throwError error

checkIncomplete =
  get subscriberIncomplete >>= mapM_ \case
    P.Persisted name -> checkForest subscriberPersisted name
    P.Published name -> checkForest subscriberPublished name
    _ -> patternFailure

receiveChunk chunk name members = do
  update subscriberReceived $ T.insert name chunk
  addMissing name members
  removeMissing name
  checkIncomplete
