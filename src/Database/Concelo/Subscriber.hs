module Database.Concelo.Subscriber
  ( receive
  , subscriberPublished
  , forestRevision ) where

-- todo: split this code into two files: one that handles incoming messages and validating forests (Subscriber.hs), and another that aggregates trees into a trie and sanitizes it (Deserializer.hs)

import Database.Concelo.Control (patternFailure, badForest, missingChunks,
                                 maybeM2, set, get, update, updateM)

import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Set as S
import qualified Database.Concelo.BiTrie as BT
import qualified Database.Concelo.Path as Path
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Rules as R
import qualified Database.Concelo.ACL as ACL
import qualified Control.Lens as L
import qualified Control.Monad.State.Class as State
import qualified Data.ByteString as BS

data KeyPair =
  KeyPair { getKeyPairPublic :: BS.ByteString
          , getKeyPairPrivate :: BS.ByteString }

data Incomplete =
  Incomplete { getIncompletePersisted :: T.Trie BS.ByteString ()
             , getIncompletePublished :: T.Trie BS.ByteString () }

incompletePersisted =
  L.lens getIncompletePersisted (\x v -> x { getIncompletePersisted = v })

incompletePublished =
  L.lens getIncompletePublished (\x v -> x { getIncompletePublished = v })

data Subscriber =
  Subscriber { getSubscriberIncomplete :: Incomplete
             , getSubscriberReceived :: T.Trie BS.ByteString P.Message
             , getSubscriberMissing :: BT.BiTrie BS.ByteString
             , getSubscriberObsolete :: T.Trie BS.ByteString P.Message
             , getSubscriberNew :: T.Trie BS.ByteString P.Message
             , getSubscriberAdminACL :: T.Trie BS.ByteString ()
             , getSubscriberPermitAdmins :: ACL.ACL
             , getSubscriberKeyPair :: KeyPair
             , getSubscriberStream :: P.Name
             , getSubscriberPersisted :: Forest
             , getSubscriberPublished :: Forest
             , getSubscriberClean :: Subscriber
             , getSubscriberDiff :: (T.Trie BS.ByteString P.Message,
                                     T.Trie BS.ByteString P.Message) }

subscriberIncomplete =
  L.lens getSubscriberIncomplete (\x v -> x { getSubscriberIncomplete = v })

subscriberReceived =
  L.lens getSubscriberReceived (\x v -> x { getSubscriberReceived = v })

subscriberMissing =
  L.lens getSubscriberMissing (\x v -> x { getSubscriberMissing = v })

subscriberObsolete =
  L.lens getSubscriberObsolete (\x v -> x { getSubscriberObsolete = v })

subscriberNew =
  L.lens getSubscriberNew (\x v -> x { getSubscriberNew = v })

subscriberAdminACL =
  L.lens getSubscriberAdminACL (\x v -> x { getSubscriberAdminACL = v })

subscriberPermitAdmins =
  L.lens getSubscriberPermitAdmins
  (\x v -> x { getSubscriberPermitAdmins = v })

subscriberPublicKey =
  L.lens getSubscriberPublicKey (\x v -> x { getSubscriberPublicKey = v })

subscriberStream =
  L.lens getSubscriberStream (\x v -> x { getSubscriberStream = v })

subscriberPersisted =
  L.lens getSubscriberPersisted (\x v -> x { getSubscriberPersisted = v }

subscriberPublished =
  L.lens getSubscriberPublished (\x v -> x { getSubscriberPublished = v }

subscriberClean =
  L.lens getSubscriberClean (\x v -> x { getSubscriberClean = v }

subscriberDiff =
  L.lens getSubscriberDiff (\x v -> x { getSubscriberDiff = v })

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

  P.Persisted forest -> updateIncomplete incompletePersisted forest

  P.Published forest -> updateIncomplete incompletePublished forest

  _ -> patternFailure

updateIncomplete key name = do
  update (key . subscriberIncomplete) $ T.union name
  checkIncomplete

addMissingToGroups member group missing =
  foldr (addMissingToGroups member) (BT.insert group member missing)
  $ T.paths $ BT.reverseFind group missing

addMissing group members = do
  received <- get subscriberReceived

  let visit member missing = case T.findValue member received of
        Nothing -> addMissingToGroups member group result

        Just (P.Group { P.getGroupMembers = members }) ->
          foldr visit result $ T.paths members

        Just _ -> result

  update subscriberMissing $ \missing -> foldr visit missing $ T.paths members

updateMissing member =
  update subscriberMissing $ BT.reverseDelete member

chunkIsLeaf = \case
  P.Leaf {} -> True
  _ -> False

chunkMembers = \case
  P.Group { P.getGroupMembers = m } -> m
  _ -> T.empty

findNewChunks oldChunks newChunks newRoot =
  visit newRoot (T.empty, T.empty, T.empty) where
    visit name (new, newLeaves, found) =
      case T.find name oldChunks of
        Just chunk ->
          return (new, newLeaves, T.union (const chunk <$> name) found)

        Nothing -> maybeM2 T.findValue name newChunks >>= \chunk ->
          -- we do not allow a given chunk to have more than one
          -- parent since it confuses the diff algorithm
          if T.member name new then
            badForest
          else
            foldM visit (T.union (const chunk <$> name) new,
                         if chunkIsLeaf chunk then
                           T.union (const chunk <$> name) newLeaves
                         else
                           newLeaves,
                         found) (chunkMembers chunk)

findObsoleteChunks oldChunks oldRoot found =
  visit oldRoot (T.empty, T.empty) where
    visit name result@(obsolete, obsoleteLeaves) =
      if T.member name found then
        result
      else
        maybeM2 T.findValue name oldChunks >>= \chunk ->
          foldM visit (T.union (const chunk <$> name) obsolete,
                       if chunkIsLeaf chunk then
                         T.union (const chunk <$> name) obsoleteLeaves
                       else
                         obsoleteLeaves) (chunkMembers chunk)

diffChunks oldChunks oldRoot newChunks newRoot = do
  (new, newLeaves, found) <- findNewChunks oldChunks newChunks newRoot

  (obsolete, obsoleteLeaves) <- findObsoleteChunks newChunks oldRoot found

  update subscriberDiff $ \(o, n) -> (T.union obsolete o, T.union new n)

  return (obsoleteLeaves, newLeaves)

verify (P.Signed { getSignedSigner = signer
                 , getSignedSignature = signature
                 , getSignedText = text }) acl =
  if T.member (Path.super P.aclWriterKey $ Path.singleton signer ()) acl then
    if C.verify signer signature text then
      return ()
    else
      badForest
  else
    badForest

updateACL currentACL (obsoleteLeaves, newLeaves) = do
  subset <- foldM remove currentACL obsoleteLeaves
  foldM add subset newLeaves where
    remove leaf acl =
      case leaf of
        P.Leaf { P.getLeafBody = body } ->
          case P.parseTrie body of
            Just trie -> return $ T.subtract trie acl
            Nothing -> patternFailure

        _ -> patternFailure

    add leaf acl =
      case leaf of
        P.Leaf { P.getLeafSigned = signed
               , P.getLeafBody = body } -> do

          get subscriberAdminACL >>= verify signed

          case P.parseTrie body of
            -- todo: handle defragmentation (or assert that no valid forest will contain a fragmented ACL)
            Just trie -> return $ T.union trie acl
            Nothing -> badForest

        _ -> patternFailure

newtype UnsanitizedElement =
  UnsanitizedElement
  { getUnsanitizedElementMap :: M.Map BS.ByteString P.Value }

unionUnsanitized signer small large =
  foldr visit large $ T.pathsAndValues small where

    visit (path, new) result =
      case T.findValue path large of
        Nothing ->
          T.union path result
        Just element ->
          T.union ((const $ update element new) <$> path) result

    update element@(UnsanitizedElement map) new =
      case P.parseValue signer new of
        Nothing -> element
        Just v -> UnsanitizedElement $ M.insert new v map

subtractUnsanitized small large =
  foldr visit large $ T.pathsAndValues small where

    visit (path, obsolete) result =
      case T.findValue path large of
        Nothing -> result
        Just (UnsanitizedElement map) ->
          let map' = M.delete obsolete map in
          if null map' then
            T.subtract path result
          else
            T.union ((const $ UnsanitizedElement map') <$> path) result

updateUnsanitizedDiff key acl (obsoleteUnsanitized, newUnsanitized)
  (obsoleteLeaves, newLeaves) = do

  obsolete <- foldM (visit false) obsoleteUnsanitized obsoleteLeaves

  new <- foldM (visit true) newUnsanitized newLeaves

  return (obsolete, new) where

    visit needVerify leaf result =
      case leaf of
        P.Leaf { P.getLeafSigned = signed
               , P.getLeafBody = body } -> do

          when needVerify $ verify signed acl

          return case P.parseTrie (C.decryptSymmetric key body) of
            -- todo: handle defragmentation
            Just trie -> unionUnsanitized (getSignedSigner signed) trie result
            Nothing -> result

        _ -> patternFailure

verifyLeafDiff acl result@(_, newLeaves) = do
  mapM_ visit newLeaves
  return result where
    visit = \case
      P.Leaf { P.getLeafSigned = signed } -> verify signed acl
      _ -> patternFailure

data Tree = Tree { getTreeRevision :: Integer
                 , getTreeStream :: BS.ByteString
                 , getTreeACL :: P.Name
                 , getTreeACLTrie :: T.Trie BS.ByteString BS.ByteString }

treeRevision =
  L.lens getTreeRevision (\x v -> x { getTreeRevision = v })

treeStream =
  L.lens getTreeStream (\x v -> x { getTreeStream = v })

treeChunks =
  L.lens getTreeChunks (\x v -> x { getTreeChunks = v })

treeACL =
  L.lens getTreeACL (\x v -> x { getTreeACL = v })

treeACLTrie =
  L.lens getTreeACLTrie (\x v -> x { getTreeACLTrie = v })

emptyTree stream = Tree (-1) stream Path.empty T.empty

updateTrees forestACLTrie currentForest (obsoleteTrees, newTrees) =
  let currentTrees = getForestTreeTrie currentForest in do
    subset <- remove currentTrees obsoleteTrees

    foldM add (subset, (T.empty, T.empty)) newTrees where

      newByStream = M.index getTreeStream newTrees

      remove tree trees =
        case tree of
          P.Tree { P.getTreeStream = stream } -> do
            when (not $ M.member stream newByStream) badForest
            return $ M.delete stream trees

          _ -> patternFailure

      add tree (trees, unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized)) =
        case tree of
          P.Tree { P.getTreeRevision = revision
                 , P.getTreeSigned = signed
                 , P.getTreeName = name
                 , P.getTreeACL = acl
                 , P.getTreeStream = stream
                 , P.getTreeForestStream = forestStream
                 , P.getTreeLeaves = leaves } -> do

            let old = M.lookup stream currentTrees
                oldACL = maybe acl getTreeACL old
                currentTree = fromJust (emptyTree stream) old

            when (revision < getTreeRevision currentTree
                  || M.member stream trees
                  || acl /= oldACL
                  || forestStream /= getForestStream currentForest)
              badForest

            received <- get subscriberReceived

            -- kind of silly to do a diff, since the current trie will
            -- only either be empty or unchanged, but it does the job:
            aclTrie <-
              diffChunks (getForestChunks currentForest)
              (getTreeACL currentTree) received acl
              >>= updateACL (getTreeACLTrie currentTree)

            when (not (forestACLTrie `T.hasAll` aclTrie)) badForest

            verify signed aclTrie

            keyPair <- get subscriberKeyPair

            let trees' =
                  M.insert stream (Tree revision stream acl aclTrie)
                  trees

                leafDiff =
                  diffChunks (getForestChunks currentForest)
                  (getTreeLeaves current) received leaves
                  >>= verifyLeafDiff

            if null (getKeyPairPublic keyPair) then do
              addMissing name leaves
              assertComplete name
              leafDiff

              return (trees', unsanitizedDiff)

              else

              case T.findValue (Path.super P.aclReaderKey
                                $ Path.singleton (getKeyPairPublic keyPair) ())
                   aclTrie of
                -- todo: handle optional trees
                Just encryptedKey -> do
                  addMissing name leaves
                  assertComplete name

                  unsanitizedDiff' <-
                    leafDiff >>= updateUnsanitizedDiff
                    (C.decryptAsymmetric (getKeyPairPrivate keyPair)
                     encryptedKey)
                    aclTrie unsanitizedDiff

                  return (trees', unsanitizedDiff')

                Nothing ->
                  -- we don't have read access to this tree, so we ignore
                  -- the leaves
                  return (trees', unsanitizedDiff)

          _ -> patternFailure

updateUnsanitized currentUnsanitized (obsolete, new) =
  unionUnsanitized new (subtractUnsanitized obsolete currentUnsanitized)

visitDirty context acl rules key
  result@(remainingDirty, sanitized, dependencies) =
    if null remainingDirty then
      result
    else
      visitValues result Nothing possibleValues where
        (rules', wildcard) = R.subRules key rules

        dirty' = T.sub key $ L.view R.contextDirty context

        context' =
          L.over R.contextEnv (if null wildcard then
                                 id
                               else
                                 M.set wildcard key)

          $ L.over R.contextVisitor (R.visitorChild key)
          $ L.set R.contextDirty remainingDirty context

        possibleValues =
          fromMaybe M.empty (getUnsanitizedElementMap <$> T.value dirty')

        visitValues result@(remainingDirty, sanitized, dependencies)
          firstValid values =
          let value = maybeHead values

              context'' = L.set R.contextValue value context'

              (readACL, readDependencies) =
                L.view R.rulesRead rules' context'' acl

              (writeACL, writeDependencies) =
                L.view R.rulesWrite rules' context'' readACL

              acl' = writeACL

              readWriteDependencies =
                T.union readDependencies writeDependencies

              (validateResult, validateDependencies) =
                fromMaybe (True, T.empty)
                (const (L.view R.rulesValidate rules' context'') <$> v)

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
                  foldr (visitDirty context' acl' rules' dirty')
                  (remainingDirty',
                   case firstValid of
                     Nothing -> T.subtract path sanitized
                     Just v -> T.union (const v <$> path) sanitized,
                   dependencies')
                  $ T.keys dirty'
          in

          if remainingDirty `T.hasAny` readWriteDependencies then
            foldr (visitDirty context' acl rules' dirty') result
            $ T.keys dirty'
          else
            if maybe True (\v -> valueSigner v `ACL.isWriter` writeACL) value
            then
              if remainingDirty' `T.hasAny` validateDependencies then
                foldr (visitDirty context' acl' rules' dirty') result
                $ T.keys dirty'
              else
                next if validateResult then value else Nothing
            else
              next Nothing

updateSanitized revision acl currentSanitized updatedUnsanitized
  updatedRules currentDependencies (obsoleteUnsanitized, newUnsanitized) =

    clean
    (Context dirty revision T.empty (R.rootVisitor currentSanitized) T.empty)
    (dirty, currentSanitized, remainingDependencies) where

      unsanitized = T.union obsoleteUnsanitized newUnsanitized

      dirty = foldr findDirty unsanitized $ T.paths unsanitized

      remainingDependencies = BT.subtract dirty currentDependencies

      findDirty path result =
        foldr findDirty
        (T.union
         (fromMaybe (UnsanitizedElement T.empty)
          (T.findValue path updatedUnsanitized) <$> path)
         result)
        (T.paths $ BT.reverseFind path currentDependencies)

      clean context result@(dirty, sanitized, dependencies)
        | null dirty = (sanitized, dependencies)
        | otherwise =
          clean context
          (foldr (visitDirty context acl updatedRules) result $ T.keys dirty)

data Forest =
  Forest { getForestRevision :: Integer
         , getForestAdminRevision :: Integer
         , getForestChunks :: T.Trie BS.ByteString P.Message
         , getForestACL :: P.Name
         , getForestACLTrie :: T.Trie BS.ByteString BS.ByteString
         , getForestPermitNone :: ACL.ACL
         , getForestTrees :: P.Name
         , getForestTreeTrie :: T.Trie BS.ByteString BS.ByteString
         , getForestUnsanitized :: T.Trie BS.ByteString UnsanitizedElement
         , getForestRules :: R.Rules
         , getForestDependencies :: BT.BiTrie BS.ByteString
         , getForestSanitized :: T.Trie BS.ByteString P.Value }

forestRevision =
  L.lens getForestRevision (\x v -> x { getForestRevision = v })

forestAdminRevision =
  L.lens getForestAdminRevision (\x v -> x { getForestAdminRevision = v })

forestChunks =
  L.lens getForestChunks (\x v -> x { getForestChunks = v })

forestACL =
  L.lens getForestACL (\x v -> x { getForestACL = v })

forestACLTrie =
  L.lens getForestACLTrie (\x v -> x { getForestACLTrie = v })

forestPermitNone =
  L.lens getForestPermitNone (\x v -> x { getForestPermitNone = v })

forestTrees =
  L.lens getForestTrees (\x v -> x { getForestTrees = v })

forestTreeTrie =
  L.lens getForestTreeTrie (\x v -> x { getForestTreeTrie = v })

forestRules =
  L.lens getForestRules (\x v -> x { getForestRules = v })

forestDependencies =
  L.lens getForestDependencies (\x v -> x { getForestDependencies = v })

forestSanitized =
  L.lens getForestSanitized (\x v -> x { getForestSanitized = v })

updateForest current persisted name = do
  received <- get subscriberReceived

  case T.findValue name received of
    Just (P.Forest { P.getForestRevision = revision
                   , P.getForestSigned = signed
                   , P.getForestStream = stream
                   , P.getForestAdminRevision = adminRevision
                   , P.getForestAdminSigned = adminSigned
                   , P.getForestACL = acl
                   , P.getForestTrees = trees }) -> do

      adminACL <- get subscriberAdminACL

      verify adminSigned adminACL

      subscribed <- get subscriberStream

      when (revision <= getForestRevision persisted
            || adminRevision < getForestAdminRevision persisted
            || (adminRevision == getForestAdminRevision persisted
                && acl /= getForestACL persisted)
            || stream /= subscribed)
        badForest

      (aclTrie, permitNone) <-
        if acl == getForestACL current then
          return (getForestACLTrie current, getForestPermitNone current)
        else do
          aclTrie <-
            diffChunks (getForestChunks current) (getForestACL current)
            received rules
            >>= updateACL (getForestACLTrie current)

          verify signed aclTrie

          return (aclTrie, ACL.permitNone aclTrie)

      (treeTrie, unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized)) <-
        diffChunks (getForestChunks current) (getForestTrees current)
        received trees
        >>= updateTrees aclTrie current

      chunks <- updateChunks (getForestChunks current) <$> get subscriberDiff

      let forest = Forest revision adminRevision chunks acl aclTrie permitNone
                   trees treeTrie

      publicKey <- get subscriberPublicKey

      if null publicKey then
        return $ forest T.empty emptyRules BT.empty T.empty
        else

        let unsanitized =
              updateUnsanitized (getForestUnsanitized current)
              unsanitizedDiff in

        if null (T.sub P.rulesKey obsoleteUnsanitized)
           && null (T.sub P.rulesKey newUnsanitized) then
          let rules = getForestRules current

              (sanitized, dependencies) =
                updateSanitized
                revision
                (getForestPermitNone current)
                (getForestSanitized current)
                unsanitized
                rules
                (getForestDependencies current)
                unsanitizedDiff in

          return $ forest unsanitized rules dependencies sanitized
        else do
          permitAdmins <- get subscriberPermitAdmins

          let (sanitizedRules, _) =
                updateSanitized
                0
                permitAdmins
                (T.sub P.rulesKey (getForestSanitized current))
                (T.sub P.rulesKey unsanitized)
                R.emptyRules
                BT.empty
                ((T.sub P.rulesKey obsoleteUnsanitized),
                 (T.sub P.rulesKey newUnsanitized))

          rules <- compileRules sanitizedRules

          let (sanitized, dependencies) =
                updateSanitized
                revision
                (getForestPermitNone current)
                T.empty
                unsanitized
                rules
                BT.empty
                (T.empty, unsanitized)

          return $ forest unsanitized rules dependencies sanitized

    _ -> patternFailure

updateChunks chunks (obsoleteChunks, newChunks) =
  T.union newChunks $ T.subtract obsoleteChunks chunks

updateSubscriber diff subscriber =
  L.over subscriberReceived (flip updateChunks diff) subscriber

filterDiff (obsoleteChunks, newChunks) = do
  persisted <- get (forestChunks . subscriberPersisted)
  published <- get (forestChunks . subscriberPublished)

  return (foldr removeLive obsoleteChunks $ M.keys obsoleteChunks, newChunks)
    where removeLive key result =
            if M.member key persisted || M.member key published then
              M.remove key result
            else
              result

checkForest lens name =
  let resetDiff = set subscriberDiff (M.empty, M.empty)
      resetSubscriber = get subscriberClean >>= State.set in

  do assertComplete name

     updateM lens $ updateForest name

     diff <- get subscriberDiff >>= filterDiff

     update subscriberClean $ updateSubscriber diff

     resetSubscriber

  `catchError` \case
    MissingChunks -> resetDiff
    BadForest -> resetSubscriber
    error -> throwError error

checkIncomplete = do
  (T.paths <$> get (incompletePersisted . subscriberIncomplete))
    >>= mapM_ (checkForest subscriberPersisted)

  (T.paths <$> get (incompletePublished . subscriberIncomplete))
    >>= mapM_ (checkForest subscriberPublished)

assertComplete name =
  complete <- get subscriberMissing >>= null . BT.find name
  when (not complete) missingChunks

receiveChunk chunk name members = do
  update subscriberReceived $ T.insert name chunk
  addMissing name members
  removeMissing name
  checkIncomplete
