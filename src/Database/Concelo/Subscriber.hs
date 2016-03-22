{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Subscriber
  ( receive
  , subscriberPublished
  , getForestRevision ) where

import Database.Concelo.Control (patternFailure, badForest, missingChunks,
                                 set, get, update, updateM)

import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Set as S
import qualified Database.Concelo.BiTrie as BT
import qualified Database.Concelo.Path as Path
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Chunks as Chunks
import qualified Control.Lens as L
import qualified Control.Monad.State.Class as State
import qualified Data.ByteString as BS

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
             , getSubscriberPublicKey :: BS.ByteString
             , getSubscriberStream :: P.Name
             , getSubscriberPersisted :: Forest
             , getSubscriberPublished :: Forest
             , getSubscriberClean :: Subscriber
             , getSubscriberRequestedTrees :: S.Set BS.ByteString
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

subscriberPublicKey =
  L.lens getSubscriberPublicKey (\x v -> x { getSubscriberPublicKey = v })

subscriberStream =
  L.lens getSubscriberStream (\x v -> x { getSubscriberStream = v })

subscriberPersisted =
  L.lens getSubscriberPersisted (\x v -> x { getSubscriberPersisted = v })

subscriberPublished =
  L.lens getSubscriberPublished (\x v -> x { getSubscriberPublished = v })

subscriberClean =
  L.lens getSubscriberClean (\x v -> x { getSubscriberClean = v })

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
          T.foldrPaths visit result members

        Just _ -> result

  update subscriberMissing $ \missing -> T.foldrPaths visit missing members

updateMissing member =
  update subscriberMissing $ BT.reverseDelete member

diffChunks oldChunks oldRoot newChunks newRoot = do
  (obsolete, obsoleteLeaves, new, newLeaves) <-
    Chunks.diffChunks oldChunks oldRoot newChunks newRoot

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

verifyLeafDiff acl result@(_, newLeaves) = do
  mapM_ visit newLeaves
  return result where
    visit = \case
      P.Leaf { P.getLeafSigned = signed } -> verify signed acl
      _ -> patternFailure

data Tree = Tree { getTreeRevision :: Integer
                 , getTreeStream :: BS.ByteString
                 , getTreeACL :: P.Name
                 , getTreeACLTrie :: T.Trie BS.ByteString BS.ByteString
                 , getTreeLeaves :: P.Name }

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
  let currentTrees = getForestTreeMap currentForest in do
    subset <- foldM remove currentTrees obsoleteTrees

    foldM add subset newTrees where

      newByStream = M.index getTreeStream newTrees

      remove tree trees =
        case tree of
          P.Tree { P.getTreeStream = stream } -> do
            when (not $ M.member stream newByStream) badForest
            return $ M.delete stream trees

          _ -> patternFailure

      add tree trees =
        case tree of
          P.Tree { P.getTreeRevision = revision
                 , P.getTreeSigned = signed
                 , P.getTreeName = name
                 , P.getTreeACL = acl
                 , P.getTreeStream = stream
                 , P.getTreeForestStream = forestStream
                 , P.getTreeOptional = optional
                 , P.getTreeLeaves = leaves } -> do

            let old = M.lookup stream currentTrees
                oldACL = maybe acl getTreeACL old
                currentTree = fromMaybe (emptyTree stream) old

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

            publicKey <- get subscriberPublicKey

            requested <- get subscriberRequestedTrees

            let descend =
                  null publicKey
                  || ((not optional || S.member stream requested)
                      && T.member (Path.super P.aclReaderKey
                                   $ Path.singleton publicKey ()) aclTrie)

            when descend do
              addMissing name leaves
              assertComplete name

              diffChunks (getForestChunks currentForest)
                (getTreeLeaves currentTree) received leaves
                >>= verifyLeafDiff aclTrie

            return $ M.insert stream
              (Tree revision stream acl aclTrie
               if descend then leaves else Path.empty) trees

          _ -> patternFailure

data Forest =
  Forest { getForestRevision :: Integer
         , getForestAdminRevision :: Integer
         , getForestChunks :: T.Trie BS.ByteString P.Message
         , getForestACL :: P.Name
         , getForestACLTrie :: T.Trie BS.ByteString BS.ByteString
         , getForestTrees :: P.Name
         , getForestTreeMap :: M.Map BS.ByteString Tree }

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

forestTrees =
  L.lens getForestTrees (\x v -> x { getForestTrees = v })

forestTreeMap =
  L.lens getForestTreeMap (\x v -> x { getForestTreeMap = v })

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

      publicKey <- get subscriberPublicKey

      when (revision <= getForestRevision persisted

            || (null publicKey
                && revision /= 1 + getForestRevision published)

            || adminRevision < getForestAdminRevision persisted

            || (adminRevision == getForestAdminRevision persisted
                && acl /= getForestACL persisted)

            || stream /= subscribed)
        badForest

      aclTrie <-
        if acl == getForestACL current then
          return $ getForestACLTrie current
        else do
          aclTrie <-
            diffChunks (getForestChunks current) (getForestACL current)
            received acl
            >>= updateACL (getForestACLTrie current)

          verify signed aclTrie

          return aclTrie

      treeMap <-
        diffChunks (getForestChunks current) (getForestTrees current)
        received trees
        >>= updateTrees aclTrie current

      chunks <- updateChunks (getForestChunks current) <$> get subscriberDiff

      return $ Forest revision adminRevision chunks acl aclTrie trees treeMap

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
  update subscriberReceived $ T.union (const chunk $ name)
  addMissing name members
  removeMissing name
  checkIncomplete
