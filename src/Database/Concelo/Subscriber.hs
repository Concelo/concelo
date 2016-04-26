{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Database.Concelo.Subscriber
  ( Subscriber()
  , subscriber
  , receive
  , Tree()
  , emptyTree
  , getTreeStream
  , getTreeLeaves
  , getTreeACLTrie
  , getTreeACLFromTries
  , getTreeOptional
  , Forest()
  , getForestChunks
  , getForestTreeMap
  , getForestTrees
  , getForestACL
  , getForestACLTrie
  , forestName
  , getSubscriberReceived
  , getSubscriberPublished
  , getSubscriberPersisted
  , getSubscriberClean
  , subscriberPublished
  , getForestRevision
  , forestRevision
  , nextMessage ) where

import Database.Concelo.Control (patternFailure, badForest, missingChunks,
                                 set, get, update, updateM, getThenUpdate)
import Control.Monad (foldM, when)
import Data.Maybe (fromMaybe)

import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.TrieLike as TL
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.VMap as VM
import qualified Database.Concelo.Set as Se
import qualified Database.Concelo.BiTrie as BT
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.Chunks as Ch
import qualified Database.Concelo.Control as Co
import qualified Database.Concelo.ACL as ACL
import qualified Control.Lens as L
import qualified Control.Monad.State as St
import qualified Control.Monad.Except as E
import qualified Data.ByteString as BS

data Incomplete =
  Incomplete { getIncompletePersisted :: Pr.Names
             , getIncompletePublished :: Pr.Names }

incompletePersisted :: L.Lens' Incomplete Pr.Names
incompletePersisted =
  L.lens getIncompletePersisted (\x v -> x { getIncompletePersisted = v })

incompletePublished :: L.Lens' Incomplete Pr.Names
incompletePublished =
  L.lens getIncompletePublished (\x v -> x { getIncompletePublished = v })

data Subscriber =
  Subscriber { getSubscriberIncomplete :: Incomplete
             , getSubscriberReceived :: VT.VTrie BS.ByteString Pr.Message
             , getSubscriberMissing :: BT.BiTrie BS.ByteString
             , getSubscriberAdminTrie :: Pr.Names
             , getSubscriberPublicKey :: Maybe Cr.PublicKey
             , getSubscriberStream :: BS.ByteString
             , getSubscriberPersisted :: Forest
             , getSubscriberPublished :: Forest
             , getSubscriberClean :: Subscriber
             , getSubscriberRequestedTrees :: Se.Set BS.ByteString
             , getSubscriberDiff :: (T.Trie BS.ByteString Pr.Message,
                                     T.Trie BS.ByteString Pr.Message)
             , getSubscriberVersion :: Integer }

subscriberIncomplete :: L.Lens' Subscriber Incomplete
subscriberIncomplete =
  L.lens getSubscriberIncomplete (\x v -> x { getSubscriberIncomplete = v })

subscriberReceived :: L.Lens' Subscriber (VT.VTrie BS.ByteString Pr.Message)
subscriberReceived =
  L.lens getSubscriberReceived (\x v -> x { getSubscriberReceived = v })

subscriberMissing :: L.Lens' Subscriber (BT.BiTrie BS.ByteString)
subscriberMissing =
  L.lens getSubscriberMissing (\x v -> x { getSubscriberMissing = v })

subscriberAdminTrie =
  L.lens getSubscriberAdminTrie (\x v -> x { getSubscriberAdminTrie = v })

subscriberPublicKey =
  L.lens getSubscriberPublicKey (\x v -> x { getSubscriberPublicKey = v })

subscriberStream =
  L.lens getSubscriberStream (\x v -> x { getSubscriberStream = v })

subscriberPersisted :: L.Lens' Subscriber Forest
subscriberPersisted =
  L.lens getSubscriberPersisted (\x v -> x { getSubscriberPersisted = v })

subscriberPublished :: L.Lens' Subscriber Forest
subscriberPublished =
  L.lens getSubscriberPublished (\x v -> x { getSubscriberPublished = v })

subscriberClean :: L.Lens' Subscriber Subscriber
subscriberClean =
  L.lens getSubscriberClean (\x v -> x { getSubscriberClean = v })

subscriberRequestedTrees =
  L.lens getSubscriberRequestedTrees (\x v -> x { getSubscriberRequestedTrees = v })

subscriberDiff :: L.Lens' Subscriber (T.Trie BS.ByteString Pr.Message,
                                      T.Trie BS.ByteString Pr.Message)
subscriberDiff =
  L.lens getSubscriberDiff (\x v -> x { getSubscriberDiff = v })

subscriberVersion :: L.Lens' Subscriber Integer
subscriberVersion =
  L.lens getSubscriberVersion (\x v -> x { getSubscriberVersion = v })

subscriber adminTrie publicKey stream = s
  where
    f = forest stream
    s = Subscriber (Incomplete T.empty T.empty) VT.empty BT.empty adminTrie
        publicKey stream f f s Se.empty (T.empty, T.empty) 0

receive = \case
  leaf@(Pr.Leaf { Pr.getLeafName = name }) ->
    receiveChunk leaf name T.empty

  group@(Pr.Group { Pr.getGroupName = name
                  , Pr.getGroupMembers = members }) ->
    receiveChunk group name members

  tree@(Pr.Tree { Pr.getTreeName = name
                , Pr.getTreeACL = acl }) ->
    receiveChunk tree name acl

  forest@(Pr.Forest { Pr.getForestName = name
                    , Pr.getForestTrees = trees
                    , Pr.getForestACL = acl }) ->
    receiveChunk forest name $ T.union trees $ T.union acl T.empty

  Pr.Persisted forest -> updateIncomplete incompletePersisted forest

  Pr.Published forest -> updateIncomplete incompletePublished forest

  _ -> patternFailure

updateIncomplete :: L.Lens' Incomplete Pr.Names ->
                    Pr.Name ->
                    Co.Action Subscriber ()
updateIncomplete lens name = do
  update (subscriberIncomplete . lens) $ T.union name
  checkIncomplete

addMissingToGroups member group missing =
  foldr (addMissingToGroups member) (BT.insert group member missing)
  $ T.paths $ BT.reverseFind group missing

addMissing :: TL.TrieLike t =>
              Pr.Name ->
              t BS.ByteString () ->
              Co.Action Subscriber ()
addMissing group members = do
  received <- get subscriberReceived

  let visit member missing = case VT.findValue member received of
        Nothing -> addMissingToGroups member group missing

        Just (Pr.Group { Pr.getGroupMembers = members }) ->
          T.foldrPaths visit missing members

        Just _ -> missing

  update subscriberMissing $ \missing -> TL.foldrPaths visit missing members

updateMissing :: Pr.Name ->
                 Co.Action Subscriber ()
updateMissing member =
  update subscriberMissing $ BT.reverseDelete member

diffChunksAll oldChunks oldRoot newChunks newRoot = do
  (obsolete, obsoleteLeaves, new, newLeaves) <-
    Ch.diffChunks oldChunks oldRoot newChunks newRoot

  update subscriberDiff $ \(o, n) -> (T.union obsolete o, T.union new n)

  return ((obsolete, new), (obsoleteLeaves, newLeaves))

diffChunks oldChunks oldRoot newChunks newRoot =
  snd <$> diffChunksAll oldChunks oldRoot newChunks newRoot

verify :: Pr.Signed ->
          T.Trie BS.ByteString a ->
          Co.Action Subscriber ()
verify (Pr.Signed { Pr.getSignedSigner = signer
                  , Pr.getSignedSignature = signature
                  , Pr.getSignedText = text }) acl =
  if T.member (Pa.super Pr.aclWriterKey
               $ Pa.singleton (Cr.fromPublic signer) ()) acl then
    if Cr.verify signer signature text then
      return ()
    else
      badForest
  else
    badForest

updateACL currentACL (obsoleteLeaves, newLeaves) = do
  subset <- foldM remove currentACL obsoleteLeaves
  foldM add subset newLeaves where
    remove :: T.Trie BS.ByteString BS.ByteString ->
              Pr.Message ->
              Co.Action Subscriber (T.Trie BS.ByteString BS.ByteString)
    remove acl leaf =
      case leaf of
        Pr.Leaf { Pr.getLeafBody = body } ->
          case Pr.parseTrie body of
            Just trie -> return $ T.subtract trie acl
            Nothing -> patternFailure

        _ -> patternFailure

    add acl leaf =
      case leaf of
        Pr.Leaf { Pr.getLeafSigned = signed
               , Pr.getLeafBody = body } -> do

          get subscriberAdminTrie >>= verify signed

          case Pr.parseTrie body of
            -- todo: handle defragmentation (or assert that no valid forest will contain a fragmented ACL)
            Just trie -> return $ T.union trie acl
            Nothing -> badForest

        _ -> patternFailure

verifyLeafDiff :: T.Trie BS.ByteString BS.ByteString ->
                  (a, T.Trie BS.ByteString Pr.Message) ->
                  Co.Action Subscriber ()
verifyLeafDiff acl (_, newLeaves) =
  mapM_ visit newLeaves where
    visit = \case
      Pr.Leaf { Pr.getLeafSigned = signed } -> verify signed acl
      _ -> patternFailure

updateSync old () =

data Tree = Tree { getTreeRevision :: Integer
                 , getTreeStream :: BS.ByteString
                 , getTreeACL :: Pr.Name
                 , getTreeACLTrie :: T.Trie BS.ByteString BS.ByteString
                 , getTreeACLFromTries :: ACL.ACL
                 , getTreeLeaves :: Pr.Name
                 , getTreeSync :: ST.SyncTree
                 , getTreeOptional :: Bool }

-- treeRevision :: L.Lens' Tree Integer
-- treeRevision =
--   L.lens getTreeRevision (\x v -> x { getTreeRevision = v })

-- treeStream :: L.Lens' Tree BS.ByteString
-- treeStream =
--   L.lens getTreeStream (\x v -> x { getTreeStream = v })

-- treeACL :: L.Lens' Tree Pr.Name
-- treeACL =
--   L.lens getTreeACL (\x v -> x { getTreeACL = v })

-- treeACLTrie :: L.Lens' Tree (T.Trie BS.ByteString BS.ByteString)
-- treeACLTrie =
--   L.lens getTreeACLTrie (\x v -> x { getTreeACLTrie = v })

-- treeLeaves  :: L.Lens' Tree Pr.Name
-- treeLeaves =
--   L.lens getTreeLeaves (\x v -> x { getTreeLeaves = v })

emptyTree stream =
  Tree (-1) stream (Pa.leaf ()) T.empty ACL.empty (Pa.leaf ()) False

updateTrees :: T.Trie BS.ByteString BS.ByteString ->
               Forest ->
               (T.Trie BS.ByteString Pr.Message,
                T.Trie BS.ByteString Pr.Message) ->
               Co.Action Subscriber (VM.VMap BS.ByteString Tree)
updateTrees forestACLTrie currentForest (obsoleteTrees, newTrees) =
  do
    subset <- foldM remove currentTrees obsoleteTrees
    foldM add subset newTrees
  where
    currentTrees = getForestTreeMap currentForest

    newByStream = M.index Pr.getTreeStream newTrees

    remove :: VM.VMap BS.ByteString Tree ->
              Pr.Message ->
              Co.Action Subscriber (VM.VMap BS.ByteString Tree)
    remove trees tree =
      case tree of
        Pr.Tree { Pr.getTreeStream = stream } -> do
          when (not $ M.member stream newByStream) badForest

          version <- get subscriberVersion

          return $ VM.delete version stream trees

        _ -> patternFailure

    add trees tree =
      case tree of
        Pr.Tree { Pr.getTreeRevision = revision
                , Pr.getTreeSigned = signed
                , Pr.getTreeName = name
                , Pr.getTreeACL = acl
                , Pr.getTreeStream = stream
                , Pr.getTreeForestStream = forestStream
                , Pr.getTreeOptional = optional
                , Pr.getTreeLeaves = leaves } -> do

          let old = VM.lookup stream currentTrees
              oldACL = maybe acl getTreeACL old
              currentTree = fromMaybe (emptyTree stream) old

          when (revision < getTreeRevision currentTree
                || VM.member stream trees
                || acl /= oldACL
                || forestStream /= getForestStream currentForest)
            badForest

          received <- T.trie <$> get subscriberReceived

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

          let descend = case publicKey of
                Nothing -> True
                Just k ->
                  ((not optional || Se.member stream requested)
                   && T.member (Pa.super Pr.aclReaderKey
                                $ Pa.singleton (Cr.fromPublic k) ())
                   aclTrie)

          sync <-
            if descend then do
              addMissing name leaves
              assertComplete name

              diff <- diffChunksAll (getForestChunks currentForest)
                      (getTreeLeaves currentTree) received leaves

              verifyLeafDiff aclTrie (snd diff)

              return $ ST.update (fst diff) (getTreeSync currentTree)
            else
              return ST.empty

          version <- get subscriberVersion

          return $ VM.insert version stream
            (Tree revision stream acl aclTrie
             (ACL.fromTries aclTrie forestACLTrie)
             (if descend then leaves else (Pa.leaf ()))
             sync optional) trees

        _ -> patternFailure

data Forest =
  Forest { getForestRevision :: Integer
         , getForestStream :: BS.ByteString
         , getForestAdminRevision :: Integer
         , getForestChunks :: T.Trie BS.ByteString Pr.Message
         , getForestACL :: Pr.Name
         , getForestACLTrie :: T.Trie BS.ByteString BS.ByteString
         , getForestTrees :: Pr.Name
         , getForestTreeMap :: VM.VMap BS.ByteString Tree
         , getForestName :: Pr.Name }

forestRevision :: L.Lens' Forest Integer
forestRevision =
  L.lens getForestRevision (\x v -> x { getForestRevision = v })

-- forestAdminRevision :: L.Lens' Forest Integer
-- forestAdminRevision =
--   L.lens getForestAdminRevision (\x v -> x { getForestAdminRevision = v })

forestChunks =
  L.lens getForestChunks (\x v -> x { getForestChunks = v })

-- forestACL :: L.Lens' Forest Pr.Name
-- forestACL =
--   L.lens getForestACL (\x v -> x { getForestACL = v })

-- forestACLTrie :: L.Lens' Forest (T.Trie BS.ByteString BS.ByteString)
-- forestACLTrie =
--   L.lens getForestACLTrie (\x v -> x { getForestACLTrie = v })

-- forestTrees :: L.Lens' Forest Pr.Name
-- forestTrees =
--   L.lens getForestTrees (\x v -> x { getForestTrees = v })

-- forestTreeMap :: L.Lens' Forest (M.Map BS.ByteString Tree)
-- forestTreeMap =
--   L.lens getForestTreeMap (\x v -> x { getForestTreeMap = v })

forestName :: L.Lens' Forest Pr.Name
forestName =
  L.lens getForestName (\x v -> x { getForestName = v })

forest stream =
  Forest (-1) stream (-1) T.empty (Pa.leaf ()) T.empty (Pa.leaf ()) VM.empty
  (Pa.leaf ())

updateForest name current = do
  received <- T.trie <$> get subscriberReceived
  published <- get subscriberPublished
  persisted <- get subscriberPersisted

  case T.findValue name received of
    Just (Pr.Forest { Pr.getForestRevision = revision
                    , Pr.getForestSigned = signed
                    , Pr.getForestStream = stream
                    , Pr.getForestAdminRevision = adminRevision
                    , Pr.getForestAdminSigned = adminSigned
                    , Pr.getForestACL = acl
                    , Pr.getForestTrees = trees }) -> do

      get subscriberAdminTrie >>= verify adminSigned

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

      return $ Forest revision subscribed adminRevision chunks acl aclTrie
        trees treeMap name

    _ -> patternFailure

updateChunks chunks (obsoleteChunks, newChunks) =
  T.union newChunks $ T.subtract obsoleteChunks chunks

updateChunksV version (obsoleteChunks, newChunks) =
  VT.union version newChunks . VT.subtract version obsoleteChunks

updateSubscriber version diff subscriber =
  L.over subscriberReceived (updateChunksV version diff) subscriber

filterDiff :: (T.Trie BS.ByteString Pr.Message, a) ->
              Co.Action Subscriber (T.Trie BS.ByteString Pr.Message, a)
filterDiff (obsoleteChunks, newChunks) = do
  persisted <- get (subscriberPersisted . forestChunks)
  published <- get (subscriberPublished . forestChunks)

  let removeLive path =
        if T.member path persisted || T.member path published then
          T.subtract path
        else
          id

  return (T.foldrPaths removeLive obsoleteChunks obsoleteChunks, newChunks)

checkForest :: L.Lens' Subscriber Forest ->
               Pr.Name ->
               Co.Action Subscriber ()
checkForest lens name =
  let resetDiff = set subscriberDiff (T.empty, T.empty)
      resetSubscriber = get subscriberClean >>= St.put in

  do assertComplete name

     updateM lens $ updateForest name

     diff <- get subscriberDiff >>= filterDiff

     version <- getThenUpdate subscriberVersion (+ 1)

     update subscriberClean $ updateSubscriber version diff

     resetSubscriber

  `E.catchError` \case
    Co.MissingChunks -> resetDiff
    Co.BadForest -> resetSubscriber
    error -> E.throwError error

checkIncomplete = do
  (T.paths <$> get (subscriberIncomplete . incompletePersisted))
    >>= mapM_ (checkForest subscriberPersisted)

  (T.paths <$> get (subscriberIncomplete . incompletePublished))
    >>= mapM_ (checkForest subscriberPublished)

assertComplete :: Pr.Name ->
                  Co.Action Subscriber ()
assertComplete name = do
  complete <- null . BT.find name <$> get subscriberMissing
  when (not complete) missingChunks

receiveChunk :: TL.TrieLike t =>
                Pr.Message ->
                Pr.Name ->
                t BS.ByteString () ->
                Co.Action Subscriber ()
receiveChunk chunk name members = do
  version <- get subscriberVersion
  update subscriberReceived $ VT.union version (const chunk <$> name)
  addMissing name members
  updateMissing name
  checkIncomplete

nextMessage :: Co.Action Subscriber (Maybe Pr.Message)
nextMessage =
  (T.firstValue . BT.reverseTrie <$> get subscriberMissing) >>= \case
    Nothing -> return Nothing
    Just path -> return $ Just $ Pr.Nack path
