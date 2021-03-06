{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Concelo.Subscriber
  ( Subscriber()
  , subscriber
  , receive
  , Tree()
  , emptyTree
  , getTreeMessage
  , getTreeACLTrie
  , getTreeACLFromTries
  , getTreeSync
  , getTreeKey
  , Forest()
  , getForestMessage
  , getForestChunks
  , getForestTreeMap
  , getForestTreeSync
  , getForestACLTrie
  , getSubscriberReceived
  , getSubscriberPublished
  , getSubscriberPersisted
  , getSubscriberClean
  , subscriberPublished
  , subscriberStream
  , subscriberPRNG
  , nextMessage ) where

import Database.Concelo.Control (patternFailure, badForest, missingChunks,
                                 set, get, update, updateM, getThenUpdate,
                                 exception, with)
import Control.Monad (when)
import Data.Functor ((<$>))
import Data.Foldable (foldr)
import Prelude hiding (foldr, mapM_, null)
import Database.Concelo.Misc (foldM, mapM_, null)
import Data.Maybe (fromMaybe, isNothing, fromJust)

import Debug.Trace

import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.TrieLike as TL
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.VMap as VM
import qualified Database.Concelo.SyncTree as ST
import qualified Database.Concelo.Set as Se
import qualified Database.Concelo.BiTrie as BT
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.Chunks as Ch
import qualified Database.Concelo.Control as Co
import qualified Database.Concelo.ACL as ACL
import qualified Control.Lens as L
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
  Subscriber { getSubscriberPRNG :: Cr.PRNG
             , getSubscriberIncomplete :: Incomplete
             , getSubscriberReceived :: VT.VTrie BS.ByteString Pr.Message
             , getSubscriberMissing :: BT.BiTrie BS.ByteString
             , getSubscriberAdminTrie :: Pr.Names
             , getSubscriberPrivateKey :: Maybe Cr.PrivateKey
             , getSubscriberStream :: BS.ByteString
             , getSubscriberPersisted :: Forest
             , getSubscriberPublished :: Forest
             , getSubscriberClean :: VT.VTrie BS.ByteString Pr.Message
             , getSubscriberRequestedTrees :: Se.Set BS.ByteString
             , getSubscriberDiff :: (T.Trie BS.ByteString Pr.Message,
                                     T.Trie BS.ByteString Pr.Message)
             , getSubscriberVersion :: Integer }

subscriberPRNG :: L.Lens' Subscriber Cr.PRNG
subscriberPRNG =
  L.lens getSubscriberPRNG (\x v -> x { getSubscriberPRNG = v })

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

subscriberPrivateKey =
  L.lens getSubscriberPrivateKey (\x v -> x { getSubscriberPrivateKey = v })

subscriberStream =
  L.lens getSubscriberStream (\x v -> x { getSubscriberStream = v })

subscriberPersisted :: L.Lens' Subscriber Forest
subscriberPersisted =
  L.lens getSubscriberPersisted (\x v -> x { getSubscriberPersisted = v })

subscriberPublished :: L.Lens' Subscriber Forest
subscriberPublished =
  L.lens getSubscriberPublished (\x v -> x { getSubscriberPublished = v })

subscriberClean :: L.Lens' Subscriber (VT.VTrie BS.ByteString Pr.Message)
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

subscriber adminTrie privateKey stream =
  Subscriber undefined (Incomplete T.empty T.empty) VT.empty BT.empty adminTrie
  privateKey stream f f VT.empty Se.empty (T.empty, T.empty) 0
  where
    f = emptyForest stream

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
                    , Pr.getForestACL = acl }) -> do
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
  if null (Pa.keys member) then missing else
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
  (subset, _) <- foldM remove (currentACL, T.empty) obsoleteLeaves
  fst <$> foldM add (subset, T.empty) newLeaves where
    remove :: (T.Trie BS.ByteString BS.ByteString,
               T.Trie BS.ByteString (BS.ByteString, BS.ByteString)) ->
              Pr.Message ->
              Co.Action Subscriber (T.Trie BS.ByteString BS.ByteString,
                                    T.Trie BS.ByteString (BS.ByteString,
                                                          BS.ByteString))
    remove (acl, fragments) = \case
      Pr.Leaf { Pr.getLeafBody = body } ->
        let (maybeTrie, fragments') = ST.defragment Pr.parseTrie fragments body
        in return (case maybeTrie of
                      Just trie -> T.subtract trie acl
                      Nothing -> acl,
                   fragments')

      _ -> patternFailure

    add (acl, fragments) = \case
      Pr.Leaf { Pr.getLeafSigned = signed
              , Pr.getLeafBody = body } -> do

        get subscriberAdminTrie >>= verify signed

        let (maybeTrie, fragments') = ST.defragment Pr.parseTrie fragments body

        -- todo: should we throw a BadForest if either a fragment is
        -- missing or the defragmented result fails to parse as a
        -- trie?
        return (case maybeTrie of
                   Just trie -> T.union trie acl
                   Nothing -> acl,
                fragments')

      _ -> patternFailure

verifyLeafDiff :: T.Trie BS.ByteString BS.ByteString ->
                  (a, T.Trie BS.ByteString Pr.Message) ->
                  Co.Action Subscriber ()
verifyLeafDiff acl (_, newLeaves) =
  mapM_ visit newLeaves where
    visit = \case
      Pr.Leaf { Pr.getLeafSigned = signed } -> verify signed acl
      _ -> patternFailure

data Tree = Tree { getTreeMessage :: Pr.Message
                 , getTreeACLTrie :: T.Trie BS.ByteString BS.ByteString
                 , getTreeACLFromTries :: ACL.ACL
                 , getTreeSync :: ST.SyncTree Pr.Value
                 , getTreeKey :: Maybe Cr.SymmetricKey }

emptyTree stream key =
  Tree
  (Pr.Tree
   undefined
   stream
   undefined
   False
   (-1)
   undefined
   (Pa.leaf ())
   (Pa.leaf ()))
  T.empty
  ACL.empty
  ST.empty
  key

updateTrees :: Integer ->
               T.Trie BS.ByteString BS.ByteString ->
               Forest ->
               (T.Trie BS.ByteString Pr.Message,
                T.Trie BS.ByteString Pr.Message) ->
               Co.Action Subscriber (VM.VMap BS.ByteString Tree)
updateTrees forestRevision forestACLTrie currentForest (obsoleteTrees, newTrees) =
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
        tree@(Pr.Tree { Pr.getTreeRevision = revision
                      , Pr.getTreeSigned = signed
                      , Pr.getTreeName = name
                      , Pr.getTreeACL = acl
                      , Pr.getTreeStream = stream
                      , Pr.getTreeForestStream = forestStream
                      , Pr.getTreeOptional = optional
                      , Pr.getTreeLeaves = leaves }) -> do

          let old = VM.lookup stream currentTrees
              oldACL = maybe acl (Pr.getTreeACL . getTreeMessage) old
              currentTree = fromMaybe (emptyTree stream Nothing) old

          when (revision < (Pr.getTreeRevision $ getTreeMessage currentTree)
                || VM.member stream trees
                || acl /= oldACL
                || forestStream /= (Pr.getForestStream
                                    $ getForestMessage currentForest))
            badForest

          received <- T.trie <$> get subscriberReceived

          -- kind of silly to do a diff, since the current trie will
          -- only either be empty or unchanged, but it does the job:
          aclTrie <-
            diffChunks (getForestChunks currentForest)
            (Pr.getTreeACL $ getTreeMessage currentTree) received acl
            >>= updateACL (getTreeACLTrie currentTree)

          when (not (forestACLTrie `T.hasAll` aclTrie)) badForest

          verify signed aclTrie

          privateKey <- get subscriberPrivateKey

          requested <- get subscriberRequestedTrees

          let aclFromTries = ACL.fromTries aclTrie forestACLTrie

              descend = case privateKey of
                Nothing -> True
                Just k ->
                  ((not optional || Se.member stream requested)
                   && T.member (Pa.super Pr.aclReaderKey
                                $ Pa.singleton
                                (Cr.fromPublic $ Cr.derivePublic k) ())
                   aclTrie)

          (sync, key) <-
            if descend then do
              addMissing name leaves
              assertComplete name

              diff <- diffChunksAll
                      (getForestChunks currentForest)
                      (Pr.getTreeLeaves $ getTreeMessage currentTree)
                      received
                      leaves

              verifyLeafDiff aclTrie (snd diff)

              case privateKey >>= \k ->
                T.findValue (Pa.super Pr.aclReaderKey
                             $ Pa.singleton
                             (Cr.fromPublic $ Cr.derivePublic k) ()) aclTrie of
                Nothing ->
                  return (ST.empty, Nothing)

                Just keyString -> do
                  key <- Cr.toSymmetric
                         <$> (with subscriberPRNG
                              $ Cr.decryptAsymmetric (fromJust privateKey)
                              keyString)

                  (, Just key) <$> ST.update
                    forestRevision
                    (Cr.decryptSymmetric key)
                    ((T.foldrPathsAndValues
                      (\(p, v) ->
                        case Pr.parseValue (Pr.getSignedSigner signed)
                             aclFromTries v
                        of
                          Nothing -> id
                          Just value -> T.union (const value <$> p))
                      T.empty <$>)
                     . Pr.parseTrie)
                    (fst diff)
                    (getTreeSync currentTree)
            else
              return (ST.empty, Nothing)

          version <- get subscriberVersion

          return $ VM.insert version stream
            (Tree tree aclTrie aclFromTries sync key) trees

        m -> exception ("unexpected message: " ++ show m)

data Forest =
  Forest { getForestMessage :: Pr.Message
         , getForestChunks :: T.Trie BS.ByteString Pr.Message
         , getForestACLTrie :: T.Trie BS.ByteString BS.ByteString
         , getForestTreeMap :: VM.VMap BS.ByteString Tree
         , getForestTreeSync :: ST.SyncTree () }

forestChunks =
  L.lens getForestChunks (\x v -> x { getForestChunks = v })

emptyForest stream =
  Forest
  (Pr.Forest
   (Pa.leaf ())
   stream
   (-1)
   undefined
   (-1)
   undefined
   (Pa.leaf ())
   (Pa.leaf ()))
  T.empty
  T.empty
  VM.empty
  ST.empty

updateForest name current = do
  received <- T.trie <$> get subscriberReceived
  published <- get subscriberPublished
  persisted <- get subscriberPersisted

  case T.findValue name received of
    Just (forest@(Pr.Forest { Pr.getForestRevision = revision
                            , Pr.getForestSigned = signed
                            , Pr.getForestStream = stream
                            , Pr.getForestAdminRevision = adminRevision
                            , Pr.getForestAdminSigned = adminSigned
                            , Pr.getForestACL = acl
                            , Pr.getForestTrees = trees })) -> do

      get subscriberAdminTrie >>= verify adminSigned

      subscribed <- get subscriberStream

      privateKey <- get subscriberPrivateKey

      when (revision < (Pr.getForestRevision $ getForestMessage persisted))
        $ traceM ("old revision " ++ show revision)

      when (isNothing privateKey
            && revision /= 1 + (Pr.getForestRevision
                                $ getForestMessage published))
        $ traceM ("incorrect revision: wanted "
                  ++ show (1 + (Pr.getForestRevision $ getForestMessage published))
                  ++ ", got " ++ show revision)

      when (adminRevision < (Pr.getForestAdminRevision
                             $ getForestMessage persisted))
        $ traceM "old admin revision"

      when (adminRevision == (Pr.getForestAdminRevision
                                  $ getForestMessage persisted)
            && acl /= (Pr.getForestACL $ getForestMessage persisted))
        $ traceM "wrong acl"

      when (stream /= subscribed)
        $ traceM "wrong stream"

      when (revision < (Pr.getForestRevision $ getForestMessage persisted)

            -- todo: if we receive a Pr.Published or Pr.Persisted
            -- message that refers to a revision we've already parsed
            -- and validated, just point to that, rather than re-parse
            -- it.  It might help to optimize this by caching a few
            -- revisions between the latest published and the latest
            -- persisted revisions.

            || (revision == (Pr.getForestRevision $ getForestMessage persisted)
                && name /= (Pr.getForestName $ getForestMessage persisted))

            || (isNothing privateKey
                && revision /= 1 + (Pr.getForestRevision
                                    $ getForestMessage published))

            || adminRevision < (Pr.getForestAdminRevision
                                $ getForestMessage persisted)

            || (adminRevision == (Pr.getForestAdminRevision
                                  $ getForestMessage persisted)
                && acl /= (Pr.getForestACL $ getForestMessage persisted))

            || stream /= subscribed)
        badForest

      aclTrie <-
        if acl == (Pr.getForestACL $ getForestMessage current) then
          return $ getForestACLTrie current
        else do
          aclTrie <- diffChunks
                     (getForestChunks current)
                     (Pr.getForestACL $ getForestMessage current)
                     received
                     acl
                     >>= updateACL (getForestACLTrie current)

          verify signed aclTrie

          return aclTrie

      treeDiff <- diffChunksAll
                  (getForestChunks current)
                  (Pr.getForestTrees $ getForestMessage current)
                  received
                  trees

      -- traceM ("tree diff " ++ show treeDiff)

      treeMap <- updateTrees revision aclTrie current (snd treeDiff)

      sync <- ST.update revision
              return
              (\hash -> Just $ T.union (Pa.singleton hash ()) T.empty)
              (fst treeDiff)
              (getForestTreeSync current)

      chunks <- updateChunks (getForestChunks current) <$> get subscriberDiff

      return $ Forest forest chunks aclTrie treeMap sync

    _ -> exception ("couldn't find " ++ show name
                    ++ " in " ++ show (const () <$> received))

updateChunks chunks (obsoleteChunks, newChunks) =
  T.union newChunks $ T.subtract obsoleteChunks chunks

updateChunksV version (obsoleteChunks, newChunks) =
  VT.union version newChunks . VT.subtract version obsoleteChunks

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
               L.Lens' Incomplete Pr.Names ->
               Pr.Name ->
               Co.Action Subscriber ()
checkForest lens incomplete name =
  let resetDiff = set subscriberDiff (T.empty, T.empty)
      resetAll = do
        resetDiff
        get subscriberClean >>= set subscriberReceived
        set subscriberIncomplete $ Incomplete T.empty T.empty
        set subscriberMissing BT.empty
  in

  do publishedName <-
       Pr.getForestName . getForestMessage <$> get subscriberPublished

     persistedName <-
       Pr.getForestName . getForestMessage <$> get subscriberPersisted

     if name == publishedName then
       get subscriberPublished >>= set lens
       else
       if name == persistedName then
         get subscriberPersisted >>= set lens
       else do
         assertComplete name

         updateM lens $ updateForest name

         diff <- get subscriberDiff >>= filterDiff

         version <- getThenUpdate subscriberVersion (+ 1)

         update subscriberReceived (updateChunksV version diff)

         get subscriberReceived >>= set subscriberClean

         resetDiff

     update (subscriberIncomplete . incomplete) $ T.subtract name

  `E.catchError` \case
    Co.MissingChunks -> resetDiff
    Co.BadForest -> resetAll
    error -> E.throwError error

checkIncomplete = do
  (T.paths <$> get (subscriberIncomplete . incompletePersisted))
    >>= mapM_ (checkForest subscriberPersisted incompletePersisted)

  (T.paths <$> get (subscriberIncomplete . incompletePublished))
    >>= mapM_ (checkForest subscriberPublished incompletePublished)

assertComplete :: Pr.Name ->
                  Co.Action Subscriber ()
assertComplete name = do
  received <- VT.member name <$> get subscriberReceived
  noneMissing <- null . BT.find name <$> get subscriberMissing
  when (not (received && noneMissing)) missingChunks

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

  -- forward <- BT.forwardTrie <$> get subscriberMissing
  -- reverse <- BT.reverseTrie <$> get subscriberMissing
  -- traceM ("received " ++ show name ++ "; forward: " ++ show forward ++ "; reverse: " ++ show reverse)

nextMessage :: Co.Action Subscriber (Maybe Pr.Message)
nextMessage = do
  missing <- ((const () <$>) . BT.reverseTrie <$> get subscriberMissing)

  return $ if T.isEmpty missing then
             Nothing
           else
             Just $ Pr.Nack missing
