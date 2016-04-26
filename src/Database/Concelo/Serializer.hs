{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Concelo.Serializer
  ( Serializer()
  , serializerPRNG
  , serializer
  , serialize ) where

import Database.Concelo.Control (exception, get, set, with, patternFailure,
                                 update, getThenSet, eitherToAction, run)

import Control.Monad (when, foldM)
import Data.Maybe (isJust)

import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.ACL as ACL
import qualified Database.Concelo.Control as Co
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.SyncTree as ST
import qualified Database.Concelo.Subscriber as Su
import qualified Database.Concelo.Trie as T

data Serializer =
  Serializer { getSerializerPrivate :: Cr.PrivateKey
             , getSerializerPRNG :: Cr.PRNG
             , getSerializerDiff :: (T.Trie BS.ByteString Pr.Message,
                                     T.Trie BS.ByteString Pr.Message) }

serializerPrivate =
  L.lens getSerializerPrivate (\x v -> x { getSerializerPrivate = v })

serializerPRNG :: L.Lens' Serializer Cr.PRNG
serializerPRNG =
  L.lens getSerializerPRNG (\x v -> x { getSerializerPRNG = v })

serializerTrees :: L.Lens' Serializer (M.Map BS.ByteString Tree)
serializerTrees =
  L.lens getSerializerTrees (\x v -> x { getSerializerTrees = v })

serializerForest :: L.Lens' Serializer Forest
serializerForest =
  L.lens getSerializerForest (\x v -> x { getSerializerForest = v })

serializerDiff :: L.Lens' Serializer (T.Trie BS.ByteString Pr.Message,
                                      T.Trie BS.ByteString Pr.Message)
serializerDiff =
  L.lens getSerializerDiff (\x v -> x { getSerializerDiff = v })

data SyncState a =
  SyncState { getSyncStateSerialize :: T.Trie BS.ByteString a ->
                                       BS.ByteString
            , getSyncStateKey :: Maybe Cr.SymmetricKey
            , getSyncStatePRNG :: Cr.PRNG }

syncStateSerialize :: L.Lens' (SyncState a) (T.Trie BS.ByteString a ->
                                             BS.ByteString)
syncStateSerialize =
  L.lens getSyncStateSerialize (\x v -> x { getSyncStateSerialize = v })

syncStateKey :: L.Lens' (SyncState a) (Maybe Cr.SymmetricKey)
syncStateKey =
  L.lens getSyncStateKey (\x v -> x { getSyncStateKey = v })

syncStatePRNG :: L.Lens' (SyncState a) Cr.PRNG
syncStatePRNG =
  L.lens getSyncStatePRNG (\x v -> x { getSyncStatePRNG = v })

serializer private stream =
  Serializer private undefined M.empty (Forest stream ST.empty undefined)
  (T.empty, T.empty)

split maxSize s =
  let (a, b) = BS.splitAt maxSize s in
  if BS.null b then
    [a]
  else
    a : split maxSize b

serializeValues = Pr.serializeTrie . fmap Pr.serializeValue

serializeStrings = Pr.serializeTrie

serializeNames = Pr.serializeNames

instance ST.Serializer a SyncState where
  serialize trie = split Pr.leafSize <$> ($ trie) <$> get syncStateSerialize

  encrypt plaintext = do
    let length = BS.length plaintext

    when (length > Pr.leafSize) (exception "plaintext too large")

    get syncStateKey >>= \case
      Nothing -> return plaintext
      Just key -> do
        padding <- with syncStatePRNG $ Cr.randomBytes (Pr.leafSize - length)

        with syncStatePRNG
          $ Cr.encryptSymmetric key (plaintext `BS.append` padding)

newTree stream = do
  key <- with serializerPRNG Cr.newSymmetric
  return $ L.set Su.treeKey key $ Su.emptyTree stream

chunkToMessage private level treeStream forestStream chunk =
  case ST.chunkHeight chunk of
    1 -> Pr.leaf private level treeStream forestStream $ ST.chunkBody chunk

    _ -> Pr.group private level (ST.chunkHeight chunk) treeStream forestStream
         $ foldr (\member -> (ST.chunkName member :)) []
         $ ST.chunkMembers chunk

visitSync serialize level syncTree key treeStream forestStream rejects obsolete new = do
  prng <- get serializerPRNG

  ((obsolete, new), SyncState _ _ prng') <-
    eitherToAction $ run (ST.visit syncTree rejects obsolete new)
    (SyncState serialize key prng)

  set serializerPRNG prng'

  private <- get serializerPrivate

  let c2m = with serializerPRNG
            . Pr.chunkToMessage private level treeStream forestStream

  obsoleteMessages <- mapM c2m obsolete

  newMessages <- mapM c2m new

  update serializerDiff $ \(allObsolete, allNew) ->
    (T.union obsoleteMessages allObsolete,
     T.union newMessages allNew)

visitTrees forest rejects revision obsoleteByACL newByACL =
  foldM visit (T.empty, T.empty) $ T.keys unionByACL where
    unionByACL = T.union obsoleteByACL newByACL

    visit result@(obsolete, new) stream = do
      private <- get serializerPrivate

      acl <- Pr.getValueACL <$> (maybe patternFailure return
                                 $ T.firstValue
                                 $ T.sub stream unionByACL)

      if (ACL.isWriter (Cr.derivePublic private) acl) then do
        let currentTree = M.lookup stream $ Su.getForestTreeMap forest

        tree <- maybe (newTree stream) return currentTree

        visitSync serializeValues Pr.treeLeafLevel sync
          (Just $ Su.getTreeKey tree) stream forestStream
          rejects (T.sub stream obsoleteByACL) (T.sub stream newByACL)

        when (isNothing currentTree) $ do
          aclTrie <- with serializerPRNG $ ACL.toTrie (Su.getTreeKey tree) acl

          visitSync serializeStrings Pr.treeACLLevel ST.empty Nothing stream
            forestStream T.empty T.empty aclTrie

        message <-
          with serializerPRNG $ Pr.tree private stream forestStream False
          revision (ST.root aclSync) (ST.root leafSync)

        update serializerDiff $ \(obsolete, new) ->
          (if isJust currentTree then
             let oldMessage = Su.getTreeMessage tree in
             T.union (const oldMessage <$> Pr.getTreeName oldMessage) obsolete
           else
             obsolete,
           T.union (const message <$> Pr.getTreeName message) new)

        return (if isJust currentTree then
                  T.union (Pr.getTreeName $ getTreeMessage tree) obsolete
                else
                  obsolete,
                T.union (Pr.getTreeName message) new)

        else
        return result

visitForest forest revision (obsoleteTrees, newTrees) = do
  private <- get serializerPrivate

  visitSync serializeNames Pr.forestTreeLevel (getForestTreeSync forest)
    Nothing BS.empty (getForestStream forest) T.empty obsoleteTrees newTrees

  let old = getForestMessage forest

  message <-
    with serializerPRNG $ Pr.forest private (getForestStream forest) revision
    (Pr.getForestAdminRevision old) (Pr.getForestAdminSigned old)
    (Pr.getForestACL old) (ST.root treeSync)

  update serializerDiff $ \(obsolete, new) ->
    (T.union (const old <$> Pr.getForestName old) obsolete,
     T.union (const message <$> Pr.getForestName message) new)

byACL path value = Pa.super (ACL.hash $ L.view Pr.valueACL value) path

group f = T.foldrPathsAndValues visit T.empty where
  visit (path, value) = T.union (f path value)

serialize forest rejects revision (obsolete, new) = do
  let obsoleteByACL = group byACL obsolete
      newByACL = group byACL new

  visitTrees forest rejects revision obsoleteByACL newByACL
    >>= visitForest forest revision

  getThenSet serializerDiff (T.empty, T.empty)
