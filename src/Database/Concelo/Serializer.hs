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
import qualified Database.Concelo.Trie as T

data Tree =
  Tree { getTreeKey :: Cr.SymmetricKey
       , getTreeLeafSync :: ST.SyncTree Pr.Value
       , getTreeACLSync :: ST.SyncTree BS.ByteString
       , getTreeMessage :: Pr.Message }

data Forest =
  Forest { getForestStream :: BS.ByteString
         , getForestTreeSync :: ST.SyncTree ()
         , getForestMessage :: Pr.Message }

forestStream :: L.Lens' Forest BS.ByteString
forestStream =
  L.lens getForestStream (\x v -> x { getForestStream = v })

data Serializer =
  Serializer { getSerializerPrivate :: Cr.PrivateKey
             , getSerializerPRNG :: Cr.PRNG
             , getSerializerTrees :: M.Map BS.ByteString Tree
             , getSerializerForest :: Forest
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

newTree = do
  key <- with serializerPRNG Cr.newSymmetric
  return $ Tree key ST.empty ST.empty Pr.NoMessage

chunkToMessage private level treeStream forestStream chunk =
  case ST.chunkHeight chunk of
    1 -> Pr.leaf private level treeStream forestStream $ ST.chunkBody chunk

    _ -> Pr.group private level (ST.chunkHeight chunk) treeStream forestStream
         $ foldr (\member -> (ST.chunkName member :)) []
         $ ST.chunkMembers chunk

sync serialize level syncTree key treeStream forestStream obsolete new = do
  prng <- get serializerPRNG

  ((result, obsolete, new), SyncState _ _ prng') <-
    eitherToAction $ run (ST.update syncTree obsolete new)
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

  return result

updateTrees :: Integer ->
               T.Trie BS.ByteString Pr.Value ->
               T.Trie BS.ByteString Pr.Value ->
               Co.Action Serializer (T.Trie BS.ByteString (),
                                     T.Trie BS.ByteString ())
updateTrees revision obsoleteByACL newByACL =
  foldM visit (T.empty, T.empty) $ T.keys unionByACL where
    unionByACL = T.union obsoleteByACL newByACL

    visit result@(obsolete, new) stream = do
      private <- get serializerPrivate
      trees <- get serializerTrees
      forestStream <- get (serializerForest . forestStream)

      acl <- Pr.getValueACL <$> (maybe patternFailure return
                                 $ T.firstValue
                                 $ T.sub stream unionByACL)

      if ACL.isWriter (Cr.derivePublic private) acl then do
        let currentTree = M.lookup stream trees

        tree <- maybe newTree return currentTree

        leafSync <-
          sync serializeValues Pr.treeLeafLevel (getTreeLeafSync tree)
          (Just $ getTreeKey tree) stream forestStream
          (T.sub stream obsoleteByACL) (T.sub stream newByACL)

        aclSync <-
          if isJust currentTree then
            return $ getTreeACLSync tree
          else do
            aclTrie <- with serializerPRNG $ ACL.toTrie (getTreeKey tree) acl

            sync serializeStrings Pr.treeACLLevel ST.empty Nothing stream
              forestStream T.empty aclTrie

        message <-
          with serializerPRNG $ Pr.tree private stream forestStream False
          revision (ST.root aclSync) (ST.root leafSync)

        update serializerTrees
          $ M.insert stream $ Tree (getTreeKey tree) leafSync aclSync message

        update serializerDiff $ \(obsolete, new) ->
          (if isJust currentTree then
             let oldMessage = getTreeMessage tree in
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

updateForest revision (obsoleteTrees, newTrees) = do
  private <- get serializerPrivate
  forest <- get serializerForest

  treeSync <-
    sync serializeNames Pr.forestTreeLevel (getForestTreeSync forest) Nothing
    BS.empty (getForestStream forest) obsoleteTrees newTrees

  let old = getForestMessage forest

  message <-
    with serializerPRNG $ Pr.forest private (getForestStream forest) revision
    (Pr.getForestAdminRevision old) (Pr.getForestAdminSigned old)
    (Pr.getForestACL old) (ST.root treeSync)

  set serializerForest $ Forest (getForestStream forest) treeSync message

  update serializerDiff $ \(obsolete, new) ->
    (T.union (const old <$> Pr.getForestName old) obsolete,
     T.union (const message <$> Pr.getForestName message) new)

byACL path value = Pa.super (ACL.hash $ L.view Pr.valueACL value) path

group f = T.foldrPathsAndValues visit T.empty where
  visit (path, value) = T.union (f path value)

serialize revision (obsolete, new) = do
  let obsoleteByACL = group byACL obsolete
      newByACL = group byACL new

  updateTrees revision obsoleteByACL newByACL >>= updateForest revision

  getThenSet serializerDiff (T.empty, T.empty)
