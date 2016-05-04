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
import Data.Maybe (isJust, isNothing, fromJust)

import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.ACL as ACL
import qualified Database.Concelo.Control as Co
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.VMap as VM
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

serializer private = Serializer private undefined (T.empty, T.empty)

serializeValues = Pr.serializeTrie . fmap Pr.serializeValue

serializeStrings = Pr.serializeTrie

serializeNames = Pr.serializeNames

instance ST.Serializer a SyncState where
  serialize trie = Pr.split <$> ($ trie) <$> get syncStateSerialize

  encrypt plaintext = do
    let length = BS.length plaintext

    when (length > Pr.leafSize) (exception "plaintext too large")

    get syncStateKey >>= \case
      Nothing -> return plaintext
      Just key -> do
        padding <- with syncStatePRNG $ Cr.randomBytes (Pr.leafSize - length)

        with syncStatePRNG
          $ Cr.encryptSymmetric key (plaintext `BS.append` padding)

newTree stream =
  Su.emptyTree stream . Just <$> with serializerPRNG Cr.newSymmetric

visitSync :: (T.Trie BS.ByteString a ->
              BS.ByteString) ->
             BS.ByteString ->
             ST.SyncTree a ->
             Maybe Cr.SymmetricKey ->
             BS.ByteString ->
             BS.ByteString ->
             T.Trie BS.ByteString [BS.ByteString] ->
             T.Trie BS.ByteString a ->
             T.Trie BS.ByteString a ->
             Co.Action Serializer Pr.Name
visitSync serialize level syncTree key treeStream forestStream rejected
  obsolete new = do
  prng <- get serializerPRNG

  ((obsolete, new, root), SyncState _ _ prng') <-
    eitherToAction $ run (ST.visit syncTree rejected obsolete new)
    (SyncState serialize key prng)

  set serializerPRNG prng'

  private <- get serializerPrivate

  let c2m = with serializerPRNG
            . ST.chunkToMessage private level treeStream forestStream

      visit result chunk =
        (\message -> T.union (const message <$> Pr.name message) result)
        <$> c2m chunk

  (allObsolete, allNew) <- get serializerDiff

  allObsolete' <- foldM visit allObsolete obsolete

  allNew' <- foldM visit allNew new

  set serializerDiff (allObsolete', allNew')

  maybe (return $ Pa.leaf ())
    ((Pr.name <$>)
     . with serializerPRNG
     . ST.chunkToMessage private level treeStream forestStream)
    root

visitTrees forest rejected revision obsoleteByACL newByACL =
  foldM visit (T.empty, T.empty) $ T.keys unionByACL where
    unionByACL = T.union obsoleteByACL newByACL

    visit result@(obsolete, new) stream = do
      private <- get serializerPrivate

      acl <- Pr.getValueACL <$> (maybe patternFailure return
                                 $ T.firstValue
                                 $ T.sub stream unionByACL)

      if (ACL.isWriter (Cr.derivePublic private) acl) then do
        let currentTree = VM.lookup stream $ Su.getForestTreeMap forest

        tree <- maybe (newTree stream) return currentTree

        leafRoot <- visitSync
                    serializeValues
                    Pr.treeLeafLevel
                    (Su.getTreeSync tree)
                    (Su.getTreeKey tree)
                    stream
                    (Pr.getForestStream $ Su.getForestMessage forest)
                    rejected
                    (T.sub stream obsoleteByACL)
                    (T.sub stream newByACL)

        aclRoot <-
          if isNothing currentTree then do
            aclTrie <- with serializerPRNG
                       $ ACL.toTrie (fromJust $ Su.getTreeKey tree) acl

            visitSync
              serializeStrings
              Pr.treeACLLevel
              ST.empty
              Nothing
              stream
              (Pr.getForestStream $ Su.getForestMessage forest)
              T.empty
              T.empty
              aclTrie
          else
            return $ Pr.getTreeACL $ Su.getTreeMessage tree

        message <-
          with serializerPRNG $ Pr.tree
          private
          stream
          (Pr.getForestStream $ Su.getForestMessage forest)
          False
          revision
          aclRoot
          leafRoot

        update serializerDiff $ \(obsolete, new) ->
          (if isJust currentTree then
             let oldMessage = Su.getTreeMessage tree in
             T.union (const oldMessage <$> Pr.getTreeName oldMessage) obsolete
           else
             obsolete,
           T.union (const message <$> Pr.getTreeName message) new)

        return (if isJust currentTree then
                  T.union (Pr.getTreeName $ Su.getTreeMessage tree) obsolete
                else
                  obsolete,
                T.union (Pr.getTreeName message) new)

        else
        return result

visitForest forest revision (obsoleteTrees, newTrees) = do
  private <- get serializerPrivate

  treeRoot <- visitSync
              serializeNames
              Pr.forestTreeLevel
              (Su.getForestTreeSync forest)
              Nothing
              BS.empty
              (Pr.getForestStream $ Su.getForestMessage forest)
              T.empty
              obsoleteTrees
              newTrees

  let old = Su.getForestMessage forest

  message <-
    with serializerPRNG $ Pr.forest
    private
    (Pr.getForestStream $ Su.getForestMessage forest)
    revision
    (Pr.getForestAdminRevision old)
    (Pr.getForestAdminSigned old)
    (Pr.getForestACL old)
    treeRoot

  update serializerDiff $ \(obsolete, new) ->
    (T.union (const old <$> Pr.getForestName old) obsolete,
     T.union (const message <$> Pr.getForestName message) new)

byACL path value = Pa.super (ACL.hash $ L.view Pr.valueACL value) path

group f = T.foldrPathsAndValues visit T.empty where
  visit (path, value) = T.union (f path value)

serialize forest rejected revision (obsolete, new) = do
  let obsoleteByACL = group byACL obsolete
      newByACL = group byACL new

  visitTrees forest rejected revision obsoleteByACL newByACL
    >>= visitForest forest revision

  getThenSet serializerDiff (T.empty, T.empty)
