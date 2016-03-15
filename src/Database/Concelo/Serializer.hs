module Database.Concelo.Serializer
  ( serialize ) where

import Database.Concelo.Control (exception, get, with, try, updateM,
                                 patternFailure)

import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Database.Concelo.Path as Path
import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.ACL as ACL
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.ST as SyncTree
import qualified Database.Concelo.Trie as T

data KeyPair =
  KeyPair { getKeyPairPublic :: BS.ByteString
          , getKeyPairPrivate :: BS.ByteString }

data Tree =
  Tree { getTreeKey :: BS.ByteString
       , getTreeLeafSync :: ST.SyncTree P.Value
       , getTreeACLSync :: ST.SyncTree BS.ByteString
       , getTreeMessage :: P.Message }

data Forest =
  Forest { getForestStream :: BS.ByteString
         , getForestTreeSync :: ST.SyncTree P.Name
         , getForestMessage :: P.Message }

data Serializer =
  Serializer { getSerializerKeyPair :: KeyPair
             , getSerializerPRNG :: C.PRNG
             , getSerializerTrees :: M.Map BS.ByteString Tree
             , getSerializerForest :: Forest
             , getSerializerDiff :: (T.Trie BS.ByteString P.Message,
                                     T.Trie BS.ByteString P.Message) }

serializerKeyPair =
  L.lens getSerializerKeyPair (\x v -> x { getSerializerKeyPair = v })

serializerPRNG =
  L.lens getSerializerPRNG (\x v -> x { getSerializerPRNG = v })

serializerTrees =
  L.lens getSerializerTrees (\x v -> x { getSerializerTrees = v })

serializerForest =
  L.lens getSerializerForest (\x v -> x { getSerializerForest = v })

serializerDiff =
  L.lens getSerializerDiff (\x v -> x { getSerializerDiff = v })

byACL path value = Path.super (ACL.hash $ L.view P.valueACL value) path

data SyncState =
  SyncState { getSyncStateSerialize :: a -> [BS.ByteString]
            , getSyncStateKey :: BS.ByteString
            , getSyncStatePRNG :: C.PRNG }

syncStateSerialize =
  L.lens getSyncStateSerialize (\x v -> x { getSyncStateSerialize = v })

syncStateKey =
  L.lens getSyncStateKey (\x v -> x { getSyncStateKey = v })

syncStatePRNG =
  L.lens getSyncStatePRNG (\x v -> x { getSyncStatePRNG = v })

split maxSize s =
  let (a, b) = BS.splitAt maxSize s in
  if null b then
    [a]
  else
    a : split maxSize b

serializeValues = P.serializeTrie . fmap P.serializeValue

serializeStrings = P.serializeTrie

serializeNames = P.serializeNames

instance ST.Serializer SyncState where
  serialize trie = split P.leafSize <$> ($ trie) <$> get syncStateSerialize

  encrypt plaintext = do
    let length = BS.length plaintext

    when (length > P.leafSize) (exception "plaintext too large")

    key <- get syncStateKey

    if null key then
      return plaintext
      else do
      padding <- with syncStatePRNG $ C.randomBytes (P.leafSize - length)

      with syncStatePRNG
        $ C.encryptSymmetric key (plaintext `BS.append` padding)

newTree = do
  key <- with serializerPRNG $ C.randomBytes C.symmetricKeySize
  return Tree key ST.empty ST.empty P.NoMessage

chunkToMessage level treeStream forestStream chunk = do
  keyPair <- getKeyPair

  case ST.chunkHeight chunk of
    1 -> P.leaf keyPair level treeStream forestStream $ ST.chunkBody chunk

    _ -> P.group keyPair level (ST.chunkHeight chunk) treeStream forestStream
         $ foldr (\member -> (ST.chunkName member :)) []
         $ ST.chunkMembers chunk

sync serialize level syncTree key treeStream forestStream obsolete new = do
  keyPair <- getKeyPair
  prng <- get serializerPRNG

  ((result, obsolete, new), SyncState _ _ prng') <-
    try (ST.update P.leafSize syncTree obsolete new)
    (SyncState serialize key prng)

  set serializerPRNG prng'

  let c2m = chunkToMessage level treeStream forestStream

  obsoleteMessages <- foldM c2m T.empty obsolete

  newMessages <- foldM c2m T.empty new

  update serializerDiff $ \(allObsolete, allNew) ->
    (T.union obsoleteMessages allObsolete,
     T.union newMessages allNew)

  return result

getKeyPair = do
  pair <- get serializerKeyPair
  return (getKeyPairPublic pair, getKeyPairPrivate pair)

updateTrees revision obsoleteByACL newByACL unionByACL = do
  keyPair <- getKeyPair
  trees <- get serializerTrees
  forestStream <- get (forestStream . serializerForest)

  foldM visit (T.empty, T.empty) $ T.keys unionByACL where
    unionByACL = T.union obsoleteByACL newByACL

    visit stream result@(obsolete, new) = do
      acl <- maybe patternFailure return
             $ T.firstValue
             $ T.sub stream unionByACL

      if ACL.isWriter (fst keyPair) acl then do
        let currentTree = M.lookup stream trees

        tree <- maybe newTree return currentTree

        leafSync <-
          sync serializeValues P.treeLeafLevel (getTreeKey tree)
          stream forestStream (getTreeLeafSync tree)
          (T.sub stream obsoleteByACL) (T.sub stream newByACL)

        aclSync <-
          if isJust currentTree then
            return $ getTreeACLSync
          else do
            sync serializeStrings P.treeACLLevel ST.empty BS.empty stream
              forestStream T.empty $ ACL.toTrie acl

        message <-
          with serializerPRNG $ P.tree keyPair stream forestStream False
          revision (ST.root aclSync) (ST.root leafSync)

        update serializerTrees
          $ M.insert stream . Tree key leafSync aclSync message

        update serializerDiff $ \(obsolete, new) ->
          (if isJust currentTree then
             let oldMessage = getTreeMessage tree in
             T.union (const oldMessage <$> P.getTreeName oldMessage) obsolete
           else
             obsolete,
           T.union (const message <$> P.getTreeName message) new)

        return (if isJust currentTree then
                  T.union (P.getTreeName $ getTreeMessage tree) obsolete
                else
                  obsolete,
                T.union (P.getTreeName message) new)

        else
        return result

updateForest revision (obsoleteTrees, newTrees) = do
  keyPair <- getKeyPair
  forest <- get serializerForest

  treeSync <-
    sync serializeNames P.forestTreeLevel (getForestTreeSync forest) BS.empty
    BS.empty (getForestStream forest) obsoleteTrees newTrees

  let old = getForestMessage forest

  message <-
    P.forest keyPair (getForestStream forest) revision signed
    (P.getForestAdminRevision old) (P.getForestAdminSigned old)
    (P.getForestACL old) (ST.root treeSync)

  set serializerForest $ Forest (getForestStream forest) treeSync message

  update serializerDiff $ \(obsolete, new) ->
    (let oldMessage = getForestMessage forest in
      T.union (const oldMessage <$> P.getForestName oldMessage) obsolete,
     T.union (const message <$> P.getForestName message) new)

serialize revision (obsolete, new) = do
  let obsoleteByACL = T.index byACL obsolete
      newByACL = T.index byACL new

  updateTrees revision obsoleteByACL newByACL >>= updateForest revision

  getThenSet serializerDiff (T.empty, T.empty)
