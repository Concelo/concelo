module Database.Concelo.Deserializer
  ( deserialize ) where

import Database.Concelo.Control (get)

import qualified Data.ByteString as BS
import qualified Control.Lens as L
import qualified Control.Monad.State.Class as State
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Path as Path
import qualified Database.Concelo.Rules as R
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.BiTrie as BT
import qualified Database.Concelo.Subscriber as Sub
import qualified Database.Concelo.Chunks as Chunks
import qualified Database.Concelo.Crypto as C

data KeyPair =
  KeyPair { getKeyPairPublic :: BS.ByteString
          , getKeyPairPrivate :: BS.ByteString }

data Deserializer =
  Deserializer { getDesKeyPair :: KeyPair
               , getDesPermitAdmins :: ACL.ACL
               , getDesPermitNone :: ACL.ACL
               , getDesUnsanitized :: T.Trie BS.ByteString UnsanitizedElement
               , getDesRules :: R.Rules
               , getDesDependencies :: BT.BiTrie BS.ByteString
               , getDesSanitized :: T.Trie BS.ByteString P.Value }

desKeyPair =
  L.lens getDesKeyPair (\x v -> x { getDesKeyPair = v })

desPermitAdmins =
  L.lens getDesPermitAdmins (\x v -> x { getDesPermitAdmins = v })

desPermitNone =
  L.lens getDesPermitNone (\x v -> x { getDesPermitNone = v })

desUnsanitized =
  L.lens getDesUnsanitized (\x v -> x { getDesUnsanitized = v })

desRules =
  L.lens getDesRules (\x v -> x { getDesRules = v })

desDependencies =
  L.lens getDesDependencies (\x v -> x { getDesDependencies = v })

desSanitized =
  L.lens getDesSanitized (\x v -> x { getDesSanitized = v })

visitDirty revision acl rules result =
  T.foldrKeys (visit M.empty (Path.root ()) rules acl dirty) result dirty
  where
    (dirty, _, _) = result

    updateTrie path = \case
      Nothing -> T.subtract path
      Just v -> T.union (const v <$> path)

    visit env path rules acl dirty key result =
      visitValues result Nothing possibleValues where
        (rules', wildcard) = R.subRules key rules

        dirty' = T.sub key $ dirty

        path' = path `Path.append` key

        env' = if null wildcard then env else M.set wildcard key env

        possibleValues =
          fromMaybe M.empty (getUnsanitizedElementMap <$> T.value dirty')

        visitValues result@(remainingDirty, sanitized, dependencies)
          firstValid values =
          let value = maybeHead values

              root = R.rootVisitor $ updateTrie path' value sanitized

              visitor = foldr R.visitorChild rootVisitor $ Path.keys path'

              context = R.Context env' revision root visitor T.empty

              (readACL, _) = R.getRulesRead rules' context acl

              (acl', writeDependencies) =
                R.getRulesWrite rules' context readACL

              (validateResult, validateDependencies) = case value of
                Nothing -> (True, T.empty)
                Just _ -> R.getRulesValidate rules' context

              dependencies' =
                BT.insertTrie path writeDependencies
                $ BT.insertTrie path validateDependencies
                dependencies

              remainingDirty' = T.subtract path remainingDirty

              visitDirty' = visitDirty env' path' rules' acl' dirty'

              next value = case maybeTail values of
                Just tail ->
                  visitValues (remainingDirty, sanitized, dependencies')
                  (firstValid <|> (L.set P.valueACL acl' <$> value)) tail

                Nothing ->
                  T.foldrKeys visitDirty'
                  (remainingDirty',
                   case firstValid of
                     Nothing -> T.subtract path sanitized
                     Just v -> T.union (const v <$> path) sanitized,
                   dependencies')
                  dirty' in

          if remainingDirty `T.hasAny` writeDependencies then
            T.foldrKeys visitDirty' result dirty'
          else
            if maybe True (\v -> L.view P.valueSigner v `ACL.isWriter` acl')
               value
            then
              if remainingDirty' `T.hasAny` validateDependencies then
                T.foldrKeys visitDirty' result dirty'
              else
                next if validateResult then value else Nothing
            else
              next Nothing

updateSanitized revision acl currentSanitized updatedUnsanitized
  updatedRules currentDependencies (obsoleteUnsanitized, newUnsanitized) =

    clean (dirty, currentSanitized, remainingDependencies) where

      unsanitized = T.union obsoleteUnsanitized newUnsanitized

      dirty = T.foldrPaths findDirty unsanitized unsanitized

      remainingDependencies = BT.subtract dirty currentDependencies

      findDirty path result =
        T.foldrPaths findDirty
        (T.union
         (fromMaybe (UnsanitizedElement T.empty)
          (T.findValue path updatedUnsanitized) <$> path)
         result)
        (BT.reverseFind path currentDependencies)

      clean result@(dirty, sanitized, dependencies)
        | null dirty = (sanitized, dependencies)
        | otherwise =
          clean $ visitDirty revision acl updatedRules result

newtype UnsanitizedElement =
  UnsanitizedElement
  { getUnsanitizedElementMap :: M.Map BS.ByteString P.Value }

unionUnsanitized signer small large =
  T.foldrPathsAndValues visit large small where

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
  T.foldrPathsAndValues visit large small where

    visit (path, obsolete) result =
      case T.findValue path large of
        Nothing -> result
        Just (UnsanitizedElement map) ->
          let map' = M.delete obsolete map in
          if null map' then
            T.subtract path result
          else
            T.union ((const $ UnsanitizedElement map') <$> path) result

updateUnsanitizedDiff' key (obsoleteUnsanitized, newUnsanitized)
  (obsoleteLeaves, newLeaves) = do

  obsolete <- foldM (visit false) obsoleteUnsanitized obsoleteLeaves

  new <- foldM (visit true) newUnsanitized newLeaves

  return (obsolete, new) where

    visit leaf result =
      case leaf of
        P.Leaf { P.getLeafSigned = signed
               , P.getLeafBody = body } ->
          return case P.parseTrie (C.decryptSymmetric key body) of
            -- todo: handle defragmentation
            Just trie ->
              unionUnsanitized (P.getSignedSigner signed) trie result

            Nothing -> result

        _ -> patternFailure

diffChunks oldChunks oldRoot newChunks newRoot = do
  (_, obsoleteLeaves, _, newLeaves) <-
    Chunks.diffChunks oldChunks oldRoot newChunks newRoot

  return (obsoleteLeaves, newLeaves)

updateUnsanitizedDiff oldForest newForest (stream, newTree) result = do
  let oldTree =
        fromMaybe Sub.emptyTree $ M.lookup stream
        $ Sub.getForestTreeMap old

      maybeKey =
        T.findValue
        (Path.super P.aclReaderKey $ Path.singleton publicKey ())
        (Sub.getTreeLeaves newTree)

  case maybeKey of
    Nothing -> return result

    Just encryptedKey -> do
      keyPair <- get desKeyPair

      diffChunks
        (Sub.getForestChunks oldForest)
        (Sub.getTreeLeaves oldTree)
        (Sub.getForestChunks newForest)
        (Sub.getTreeLeaves newTree)
        >>= updateUnsanitizedDiff'
        (C.decryptAsymmetric (getKeyPairPrivate keyPair) encryptedKey)
        result

updateUnsanitized (obsolete, new) currentUnsanitized =
  unionUnsanitized new (subtractUnsanitized obsolete currentUnsanitized)

deserialize old new = do
  keyPair <- get desKeyPair
  permitAdmins <- get desPermitAdmins

  unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized) <-
    foldM (updateUnsanitizedDiff old new) unsanitized
    (M.pairs $ Sub.getForestTreeMap new)

  unsanitized <- get desUnsanitized >>= updateUnsanitized unsanitizedDiff

  permitNone <-
    if Sub.getForestACL old == Sub.getForestACL new then
      get desPermitNone
    else
      return $ ACL.permitNone $ Sub.getForestACLTrie new

  let des = Deserializer keyPair permitAdmins permitNone unsanitized

  if null (T.sub P.rulesKey obsoleteUnsanitized)
     && null (T.sub P.rulesKey newUnsanitized) then
    let rules = Sub.getForestRules old

        (sanitized, dependencies) =
          updateSanitized
          (Sub.getForestRevision new)
          permitNone
          (Sub.getForestSanitized old)
          unsanitized
          rules
          (Sub.getForestDependencies old)
          unsanitizedDiff in

    State.set $ des rules dependencies sanitized permitNone
    else do
    let (sanitizedRules, _) =
          updateSanitized
          0
          permitAdmins
          (T.sub P.rulesKey (Sub.getForestSanitized old))
          (T.sub P.rulesKey unsanitized)
          R.emptyRules
          BT.empty
          ((T.sub P.rulesKey obsoleteUnsanitized),
           (T.sub P.rulesKey newUnsanitized))

    rules <- compileRules sanitizedRules

    let (sanitized, dependencies) =
          updateSanitized
          (Sub.getForestRevision new)
          permitNone
          T.empty
          unsanitized
          rules
          BT.empty
          (T.empty, unsanitized)

    State.set $ des rules dependencies sanitized permitNone
