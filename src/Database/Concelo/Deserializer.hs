{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Deserializer
  ( deserialize ) where

import Database.Concelo.Control (get, patternFailure)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import Control.Monad (foldM)

import qualified Data.ByteString as BS
import qualified Control.Lens as L
import qualified Control.Monad.State as St
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Rules as R
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.BiTrie as BT
import qualified Database.Concelo.Subscriber as Su
import qualified Database.Concelo.Chunks as Ch
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.ACL as ACL

data Deserializer =
  Deserializer { getDesPrivateKey :: Cr.PrivateKey
               , getDesPermitAdmins :: ACL.ACL
               , getDesPermitNone :: ACL.ACL
               , getDesUnsanitized :: T.Trie BS.ByteString UnsanitizedElement
               , getDesRules :: R.Rules
               , getDesDependencies :: BT.BiTrie BS.ByteString
               , getDesSanitized :: T.Trie BS.ByteString Pr.Value }

desPrivateKey =
  L.lens getDesPrivateKey (\x v -> x { getDesPrivateKey = v })

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

maybeHead = \case
  x:_ -> Just x
  _ -> Nothing

maybeTail = \case
  _:xs -> Just xs
  _ -> Nothing

visitDirty revision acl rules result =
  T.foldrKeys (visit M.empty (Pa.leaf ()) rules acl dirty) result dirty
  where
    (dirty, _, _) = result

    updateTrie path = \case
      Nothing -> T.subtract path
      Just v -> T.union (const v <$> path)

    visit env path rules acl dirty key result =
      visitValues result Nothing possibleValues where
        (rules', wildcard) = R.subRules key rules

        dirty' = T.sub key $ dirty

        path' = path `Pa.append` key

        env' = if null wildcard then env else M.insert wildcard key env

        possibleValues =
          fromMaybe M.empty (getUnsanitizedElementMap <$> T.value dirty')

        visitValues result@(remainingDirty, sanitized, dependencies)
          firstValid values =
          let value = maybeHead values

              root = R.rootVisitor $ updateTrie path' value sanitized

              visitor = foldr R.visitorChild root $ Pa.keys path'

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
                  (firstValid <|> (L.set Pr.valueACL acl' <$> value)) tail

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
            if maybe True (\v -> L.view Pr.valueSigner v `ACL.isWriter` acl')
               value
            then
              if remainingDirty' `T.hasAny` validateDependencies then
                T.foldrKeys visitDirty' result dirty'
              else
                next $ if validateResult then value else Nothing
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
  { getUnsanitizedElementMap :: M.Map BS.ByteString Pr.Value }

unionUnsanitized signer small large =
  T.foldrPathsAndValues visit large small where

    visit (path, new) result =
      case T.findValue path large of
        Nothing ->
          T.union path result
        Just element ->
          T.union ((const $ update element new) <$> path) result

    update element@(UnsanitizedElement map) new =
      case Pr.parseValue signer new of
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

  obsolete <- foldM (visit False) obsoleteUnsanitized obsoleteLeaves

  new <- foldM (visit True) newUnsanitized newLeaves

  return (obsolete, new) where

    visit leaf result =
      case leaf of
        Pr.Leaf { Pr.getLeafSigned = signed
               , Pr.getLeafBody = body } ->
          return $ case Pr.parseTrie (Cr.decryptSymmetric key body) of
            -- todo: handle defragmentation
            Just trie ->
              unionUnsanitized (Pr.getSignedSigner signed) trie result

            Nothing -> result

        _ -> patternFailure

diffChunks oldChunks oldRoot newChunks newRoot = do
  (_, obsoleteLeaves, _, newLeaves) <-
    Ch.diffChunks oldChunks oldRoot newChunks newRoot

  return (obsoleteLeaves, newLeaves)

updateUnsanitizedDiff oldForest newForest (stream, newTree) result = do
  privateKey <- get desPrivateKey

  let oldTree =
        fromMaybe Su.emptyTree $ M.lookup stream
        $ Su.getForestTreeMap oldForest

      maybeKey =
        T.findValue
        (Pa.super Pr.aclReaderKey
         $ Pa.singleton (Cr.fromPublic $ Cr.derivePublic privateKey) ())
        (Su.getTreeLeaves newTree)

  case maybeKey of
    Nothing -> return result

    Just encryptedKey -> do
      diffChunks
        (Su.getForestChunks oldForest)
        (Su.getTreeLeaves oldTree)
        (Su.getForestChunks newForest)
        (Su.getTreeLeaves newTree)
        >>= updateUnsanitizedDiff'
        (Cr.decryptAsymmetric privateKey encryptedKey)
        result

updateUnsanitized (obsolete, new) currentUnsanitized =
  unionUnsanitized new (subtractUnsanitized obsolete currentUnsanitized)

deserialize old new = do
  privateKey <- get desPrivateKey
  permitAdmins <- get desPermitAdmins

  unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized) <-
    foldM (updateUnsanitizedDiff old new) unsanitized
    (M.pairs $ Su.getForestTreeMap new)

  unsanitized <- get desUnsanitized >>= updateUnsanitized unsanitizedDiff

  permitNone <-
    if Su.getForestACL old == Su.getForestACL new then
      get desPermitNone
    else
      return $ ACL.permitNone $ Su.getForestACLTrie new

  let des = Deserializer privateKey permitAdmins permitNone unsanitized

  if null (T.sub Pr.rulesKey obsoleteUnsanitized)
     && null (T.sub Pr.rulesKey newUnsanitized) then
    let rules = Su.getForestRules old

        (sanitized, dependencies) =
          updateSanitized
          (Su.getForestRevision new)
          permitNone
          (Su.getForestSanitized old)
          unsanitized
          rules
          (Su.getForestDependencies old)
          unsanitizedDiff in

    St.set $ des rules dependencies sanitized permitNone
    else do
    let (sanitizedRules, _) =
          updateSanitized
          0
          permitAdmins
          (T.sub Pr.rulesKey (Su.getForestSanitized old))
          (T.sub Pr.rulesKey unsanitized)
          R.emptyRules
          BT.empty
          ((T.sub Pr.rulesKey obsoleteUnsanitized),
           (T.sub Pr.rulesKey newUnsanitized))

    rules <- compileRules sanitizedRules

    let (sanitized, dependencies) =
          updateSanitized
          (Su.getForestRevision new)
          permitNone
          T.empty
          unsanitized
          rules
          BT.empty
          (T.empty, unsanitized)

    St.set $ des rules dependencies sanitized permitNone
