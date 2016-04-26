{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Database.Concelo.Deserializer
  ( Deserializer()
  , desSanitized
  , getDesSanitized
  , getDesDefaultACL
  , getDesRules
  , deserializer
  , deserialize ) where

import Database.Concelo.Control (get, patternFailure, with)
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Control.Applicative ((<|>))
import Control.Monad (foldM)

import qualified Data.ByteString as BS
import qualified Control.Lens as L
import qualified Control.Monad.State as St
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.VMap as VM
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Rules as R
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.BiTrie as BT
import qualified Database.Concelo.Subscriber as Su
import qualified Database.Concelo.Chunks as Ch
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.ACL as ACL

data Deserializer =
  Deserializer { getDesPrivateKey :: Cr.PrivateKey
               , getDesPermitAdmins :: ACL.ACL
               , getDesDefaultACL :: ACL.ACL
               , getDesUnsanitized :: T.Trie BS.ByteString UnsanitizedElement
               , getDesRules :: R.Rules
               , getDesDependencies :: BT.BiTrie BS.ByteString
               , getDesSanitized :: VT.VTrie BS.ByteString Pr.Value
               , getDesRejected :: T.Trie BS.ByteString UnsanitizedElement
               , getDesPRNG :: Cr.PRNG }

desPrivateKey =
  L.lens getDesPrivateKey (\x v -> x { getDesPrivateKey = v })

desPermitAdmins =
  L.lens getDesPermitAdmins (\x v -> x { getDesPermitAdmins = v })

desDefaultACL =
  L.lens getDesDefaultACL (\x v -> x { getDesDefaultACL = v })

desUnsanitized =
  L.lens getDesUnsanitized (\x v -> x { getDesUnsanitized = v })

desRules =
  L.lens getDesRules (\x v -> x { getDesRules = v })

desDependencies =
  L.lens getDesDependencies (\x v -> x { getDesDependencies = v })

desSanitized =
  L.lens getDesSanitized (\x v -> x { getDesSanitized = v })

desPRNG :: L.Lens' Deserializer Cr.PRNG
desPRNG =
  L.lens getDesPRNG (\x v -> x { getDesPRNG = v })

deserializer private permitAdmins defaultACL =
  Deserializer private permitAdmins defaultACL T.empty
  R.identity BT.empty VT.empty undefined

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
      Just (_, v) -> T.union (const v <$> path)

    visit env path rules acl dirty key result =
      visitValues result Nothing $ M.pairs possibleValues where
        (rules', wildcard) = R.subRules key rules

        dirty' = T.sub key $ dirty

        path' = path `Pa.append` key

        env' = if BS.null wildcard then env else M.insert wildcard key env

        possibleValues = fromMaybe M.empty
          (getUnsanitizedElementMap <$> T.value dirty')

        visitValues result@(remainingDirty, sanitized, rejected, dependencies)
          firstValid values =
          let value = maybeHead values

              root = R.rootVisitor $ updateTrie path' value $ T.trie sanitized

              visitor = foldr R.visitorChild root $ Pa.keys path'

              context = R.context revision env' root visitor

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

              visit' = visit env' path' rules' acl' dirty'

              next value = case maybeTail values of
                Just tail ->
                  visitValues
                  (remainingDirty, sanitized, rejected, dependencies')
                  (firstValid <|> (L.set Pr.valueACL acl' <$> value)) tail

                Nothing ->
                  T.foldrKeys visit'
                  (remainingDirty',

                   case firstValid of
                     Nothing ->
                       VT.subtract revision path sanitized
                     Just (_, v) ->
                       VT.union revision (const v <$> path) sanitized,

                   let map = case firstValid of
                         Nothing -> possibleValues
                         Just (k, _) -> M.delete k possibleValues in

                   if null map then
                     T.subtract path rejected
                   else
                     T.union (const (M.keys map) <$> path) rejected,

                   dependencies')
                  dirty' in

          if remainingDirty `T.hasAny` writeDependencies then
            T.foldrKeys visit' result dirty'
          else
            if maybe True
               (\(_, v) -> L.view Pr.valueSigner v `ACL.isWriter` acl')
               value
            then
              if remainingDirty' `T.hasAny` validateDependencies then
                T.foldrKeys visit' result dirty'
              else
                next $ if validateResult then value else Nothing
            else
              next Nothing

updateSanitized revision acl currentSanitized currentRejected
  updatedUnsanitized updatedRules currentDependencies
  (obsoleteUnsanitized, newUnsanitized)
  =
    clean (dirty, currentSanitized, currentRejected, remainingDependencies)
  where
    unsanitized = T.union obsoleteUnsanitized newUnsanitized

    dirty = T.foldrPaths findDirty unsanitized unsanitized

    remainingDependencies = BT.subtract dirty currentDependencies

    findDirty :: Pa.Path BS.ByteString v ->
                 T.Trie BS.ByteString UnsanitizedElement ->
                 T.Trie BS.ByteString UnsanitizedElement
    findDirty path result =
      T.foldrPaths findDirty
      (T.union
       (const (fromMaybe (UnsanitizedElement M.empty)
               (T.findValue path updatedUnsanitized)) <$> path)
       result)
      (BT.reverseFind path currentDependencies)

    clean result@(dirty, sanitized, rejected, dependencies)
      | null dirty = (sanitized, rejected, dependencies)
      | otherwise =
        clean $ visitDirty revision acl updatedRules result

newtype UnsanitizedElement =
  UnsanitizedElement
  { getUnsanitizedElementMap :: M.Map BS.ByteString Pr.Value }

emptyUnsanitizedElement = UnsanitizedElement M.empty

-- todo: there's a lot of code duplicated between
-- unionUnsanitizedWithRaw and unionUnsanitizedWithRaw; fix that

unionUnsanitizedWithRaw hash signer acl small large =
  T.foldrPathsAndValues visit large small where

    visit (path, value) =
      let e@(UnsanitizedElement map) =
            update (fromMaybe emptyUnsanitizedElement (T.findValue path large))
            value in
      if null map then
        id
      else
        T.union (e <$> path)

    update element@(UnsanitizedElement map) new =
      case Pr.parseValue signer acl new of
        Nothing -> element
        Just v -> UnsanitizedElement $ M.insert hash v map

unionUnsanitized small large =
  T.foldrPathsAndValues visit large small where

    visit (path, value) =
      let e@(UnsanitizedElement map) =
            update (fromMaybe emptyUnsanitizedElement (T.findValue path large))
            value in
      if null map then
        id
      else
        T.union (e <$> path)

    update (UnsanitizedElement old) (UnsanitizedElement new) =
      UnsanitizedElement $ M.union new old

subtractUnsanitized small large =
  T.foldrPathsAndValues visit large small where

    visit (path, UnsanitizedElement obsolete) result =
      case T.findValue path large of
        Nothing -> result
        Just (UnsanitizedElement map) ->
          let map' = M.subtract obsolete map in
          if null map' then
            T.subtract path result
          else
            T.union ((const $ UnsanitizedElement map') <$> path) result

updateUnsanitizedDiff' key acl (obsoleteUnsanitized, newUnsanitized)
  (obsoleteLeaves, newLeaves) = do

  obsolete <- foldM visit obsoleteUnsanitized obsoleteLeaves

  new <- foldM visit newUnsanitized newLeaves

  return (obsolete, new) where

    visit result leaf =
      case leaf of
        Pr.Leaf { Pr.getLeafSigned = signed
                , Pr.getLeafName = name
                , Pr.getLeafBody = body } ->
          (Pr.parseTrie <$> Cr.decryptSymmetric key body) >>= \case
            -- todo: handle defragmentation
            Just trie -> do
              hash <- chunkHash leaf

              return $ unionUnsanitizedWithRaw
              hash (Pr.getSignedSigner signed) acl trie result

            Nothing -> return result

        _ -> patternFailure

diffChunks oldChunks oldRoot newChunks newRoot = do
  (_, obsoleteLeaves, _, newLeaves) <-
    Ch.diffChunks oldChunks oldRoot newChunks newRoot

  return (obsoleteLeaves, newLeaves)

updateUnsanitizedDiff oldForest newForest result (stream, newTree) = do
  privateKey <- get desPrivateKey

  let oldTree =
        fromMaybe (Su.emptyTree stream)
        $ VM.lookup stream
        $ Su.getForestTreeMap oldForest

      maybeKey =
        T.findValue
        (Pa.super Pr.aclReaderKey
         $ Pa.singleton (Cr.fromPublic $ Cr.derivePublic privateKey) ())
        (Su.getTreeACLTrie newTree)

  case maybeKey of
    Nothing -> return result

    Just encryptedKey -> do
      key <- with desPRNG $ Cr.decryptAsymmetric privateKey encryptedKey

      diffChunks
        (Su.getForestChunks oldForest)
        (Su.getTreeLeaves oldTree)
        (Su.getForestChunks newForest)
        (Su.getTreeLeaves newTree)

        >>= updateUnsanitizedDiff'
        (Cr.toSymmetric key)
        (Su.getTreeACLFromTries newTree)
        result

updateUnsanitized (obsolete, new) currentUnsanitized =
  unionUnsanitized new (subtractUnsanitized obsolete currentUnsanitized)

deserialize old new = do
  privateKey <- get desPrivateKey
  permitAdmins <- get desPermitAdmins
  oldRules <- get desRules
  oldSanitized <- get desSanitized
  oldDependencies <- get desDependencies

  unsanitizedDiff@(obsoleteUnsanitized, newUnsanitized) <-
    foldM (updateUnsanitizedDiff old new) (T.empty, T.empty)
    (VM.pairs $ Su.getForestTreeMap new)

  unsanitized <- updateUnsanitized unsanitizedDiff <$> get desUnsanitized

  defaultACL <-
    if Su.getForestACL old == Su.getForestACL new then
      get desDefaultACL
    else
      -- todo: union this ACL with permitAdmins
      return $ ACL.fromBlackTrie $ Su.getForestACLTrie new

  let des = Deserializer privateKey permitAdmins defaultACL unsanitized

  if null (T.sub Pr.rulesKey obsoleteUnsanitized)
     && null (T.sub Pr.rulesKey newUnsanitized) then
    let (sanitized, rejected, dependencies) =
          updateSanitized
          (Su.getForestRevision new)
          defaultACL
          oldSanitized
          oldRejected
          unsanitized
          oldRules
          oldDependencies
          unsanitizedDiff in

    get desPRNG >>= St.put . des oldRules dependencies sanitized rejected
    else do
    let (sanitizedRules, _, _) =
          updateSanitized
          0
          permitAdmins
          (VT.sub Pr.rulesKey oldSanitized)
          (T.sub Pr.rulesKey unsanitized)
          R.identity
          BT.empty
          ((T.sub Pr.rulesKey obsoleteUnsanitized),
           (T.sub Pr.rulesKey newUnsanitized))

    rules <- R.parse (fromMaybe BS.empty . Pr.valueString
                      <$> T.trie sanitizedRules)

    let (sanitized, rejected, dependencies) =
          updateSanitized
          (Su.getForestRevision new)
          defaultACL
          VT.empty
          T.empty
          unsanitized
          rules
          BT.empty
          (T.empty, unsanitized)

    get desPRNG >>= St.put . des rules dependencies sanitized rejected
