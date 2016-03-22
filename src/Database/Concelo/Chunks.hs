{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Chunks
  ( diffChunks ) where

import Database.Concelo.Control (badForest, maybeM2)

import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Trie as T

chunkIsLeaf = \case
  P.Leaf {} -> True
  _ -> False

chunkMembers = \case
  P.Group { P.getGroupMembers = m } -> m
  _ -> T.empty

findNewChunks oldChunks newChunks newRoot =
  visit newRoot (T.empty, T.empty, T.empty) where
    visit name (new, newLeaves, found) =
      case T.find name oldChunks of
        Just chunk ->
          return (new, newLeaves, T.union (const chunk <$> name) found)

        Nothing -> maybeM2 T.findValue name newChunks >>= \chunk ->
          -- we do not allow a given chunk to have more than one
          -- parent since it confuses the diff algorithm
          if T.member name new then
            badForest
          else
            foldM visit (T.union (const chunk <$> name) new,
                         if chunkIsLeaf chunk then
                           T.union (const chunk <$> name) newLeaves
                         else
                           newLeaves,
                         found) (chunkMembers chunk)

findObsoleteChunks oldChunks oldRoot found =
  visit oldRoot (T.empty, T.empty) where
    visit name result@(obsolete, obsoleteLeaves) =
      if T.member name found then
        result
      else
        maybeM2 T.findValue name oldChunks >>= \chunk ->
          foldM visit (T.union (const chunk <$> name) obsolete,
                       if chunkIsLeaf chunk then
                         T.union (const chunk <$> name) obsoleteLeaves
                       else
                         obsoleteLeaves) (chunkMembers chunk)

diffChunks oldChunks oldRoot newChunks newRoot = do
  (new, newLeaves, found) <- findNewChunks oldChunks newChunks newRoot

  (obsolete, obsoleteLeaves) <- findObsoleteChunks newChunks oldRoot found

  return (obsolete, obsoleteLeaves, new, newLeaves)
