{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Chunks
  ( diffChunks ) where

import Database.Concelo.Control (badForest, maybeM2)

import Control.Monad (foldM)

import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Control as C
import qualified Data.ByteString as BS

chunkIsLeaf = \case
  Pr.Leaf {} -> True
  _ -> False

chunkMembers = \case
  Pr.Group { Pr.getGroupMembers = m } -> m
  _ -> T.empty

findNewChunks oldChunks newChunks newRoot =
  visit (T.empty, T.empty, T.empty) newRoot where
    visit :: (T.Trie BS.ByteString Pr.Message,
              T.Trie BS.ByteString Pr.Message,
              T.Trie BS.ByteString ()) ->
             Pa.Path BS.ByteString () ->
             C.Action s (T.Trie BS.ByteString Pr.Message,
                         T.Trie BS.ByteString Pr.Message,
                         T.Trie BS.ByteString ())
    visit (new, newLeaves, found) name =
      case T.findValue name oldChunks of
        Just _ ->
          return (new, newLeaves, T.union name found)

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
                         found) (T.paths $ chunkMembers chunk)

findObsoleteChunks oldChunks oldRoot found =
  visit (T.empty, T.empty) oldRoot where
    visit :: (T.Trie BS.ByteString Pr.Message,
              T.Trie BS.ByteString Pr.Message) ->
             Pa.Path BS.ByteString () ->
             C.Action s (T.Trie BS.ByteString Pr.Message,
                         T.Trie BS.ByteString Pr.Message)
    visit result@(obsolete, obsoleteLeaves) name =
      if T.member name found then
        return result
      else
        maybeM2 T.findValue name oldChunks >>= \chunk ->
          foldM visit (T.union (const chunk <$> name) obsolete,
                       if chunkIsLeaf chunk then
                         T.union (const chunk <$> name) obsoleteLeaves
                       else
                         obsoleteLeaves) (T.paths $ chunkMembers chunk)

diffChunks oldChunks oldRoot newChunks newRoot = do
  (new, newLeaves, found) <- findNewChunks oldChunks newChunks newRoot

  (obsolete, obsoleteLeaves) <- findObsoleteChunks newChunks oldRoot found

  return (obsolete, obsoleteLeaves, new, newLeaves)
