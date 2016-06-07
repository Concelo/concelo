{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Chunks
  ( diffChunks ) where

import Database.Concelo.Control (badForest, maybeToAction,
                                 Exception(Exception))

import Control.Monad (foldM)
import Data.Functor ((<$>))

import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Control as C
import qualified Data.ByteString as BS

chunkIsGroup = \case
  Pr.Group {} -> True
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
    visit result@(new, newLeaves, found) name =
      if null (Pa.keys name) then
        return result
      else
        case T.findValue name oldChunks of
          Just _ ->
            return (new, newLeaves, T.union name found)

          Nothing ->
            maybeToAction
            (Exception ("could not find " ++ show name
                        ++ " in " ++ show newChunks))
            (T.findValue name newChunks) >>= \chunk ->
            -- we do not allow a given chunk to have more than one
            -- parent since it confuses the diff algorithm
            if T.member name new then
              badForest
            else
              foldM visit (T.union (const chunk <$> name) new,
                           if chunkIsGroup chunk then
                             newLeaves
                           else
                             T.union (const chunk <$> name) newLeaves,
                           found) (T.paths $ chunkMembers chunk)

findObsoleteChunks oldChunks oldRoot found =
  visit (T.empty, T.empty) oldRoot where
    visit :: (T.Trie BS.ByteString Pr.Message,
              T.Trie BS.ByteString Pr.Message) ->
             Pa.Path BS.ByteString () ->
             C.Action s (T.Trie BS.ByteString Pr.Message,
                         T.Trie BS.ByteString Pr.Message)
    visit result@(obsolete, obsoleteLeaves) name =
      if null (Pa.keys name) || T.member name found then
        return result
      else
        maybeToAction
        (Exception ("could not find " ++ show name
                    ++ " in " ++ show oldChunks))
        (T.findValue name oldChunks) >>= \chunk ->
          foldM visit (T.union (const chunk <$> name) obsolete,
                       if chunkIsGroup chunk then
                         obsoleteLeaves
                       else
                         T.union (const chunk <$> name) obsoleteLeaves)
          (T.paths $ chunkMembers chunk)

diffChunks oldChunks oldRoot newChunks newRoot = do
  (new, newLeaves, found) <- findNewChunks oldChunks newChunks newRoot

  (obsolete, obsoleteLeaves) <- findObsoleteChunks newChunks oldRoot found

  return (obsolete, obsoleteLeaves, new, newLeaves)
