{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Publisher
  ( Publisher()
  , publisherPublished
  , nextMessage
  , update
  , receive
  , publisher ) where

import Database.Concelo.Control (updateThenGet, get, patternFailure, set)

import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Control as C
import qualified Control.Lens as L
import qualified Data.ByteString as BS

data Publisher =
  Publisher { getPublisherPublished :: Pr.Name
            , getPublisherAcks :: T.Trie BS.ByteString Pr.Message
            , getPublisherNacks :: T.Trie BS.ByteString Pr.Message }

publisherPublished :: L.Lens' Publisher Pr.Name
publisherPublished =
  L.lens getPublisherPublished (\x v -> x { getPublisherPublished = v })

publisherAcks :: L.Lens' Publisher (T.Trie BS.ByteString Pr.Message)
publisherAcks = L.lens getPublisherAcks (\x v -> x { getPublisherAcks = v })

publisherNacks :: L.Lens' Publisher (T.Trie BS.ByteString Pr.Message)
publisherNacks = L.lens getPublisherNacks (\x v -> x { getPublisherNacks = v })

publisher = Publisher (Pa.leaf ()) T.empty T.empty

update :: Pr.Name ->
          (T.Trie BS.ByteString Pr.Message,
           T.Trie BS.ByteString Pr.Message) ->
          C.Action Publisher ()
update published (obsolete, new) = do
  set publisherPublished published

  acks <- updateThenGet publisherAcks $ T.intersectL new . T.subtract obsolete

  C.update publisherNacks (T.union (T.subtract acks new) . T.subtract obsolete)


nextMessage :: C.Action Publisher (Maybe Pr.Message)
nextMessage =
  (T.firstPath <$> get publisherNacks) >>= \case
    Just nack -> do
      C.update publisherNacks $ T.subtract nack
      C.update publisherAcks $ T.union nack
      return $ Just $ Pa.value nack

    Nothing -> return Nothing

chunkStream = \case
  Pr.Leaf { Pr.getLeafTreeStream = ts } -> ts
  Pr.Group { Pr.getGroupTreeStream = ts } -> ts
  _ -> BS.empty

receive :: (BS.ByteString -> Bool) ->
           Pr.Message ->
           C.Action Publisher ()
receive streamAccessible = \case
  Pr.Nack names -> mapM_ visit $ T.paths names

  _ -> patternFailure

  where
    visit :: Pr.Name ->
             C.Action Publisher ()
    visit path = (T.findValue path <$> get publisherAcks) >>= \case
      Just message ->
        let s = chunkStream message in
        if BS.null s || streamAccessible s then do
          C.update publisherNacks $ T.union (const message <$> path)
          C.update publisherAcks $ T.subtract path
        else
          return ()

      Nothing ->
        error ("can't find " ++ show path)
        -- return ()
