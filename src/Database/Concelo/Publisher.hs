module Database.Concelo.Publisher
  ( Publisher
  , publisherPublished
  , empty ) where

import Data.ByteString (ByteString)
import Database.Concelo.Control (update, updateThenGet)

import qualified Control.Lens as L
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Protocol as P

data Publisher = Publisher { getPublisherPublished :: ByteString
                           , getPublisherAcks :: T.Trie ByteString P.Message
                           , getPublisherNacks :: T.Trie ByteString P.Message }

publisherPublished =
  L.lens getPublisherPublished (\x v -> x { getPublisherPublished = v })

publisherAcks = L.lens getPublisherAcks (\x v -> x { getPublisherAcks = v })

publisherNacks = L.lens getPublisherNacks (\x v -> x { getPublisherNacks = v })

update allObsolete allNew = do
  acks <-
    updateThenGet publisherAcks $ T.intersectL allNew . T.subtract allObsolete

  update publisherNacks
    (T.union (T.subtract acks allNew) . T.subtract allObsolete)

nextMessage =
  (T.firstPath <$> get publisherNacks) >>= \case
    Just nack -> do
      update publisherNacks $ T.subtract nack
      update publisherAcks $ T.union nack
      return $ T.first nack

    Nothing -> return Nothing

receive hashAccessible = \case
  P.Nack path ->
    (T.find path <$> get publisherAcks) >>= \case
      Just (nack, message) ->
        let h = P.getMessageKeyHash message in
        if null h || hashAccessible h then do
          update publisherNacks $ T.union nack
          update publisherAcks $ T.subtract nack
          return ()
        else
          return ()

      Nothing -> return ()

  _ -> patternFailure
