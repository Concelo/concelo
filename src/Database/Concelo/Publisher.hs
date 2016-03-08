module Database.Concelo.Publisher
  ( Publisher()
  , publisherPublished
  , nextMessage
  , update
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

update (obsolete, new) = do
  acks <- updateThenGet publisherAcks $ T.intersectL new . T.subtract obsolete

  update publisherNacks (T.union (T.subtract acks new) . T.subtract obsolete)

nextMessage =
  (T.firstPath <$> get publisherNacks) >>= \case
    Just nack -> do
      update publisherNacks $ T.subtract nack
      update publisherAcks $ T.union nack
      return $ T.first nack

    Nothing -> return Nothing

receive hashAccessible = \case
  P.Nack path ->
    (T.findValue path <$> get publisherAcks) >>= \case
      Just message ->
        let h = P.getMessageKeyHash message in
        if null h || hashAccessible h then do
          update publisherNacks $ T.union (const message <$> nack)
          update publisherAcks $ T.subtract path
          return ()
        else
          return ()

      Nothing -> return ()

  _ -> patternFailure
