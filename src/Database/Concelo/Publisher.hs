module Database.Concelo.Publisher
  ( empty ) where

update (allObsolete, allNew) = do
  acks <-
    updateThenGet publisherAcks $ T.intersect allNew . T.subtract allObsolete

  update publisherNacks
    (T.union (T.subract acks allNew) . T.subtract allObsolete)

nextMessage =
  T.firstPath <$> (get ignisNacks) >>= \case
    Just nack -> do
      update publisherNacks $ T.subtract nack
      update publisherAcks $ T.union nack
      return $ T.first nack

    Nothing -> return Nothing

receive = \case
  P.Nack path ->
    fmap (T.findPath path) (get ignisAcks) >>= \case
      Just nack -> do
        update ignisNacks $ T.union nack
        update ignisAcks $ T.subtract nack
        return ()

      Nothing -> return ()

  _ -> patternFailure
