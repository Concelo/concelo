module Database.Concelo.Relay
  ( make
  , receive
  , nextMessages ) where

make = Relay Sub.empty Pub.empty S.empty Nothing 0

share subscriber =
  get relayRelays >>= mapM_ (setSubscriber subscriber)

chunks = L.get (subscriberReceived . subscriberClean)

setRelays = set relayRelays

setSubscriber new = do
  set relayPendingSubscriber new

  publicKey <- get relayPublicKey
  case publicKey of
    Nothing ->
      return ()

    Just key -> do
      previous <- get relaySubscriber
      set relaySubscriber new

      keys <- updateAndGet relayKeys $ updateKeys key previous new

      update relayPublisher $ Pub.update $ filterDiff keys
      $ M.diff (chunks previous) (chunks new)

updateKeys publicKey oldSubscriber newSubscriber keys =
  let allChunks = chunks newSubscriber in
  updateKeys' allChunks publicKey
  (L.get (forestTreeTrie . subscriberPublished) oldSubscriber)
  (L.get (forestTreeTrie . subscriberPublished) newSubscriber)
  $ updateKeys' allChunks publicKey
  (L.get (forestTreeTrie . subscriberPersisted) oldSubscriber)
  (L.get (forestTreeTrie . subscriberPersisted) newSubscriber)
  keys

updateKeys' allChunks publicKey oldTrees newTrees keys =
  foldr maybeAdd (foldr maybeRemove keys obsolete) new where
    (obsolete, new) = M.diff oldTrees newTrees

    getHash acl = T.find (T.super ReadKeys $ T.key publicKey) acl
      >> T.find (T.key ReadKeyHash) acl

    maybeRemove obsolete keys =
      let acl = getTreeACL obsolete in
      if M.member acl allChunks then
        keys
      else
        maybe keys (\hash -> S.delete hash keys) (getHash acl)

    add maybeAdd keys =
      let acl = getTreeACL obsolete in
      maybe keys (\hash -> S.insert hash keys) (getHash acl)

filterDiff keys (obsoleteChunks, newChunks) =
  (M.foldrWithKeys maybeRemove obsoleteChunks obsoleteChunks,
   M.foldrWithKeys maybeRemove newChunks newChunks) where
    maybeRemove key chunk chunks =
      case chunkKeyHash chunk of
        Nothing -> chunks
        Just keyHash ->
          if S.member keyHash keys then
            chunks
          else
            M.delete key chunks

chunkKeyHash = \case
  P.Leaf { P.getLeafKeyHash = keyHash } -> keyHash
  P.Group { P.getGroupKeyHash = keyHash } -> keyHash
  _ -> Nothing

receive = \case
  P.Cred request publicKey signature -> do
    relays <- get relayRelays
    subscriber <- get relayPendingSubscriber
    State.set $ make relays subscriber

    get relayChallenge >>= verify signature publicKey
    set relayPublicKey $ Just publicKey

  nack@(P.Nack {}) -> do
    publicKey <- get relayPublicKey
    when (isJust publicKey) $ with relayPublisher $ Pub.receive nack

  P.Persisted {} -> return ()

  -- todo: stream (authenticated) chunks to subscribers as they are
  -- received rather than wait until we have a complete tree
  message -> do
    publicKey <- get relayPublicKey
    when (isJust publicKey) $ get relaySubscriber >>= \subscriber ->
      case execute (Sub.receive message) subscriber of
        Left error -> throwError error
        Right subscriber' ->
          let revision = L.get (forestRevision . subscriberPublished) in
          if revision subscriber' > revision subscriber then
            share subscriber
          else
            set relaySubscriber subscriber'

ping = do
  published <-
    P.Published <$> get (forestName . subscriberPublished . relaySubscriber)

  persisted <-
    P.Persisted <$> get (forestName . subscriberPersisted . relaySubscriber)

  return [published, persisted]

nextMessages now =
  publicKey <- get relayPublicKey
  lastPing <- get relayLastPing
  case publicKey of
    Nothing ->
      if now - lastPing > pingInterval then do
        set relayLastPing now
        ((:[]) . P.Challenge) <$> get relayChallenge
      else
        return []

    Just _ ->
      PubSub.nextMessages relayPublisher relaySubscriber relayPubSent
      relayLastPing ping
