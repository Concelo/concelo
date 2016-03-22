module Database.Concelo.Relay
  ( empty
  , receive
  , nextMessages
  , setSubscriber ) where

import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Set as S
import qualified Database.Concelo.Path as Path
import qualified Database.Concelo.Publisher as Pub
import qualified Database.Concelo.Subscriber as Sub
import qualified Database.Concelo.Pipe as Pipe
import qualified Control.Lens as L
import qualified Control.Monad.State.Class as State
import qualified Data.ByteString as BS

data Relay =
  Relay { getRelayPipe :: Pipe.Pipe
        , getRelayPendingSubscriber :: Maybe Sub.Subscriber
        , getRelayStreams :: S.Set BS.ByteString
        , getRelayPublicKey :: Maybe BS.ByteString
        , getRelayChallenge :: BS.ByteString }

relayPipe =
  L.lens getRelayPipe (\x v -> x { getRelayPipe = v })

relayPendingSubscriber =
  L.lens getRelayPendingSubscriber
  (\x v -> x { getRelayPendingSubscriber = v })

relayStreams =
  L.lens getRelayStreams (\x v -> x { getRelayStreams = v })

relayPublicKey =
  L.lens getRelayPublicKey (\x v -> x { getRelayPublicKey = v })

relayChallenge =
  L.lens getRelayChallenge (\x v -> x { getRelayChallenge = v })

empty = relay Pipe.empty Nothing

relay pipe publicKey =
  Relay pipe Nothing S.empty publicKey

chunks = L.get (Sub.subscriberReceived . Sub.subscriberClean)

setSubscriber new = do
  set relayPendingSubscriber $ Just new

  get relayPublicKey >>= \case
    Nothing -> return ()

    Just publicKey -> do
      previous <- get relaySubscriber

      set relaySubscriber new

      streams <-
        updateAndGet relayStreams
        $ updateStreams publicKey previous new

      update (Pipe.pipePublisher . relayPipe)
        $ Pub.update
        $ filterDiff streams
        $ M.diff (chunks previous) (chunks new)

updateStreams publicKey oldSubscriber newSubscriber streams =
  let allChunks = chunks newSubscriber in
  updateStreams' allChunks publicKey
  (L.get (forestTreeTrie . subscriberPublished) oldSubscriber)
  (L.get (forestTreeTrie . subscriberPublished) newSubscriber)
  $ updateStreams' allChunks publicKey
  (L.get (forestTreeTrie . subscriberPersisted) oldSubscriber)
  (L.get (forestTreeTrie . subscriberPersisted) newSubscriber)
  streams

updateStreams' allChunks publicKey oldTrees newTrees streams =
  foldr maybeAdd streams new where
    (_, new) = M.diff oldTrees newTrees

    maybeAdd new streams =
      if T.member
         (Path.super P.aclReaderKey $ P.singleton publicKey ())
         (Sub.getTreeACL new)
         && (not $ Sub.getTreeOptional new)
      then
        S.insert (Sub.getTreeStream new) streams
      else
        streams

filterDiff streams (obsoleteChunks, newChunks) =
  (T.foldrPathsAndValues maybeRemove obsoleteChunks obsoleteChunks,
   T.foldrPathsAndValues maybeRemove newChunks newChunks) where
    maybeRemove path chunk chunks =
      case chunkTreeStream chunk of
        Nothing -> chunks
        Just stream ->
          if S.member stream streams then
            chunks
          else
            T.subtract path chunks

chunkTreeStream = \case
  P.Leaf { P.getLeafTreeStream = treeStream } -> Just treeStream
  P.Group { P.getGroupTreeStream = treeStream } -> Just treeStream
  _ -> Nothing

receive = \case
  P.Cred version request publicKey signature ->
    if version != P.version then
      exception "unexpected protocol version: " ++ show version
    else do
      get relayChallenge >>= verify signature publicKey

      subscriber <- get relayPendingSubscriber

      State.set $ relay (L.set Pipe.pipeSubscriber subscriber Pipe.empty)
        (Just publicKey)

  nack@(P.Nack {}) -> do
    publicKey <- get relayPublicKey
    when (isJust publicKey)
      $ with (Pipe.pipePublisher . relayPipe)
      $ Pub.receive nack

  P.Persisted {} -> return ()

  -- todo: stream (authenticated) chunks to subscribers as they are
  -- received rather than wait until we have a complete tree

  -- todo: enforce bandwidth and storage limits for this forest
  -- stream.  Consider both soft and hard storage limits, where no new
  -- revision can exceed the soft limit and the combined size of all
  -- partial revisions and the latest full revision cannot exceed the
  -- hard limit.

  message -> do
    publicKey <- get relayPublicKey
    when (isJust publicKey)
      $ get (Pipe.pipeSubscriber . relayPipe) >>= \subscriber -> do

      subscriber' <- try $ exec (Sub.receive message) subscriber

      let revision = L.get (Sub.forestRevision . Sub.subscriberPublished)

      if revision subscriber' > revision subscriber then do
        return $ Just subscriber'
        else do
        set (Pipe.pipeSubscriber . relayPipe) subscriber'

        return Nothing

nextMessages now = do
  publicKey <- get relayPublicKey
  with relayPipe $ Pipe.nextMessages now case publicKey of
    Nothing ->
      ((:[]) . P.Challenge) <$> get relayChallenge

    Just _ -> do
      published <-
        P.Published <$> get
        (Sub.forestName . Sub.subscriberPublished . Pipe.pipeSubscriber
         . relayPipe)

      persisted <-
        -- todo: actually persist incoming revisions to durable
        -- storage and send P.Persisted messages only once they are
        -- safely stored
        P.Persisted <$> get
        (Sub.forestName . Sub.subscriberPublished . Pipe.pipeSubscriber
         . relayPipe)

      return [published, persisted]
