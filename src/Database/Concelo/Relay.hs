{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Relay
  ( empty
  , receive
  , nextMessages
  , setSubscriber ) where

import Database.Concelo.Control (set, get, getThenSet, updateThenGet, update,
                                 exception, with, try, exec)

import Control.Monad (when)
import Data.Maybe (isJust)

import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Set as Se
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Publisher as Pu
import qualified Database.Concelo.Subscriber as Su
import qualified Database.Concelo.Pipe as Pi
import qualified Database.Concelo.Crypto as C
import qualified Control.Lens as L
import qualified Control.Monad.State as St
import qualified Data.ByteString as BS

data Relay =
  Relay { getRelayPipe :: Pi.Pipe
        , getRelayPendingSubscriber :: Maybe Su.Subscriber
        , getRelayStreams :: Se.Set BS.ByteString
        , getRelayPublicKey :: Maybe BS.ByteString
        , getRelayChallenge :: BS.ByteString }

relayPipe =
  L.lens getRelayPipe (\x v -> x { getRelayPipe = v })

relayPendingSubscriber =
  L.lens getRelayPendingSubscriber
  (\x v -> x { getRelayPendingSubscriber = v })

relayStreams :: L.Lens' Relay (Se.Set BS.ByteString)
relayStreams =
  L.lens getRelayStreams (\x v -> x { getRelayStreams = v })

relayPublicKey =
  L.lens getRelayPublicKey (\x v -> x { getRelayPublicKey = v })

relayChallenge =
  L.lens getRelayChallenge (\x v -> x { getRelayChallenge = v })

relay adminACL stream = make (Pi.pipe adminACL Nothing stream) Nothing

make pipe publicKey =
  Relay pipe Nothing Se.empty publicKey

chunks = Su.getSubscriberReceived . Su.getSubscriberClean

setSubscriber new = do
  set relayPendingSubscriber $ Just new

  get relayPublicKey >>= \case
    Nothing -> return ()

    Just publicKey -> do
      previous <- getThenSet (relayPipe . Pi.pipeSubscriber) new

      streams <-
        updateThenGet relayStreams
        $ updateStreams publicKey previous new

      update (Pi.pipePublisher . relayPipe)
        $ Pu.update
        $ filterDiff streams
        $ T.diff (chunks previous) (chunks new)

updateStreams publicKey oldSubscriber newSubscriber streams =
  let allChunks = chunks newSubscriber in
  updateStreams' allChunks publicKey
  (Su.getForestTreeMap $ Su.getSubscriberPublished oldSubscriber)
  (Su.getForestTreeMap $ Su.getSubscriberPublished newSubscriber)
  $ updateStreams' allChunks publicKey
  (Su.getForestTreeMap $ Su.getSubscriberPersisted oldSubscriber)
  (Su.getForestTreeMap $ Su.getSubscriberPersisted newSubscriber)
  streams

updateStreams' allChunks publicKey oldTrees newTrees streams =
  foldr maybeAdd streams new where
    (_, new) = M.diff oldTrees newTrees

    maybeAdd new streams =
      if T.member
         (Pa.super Pr.aclReaderKey $ Pa.singleton publicKey ())
         (Su.getTreeACLTrie new)
         && (not $ Su.getTreeOptional new)
      then
        Se.insert (Su.getTreeStream new) streams
      else
        streams

filterDiff streams (obsoleteChunks, newChunks) =
  (T.foldrPathsAndValues maybeRemove obsoleteChunks obsoleteChunks,
   T.foldrPathsAndValues maybeRemove newChunks newChunks) where
    maybeRemove path chunk chunks =
      case chunkTreeStream chunk of
        Nothing -> chunks
        Just stream ->
          if Se.member stream streams then
            chunks
          else
            T.subtract path chunks

chunkTreeStream = \case
  Pr.Leaf { Pr.getLeafTreeStream = treeStream } -> Just treeStream
  Pr.Group { Pr.getGroupTreeStream = treeStream } -> Just treeStream
  _ -> Nothing

receive = \case
  Pr.Cred version request publicKey signature ->
    if version /= Pr.version then
      exception "unexpected protocol version: " ++ show version
    else do
      challenge <- get relayChallenge

      when (not $ C.verify publicKey signature challenge)
        $ exception "received improperly signed challenge"

      subscriber <- get relayPendingSubscriber

      St.put $ make (Pi.fromSubscriber subscriber) (Just publicKey)

  nack@(Pr.Nack {}) -> do
    publicKey <- get relayPublicKey
    when (isJust publicKey)
      $ with (Pi.pipePublisher . relayPipe)
      $ Pu.receive nack

  Pr.Persisted {} -> return ()

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
      $ get (Pi.pipeSubscriber . relayPipe) >>= \subscriber -> do

      subscriber' <- try $ exec (Su.receive message) subscriber

      let revision = Su.getForestRevision . Su.getSubscriberPublished

      if revision subscriber' > revision subscriber then do
        return $ Just subscriber'
        else do
        set (Pi.pipeSubscriber . relayPipe) subscriber'

        return Nothing

nextMessages now = do
  publicKey <- get relayPublicKey
  with relayPipe $ Pi.nextMessages now $ case publicKey of
    Nothing ->
      ((:[]) . Pr.Challenge) <$> get relayChallenge

    Just _ -> do
      published <-
        Pr.Published <$> get
        (Su.forestName . Su.subscriberPublished . Pi.pipeSubscriber
         . relayPipe)

      persisted <-
        -- todo: actually persist incoming revisions to durable
        -- storage and send Pr.Persisted messages only once they are
        -- safely stored
        Pr.Persisted <$> get
        -- yes, this should be Su.subscriberPublished, not
        -- Su.subscriberPersisted:
        (Su.forestName . Su.subscriberPublished . Pi.pipeSubscriber
         . relayPipe)

      return [published, persisted]
