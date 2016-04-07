{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Concelo.Relay
  ( relay
  , receive
  , nextMessages
  , setSubscriber ) where

import Database.Concelo.Control (set, get, getThenSet, updateThenGet,
                                 exception, with, exec, eitherToAction)

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
        , getRelayPendingSubscriber :: Su.Subscriber
        , getRelayStreams :: Se.Set BS.ByteString
        , getRelayPublicKey :: Maybe C.PublicKey
        , getRelayChallenge :: BS.ByteString }

relayPipe :: L.Lens' Relay Pi.Pipe
relayPipe =
  L.lens getRelayPipe (\x v -> x { getRelayPipe = v })

relayPendingSubscriber :: L.Lens' Relay Su.Subscriber
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

relay adminACL stream challenge =
  make (Pi.pipe adminACL Nothing stream) Nothing challenge

make pipe publicKey challenge =
  Relay pipe (Pi.getPipeSubscriber pipe) Se.empty publicKey challenge

chunks = Su.getSubscriberReceived . Su.getSubscriberClean

setSubscriber new = do
  set relayPendingSubscriber new

  get relayPublicKey >>= \case
    Nothing -> return ()

    Just publicKey -> do
      previous <- getThenSet (relayPipe . Pi.pipeSubscriber) new

      streams <-
        updateThenGet relayStreams
        $ updateStreams publicKey previous new

      with (relayPipe . Pi.pipePublisher)
        $ Pu.update
        $ filterDiff streams
        $ T.diff (chunks previous) (chunks new)

updateStreams publicKey oldSubscriber newSubscriber =
  updateStreams' publicKey
  (Su.getForestTreeMap $ Su.getSubscriberPublished oldSubscriber)
  (Su.getForestTreeMap $ Su.getSubscriberPublished newSubscriber)
  . updateStreams' publicKey
  (Su.getForestTreeMap $ Su.getSubscriberPersisted oldSubscriber)
  (Su.getForestTreeMap $ Su.getSubscriberPersisted newSubscriber)

updateStreams' publicKey oldTrees newTrees streams =
  foldr maybeAdd streams new where
    (_, new) = M.diff oldTrees newTrees

    maybeAdd new streams =
      -- todo: allow client to subscribe to optional tree streams
      if T.member
         (Pa.super Pr.aclReaderKey $ Pa.singleton (C.fromPublic publicKey) ())
         (Su.getTreeACLTrie new)
         && (not $ Su.getTreeOptional new)
      then
        Se.insert (Su.getTreeStream new) streams
      else
        streams

filterDiff streams (obsoleteChunks, newChunks) =
  (T.foldrPathsAndValues maybeRemove obsoleteChunks obsoleteChunks,
   T.foldrPathsAndValues maybeRemove newChunks newChunks) where
    maybeRemove (path, chunk) chunks =
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
  Pr.Cred version publicKey signature ->
    if version /= Pr.version then
      exception ("unexpected protocol version: " ++ show version)
    else do
      challenge <- get relayChallenge
      public <- eitherToAction $ C.toPublic publicKey

      when (not $ C.verify public signature challenge)
        $ exception "received improperly signed challenge"

      subscriber <- get relayPendingSubscriber

      St.put $ make (Pi.fromSubscriber subscriber) (Just public) challenge
      return Nothing

  nack@(Pr.Nack {}) -> do
    publicKey <- get relayPublicKey
    streams <- get relayStreams

    when (isJust publicKey)
      $ with (relayPipe . Pi.pipePublisher)
      $ Pu.receive (flip Se.member streams) nack

    return Nothing

  Pr.Persisted {} -> return Nothing

  -- todo: stream (authenticated) chunks to subscribers as they are
  -- received rather than wait until we have a complete tree

  -- todo: enforce bandwidth and storage limits for this forest
  -- stream.  Consider both soft and hard storage limits, where no new
  -- revision can exceed the soft limit and the combined size of all
  -- partial revisions and the latest full revision cannot exceed the
  -- hard limit.

  message ->
    get relayPublicKey >>= \case
      Just _ -> do
        subscriber <- get (relayPipe . Pi.pipeSubscriber)

        subscriber' <- eitherToAction $ exec (Su.receive message) subscriber

        let revision = Su.getForestRevision . Su.getSubscriberPublished

        if revision subscriber' > revision subscriber then do
          return $ Just subscriber'
          else do
          set (relayPipe . Pi.pipeSubscriber) subscriber'

          return Nothing

      Nothing ->
        return Nothing

nextMessages now = do
  ping <- get relayPublicKey >>= \case
    Nothing ->
      ((:[]) . Pr.Challenge Pr.version) <$> get relayChallenge

    Just _ -> do
      let lens = relayPipe . Pi.pipeSubscriber . Su.subscriberPublished
                 . Su.forestName

      published <- Pr.Published <$> get lens

      -- todo: actually persist incoming revisions to durable storage
      -- and send Pr.Persisted messages only once they are safely
      -- stored
      persisted <- Pr.Persisted <$> get lens

      return [published, persisted]

  with relayPipe $ Pi.nextMessages now ping
