{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Concelo.Relay
  ( Relay()
  , relay
  , getRelayPRNG
  , getRelayChallenge
  , receive
  , nextMessages
  , setSubscriber
  , initAdmin ) where

import Database.Concelo.Control (set, get, getThenSet, updateThenGet,
                                 exception, with, exec, eitherToAction, run)

import Control.Monad (when, foldM)
import Data.Maybe (isJust, fromJust)

-- import Debug.Trace

import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Set as Se
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Publisher as Pu
import qualified Database.Concelo.Subscriber as Su
import qualified Database.Concelo.Pipe as Pi
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.ACL as ACL
import qualified Database.Concelo.SyncTree as ST
import qualified Database.Concelo.Bytes as B
import qualified Control.Lens as L
import qualified Control.Monad.State as St
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Base16 as B16

data Relay =
  Relay { getRelayPRNG :: C.PRNG
        , getRelayPipe :: Pi.Pipe
        , getRelayPendingSubscriber :: Su.Subscriber
        , getRelayStreams :: Se.Set BS.ByteString
        , getRelayPublicKey :: Maybe C.PublicKey
        , getRelayChallenge :: BS.ByteString }

instance Show Relay where
  show relay = concat ["relay(", show $ getRelayChallenge relay, ")"]

relayPRNG :: L.Lens' Relay C.PRNG
relayPRNG =
  L.lens getRelayPRNG (\x v -> x { getRelayPRNG = v })

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

relay prng admins stream challenge =
  make prng (Pi.pipe (ACL.writerTrie admins) Nothing stream) Nothing challenge

make prng pipe publicKey challenge =
  Relay prng pipe (Pi.getPipeSubscriber pipe) Se.empty publicKey challenge

data SyncState a = SyncState { getSyncStatePRNG :: C.PRNG }

syncStatePRNG :: L.Lens' (SyncState a) C.PRNG
syncStatePRNG =
    L.lens getSyncStatePRNG (\x v -> x { getSyncStatePRNG = v })

instance ST.Serializer a SyncState where
  serialize trie = return $ Pr.serializeNames $ fmap (const ()) trie
  makeId = with syncStatePRNG $ C.randomBytes ST.idSize
  encrypt = return

trieToMessages private trie level = do
  stream <- get (relayPipe . Pi.pipeSubscriber . Su.subscriberStream)

  prng <- get relayPRNG

  ((_, body, root), SyncState prng') <-
    eitherToAction $ run (ST.visit ST.empty T.empty T.empty trie)
    (SyncState prng)

  set relayPRNG prng'

  -- traceM ("root is " ++ show root ++ "; body is " ++ show body ++ "; trie is " ++ show trie)

  messages <-
    foldM (\result chunk -> do
              message <- with relayPRNG
                (ST.chunkToMessage private level BS.empty stream chunk)

              return $ T.union (const message <$> Pr.name message) result)
    T.empty body

  (,messages) <$> maybe (return $ Pa.leaf ())
    ((Pr.name <$>)
     . with relayPRNG
     . ST.chunkToMessage private level BS.empty stream)
    root

initAdmin private readers writers = do
  stream <- get (relayPipe . Pi.pipeSubscriber . Su.subscriberStream)

  let toPath public = Pa.singleton (C.fromPublic public) ()
      writerTrie :: T.Trie BS.ByteString ()
      writerTrie = T.index toPath writers
      readerTrie :: T.Trie BS.ByteString ()
      readerTrie = foldr (T.union . toPath) writerTrie readers
      aclTrie = T.union (T.super ACL.writerKey writerTrie)
                (T.super ACL.readerKey readerTrie)

  (aclRoot, aclMessages) <- trieToMessages private aclTrie Pr.forestACLLevel

  let text = BS.concat [B.fromInteger (0 :: Int), Pr.serializeName aclRoot]

  signature <- with relayPRNG $ C.sign private text

  forest <- with relayPRNG $ Pr.forest
            private
            stream
            0
            0
            (Pr.Signed (C.derivePublic private) signature text)
            aclRoot
            (Pa.leaf ())

  let forestName = Pr.name forest
      messages = T.union (const forest <$> forestName) aclMessages

  with (relayPipe . Pi.pipePublisher)
    $ Pu.update forestName (T.empty, messages)

  mapM_ receiveDirect messages

  receiveDirect (Pr.Published (Pr.getForestName forest))
    >>= setSubscriber . fromJust

setSubscriber new = do
  set relayPendingSubscriber new

  get relayPublicKey >>= \case
    Nothing -> return ()

    Just publicKey -> do
      previous <- getThenSet (relayPipe . Pi.pipeSubscriber) new

      streams <-
        updateThenGet relayStreams
        $ updateStreams publicKey previous new

      -- let (obsM, newM) = T.diff (Su.getSubscriberClean previous) (Su.getSubscriberClean new)

      -- traceM ("relay obs: " ++ show obsM)
      -- traceM ("relay new: " ++ show newM)

      -- traceM ("streams: " ++ show streams)

      -- let (obsMF, newMF) = filterDiff streams (obsM, newM)

      -- traceM ("relay obs filtered: " ++ show obsMF)
      -- traceM ("relay new filtered: " ++ show newMF)

      with (relayPipe . Pi.pipePublisher)
        $ Pu.update (Pr.getForestName
                     $ Su.getForestMessage
                     $ Su.getSubscriberPublished new)
        $ filterDiff streams
        $ T.diff (Su.getSubscriberClean previous) (Su.getSubscriberClean new)

updateStreams publicKey oldSubscriber newSubscriber =
  updateStreams' publicKey
  (Su.getForestTreeMap $ Su.getSubscriberPublished oldSubscriber)
  (Su.getForestTreeMap $ Su.getSubscriberPublished newSubscriber)
  . updateStreams' publicKey
  (Su.getForestTreeMap $ Su.getSubscriberPersisted oldSubscriber)
  (Su.getForestTreeMap $ Su.getSubscriberPersisted newSubscriber)

updateStreams' publicKey oldTrees newTrees streams =
  -- trace ("diffed " ++ show (Su.getTreeMessage <$> oldTrees)
  --        ++ " with " ++ show (Su.getTreeMessage <$> newTrees)
  --        ++ " result: " ++ show (Su.getTreeMessage <$> new))
  foldr maybeAdd streams new where
    (_, new) = M.diff oldTrees newTrees

    maybeAdd new streams =
      -- trace ("is " ++ show (B16.encode $ BS.take 4 $ C.fromPublic publicKey) ++ " in " ++ show (Su.getTreeACLTrie new) ++ "? (optional: " ++ show (Pr.getTreeOptional $ Su.getTreeMessage new) ++ ")") $
      -- todo: allow client to subscribe to optional tree streams
      if T.member
         (Pa.super Pr.aclReaderKey $ Pa.singleton (C.fromPublic publicKey) ())
         (Su.getTreeACLTrie new)
         && (not $ Pr.getTreeOptional $ Su.getTreeMessage new)
      then
        Se.insert (Pr.getTreeStream $ Su.getTreeMessage new) streams
      else
        streams

filterDiff streams (obsoleteChunks, newChunks) =
  (T.foldrPathsAndValues maybeRemove obsoleteChunks obsoleteChunks,
   T.foldrPathsAndValues maybeRemove newChunks newChunks) where
    maybeRemove (path, chunk) chunks =
      case chunkTreeStream chunk of
        Nothing -> chunks
        Just stream ->
          if BS.null stream || Se.member stream streams then
            chunks
          else
            T.subtract path chunks

chunkTreeStream = \case
  Pr.Leaf { Pr.getLeafTreeStream = treeStream } -> Just treeStream
  Pr.Group { Pr.getGroupTreeStream = treeStream } -> Just treeStream
  _ -> Nothing

receive = \case -- m = traceM ("relay receive " ++ show m) >> case m of
  Pr.Cred version publicKey signature ->
    if version /= Pr.version then
      exception ("unexpected protocol version: " ++ show version)
    else do
      challenge <- get relayChallenge
      public <- eitherToAction $ C.toPublic publicKey

      when (not $ C.verify public signature challenge)
        $ exception "received improperly signed challenge"

      prng <- get relayPRNG
      subscriber <- get relayPendingSubscriber

      St.put $ make prng (Pi.fromSubscriber subscriber) (Just public) challenge
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
      Just _ -> receiveDirect message
      Nothing -> return Nothing

receiveDirect message = do
  subscriber <- get (relayPipe . Pi.pipeSubscriber)

  subscriber' <- eitherToAction $ exec (Su.receive message) subscriber

  let revision = Pr.getForestRevision
                 . Su.getForestMessage
                 . Su.getSubscriberPublished

  if revision subscriber' > revision subscriber then do
    return $ Just subscriber'
    else do
    set (relayPipe . Pi.pipeSubscriber) subscriber'

    return Nothing

nextMessages now = do
  ping <- get relayPublicKey >>= \case
    Nothing ->
      (:[]) . Pr.Challenge Pr.version <$> get relayChallenge

    Just _ -> do
      let lens = relayPipe . Pi.pipeSubscriber . Su.subscriberPublished

      -- todo: actually persist incoming revisions to durable storage
      -- and send Pr.Persisted messages only once they are safely
      -- stored; until then, we just pretend the currently published
      -- revision has been persisted

      published <- Pr.getForestName . Su.getForestMessage <$> get lens

      -- traceM ("published is " ++ show published)

      return $ if null (Pa.keys published) then [] else
                 [Pr.Published published, Pr.Persisted published]

  with relayPipe $ Pi.nextMessages now ping
