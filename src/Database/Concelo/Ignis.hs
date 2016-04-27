{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Concelo.Ignis
  ( Ignis()
  , ignis
  , initAdmin
  , Credentials(PrivateKey, EmailPassword)
  , update
  , receive
  , nextMessages
  , getHead
  , getPublishedTrie ) where

import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Pipe as Pi
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Serializer as Se
import qualified Database.Concelo.Deserializer as D
import qualified Database.Concelo.Subscriber as Su
import qualified Database.Concelo.Publisher as Pu
import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.ACL as ACL
import qualified Database.Concelo.Bytes as B
import qualified Database.Concelo.Rules as R
import qualified Database.Concelo.SyncTree as ST
import qualified Database.Concelo.Control as Co

import Database.Concelo.Control (get, set, with, lend, exception, run,
                                 eitherToAction)

import Data.Maybe (fromMaybe)
import Control.Monad (foldM)

data Ignis = Ignis { getIgnisPrivate :: Cr.PrivateKey
                   , getIgnisSentChallengeResponse :: Bool
                   , getIgnisChallenge :: Maybe BS.ByteString
                   , getIgnisHead :: VT.VTrie BS.ByteString Pr.Value
                   , getIgnisDiff :: (T.Trie BS.ByteString Pr.Value,
                                      T.Trie BS.ByteString Pr.Value)
                   , getIgnisPipe :: Pi.Pipe
                   , getIgnisSerializer :: Se.Serializer
                   , getIgnisDeserializer :: D.Deserializer
                   , getIgnisPRNG :: Cr.PRNG }

instance Show Ignis where
  show relay = concat ["ignis(", show $ getIgnisChallenge relay, ")"]

ignisPrivate =
  L.lens getIgnisPrivate (\x v -> x { getIgnisPrivate = v })

ignisSentChallengeResponse :: L.Lens' Ignis Bool
ignisSentChallengeResponse =
  L.lens getIgnisSentChallengeResponse
  (\x v -> x { getIgnisSentChallengeResponse = v })

ignisChallenge :: L.Lens' Ignis (Maybe BS.ByteString)
ignisChallenge =
  L.lens getIgnisChallenge (\x v -> x { getIgnisChallenge = v })

ignisHead :: L.Lens' Ignis (VT.VTrie BS.ByteString Pr.Value)
ignisHead =
  L.lens getIgnisHead (\x v -> x { getIgnisHead = v })

ignisDiff :: L.Lens' Ignis (T.Trie BS.ByteString Pr.Value,
                            T.Trie BS.ByteString Pr.Value)
ignisDiff =
  L.lens getIgnisDiff (\x v -> x { getIgnisDiff = v })

ignisPipe :: L.Lens' Ignis Pi.Pipe
ignisPipe =
  L.lens getIgnisPipe (\x v -> x { getIgnisPipe = v })

ignisSerializer :: L.Lens' Ignis Se.Serializer
ignisSerializer =
  L.lens getIgnisSerializer (\x v -> x { getIgnisSerializer = v })

ignisDeserializer :: L.Lens' Ignis D.Deserializer
ignisDeserializer =
  L.lens getIgnisDeserializer (\x v -> x { getIgnisDeserializer = v })

ignisPRNG :: L.Lens' Ignis Cr.PRNG
ignisPRNG =
  L.lens getIgnisPRNG (\x v -> x { getIgnisPRNG = v })

data Credentials = PrivateKey { _getPKPrivateKey :: BS.ByteString
                              , _getPKPassword :: BS.ByteString }

                 | EmailPassword { _getEPEmail :: BS.ByteString
                                 , _getEPPassword :: BS.ByteString }

ignis admins credentials stream seed =
  do
    let prng = Cr.makePRNG seed

    (private, prng') <- run (authenticate credentials) prng

    let adminTrie :: T.Trie BS.ByteString ()
        adminTrie = ACL.writerTrie admins
        permitAdmins = ACL.fromWhiteTrie adminTrie
        public = Cr.derivePublic private
        me = Cr.fromPublic public
        permitAdminsAndMe = ACL.whiteList ACL.aclWriteLists me
                            $ ACL.whiteList ACL.aclReadLists me permitAdmins
        ignis = Ignis
                private
                False
                Nothing
                VT.empty
                (T.empty, T.empty)
                (Pi.pipe adminTrie (Just public) stream)
                (Se.serializer private)
                (D.deserializer private permitAdmins permitAdminsAndMe)
                prng'

    return ignis

data SyncState a = SyncState

instance ST.Serializer a SyncState where
  serialize _ = return [BS.empty]
  encrypt = return . id

initAdmin readers writers = do
  private <- get ignisPrivate
  stream <- get (ignisPipe . Pi.pipeSubscriber . Su.subscriberStream)

  let toPath public = Pa.singleton (Cr.fromPublic public) ()
      writerTrie :: T.Trie BS.ByteString ()
      writerTrie = T.index toPath writers
      readerTrie :: T.Trie BS.ByteString ()
      readerTrie = foldr (T.union . toPath) writerTrie readers
      aclTrie = T.union (T.super ACL.writerKey writerTrie)
                (T.super ACL.readerKey readerTrie)

  ((_, aclBody, aclRoot), _) <-
    eitherToAction $ run (ST.visit ST.empty T.empty T.empty aclTrie) SyncState

  mapM_ ((receive =<<)
         . with ignisPRNG
         . ST.chunkToMessage private Pr.forestACLLevel BS.empty stream) aclBody

  let text = BS.concat [B.fromInteger (0 :: Int), Pr.serializeName aclRoot]

  signature <- with ignisPRNG $ Cr.sign private text

  forest <- with ignisPRNG $ Pr.forest
            private
            stream
            0
            0
            (Pr.Signed (Cr.derivePublic private) signature text)
            aclRoot
            (Pa.leaf ())

  receive forest

  receive $ Pr.Published (Pr.getForestName forest)

getHead = getIgnisHead

authenticate = \case
  PrivateKey key password -> Cr.decryptPrivate password key

  EmailPassword email password -> return $ Cr.derivePrivate password email

sign private = with ignisPRNG . Cr.sign private

maybeAuthenticate =
  get ignisSentChallengeResponse >>= \case
    True -> return $ Just []
    False -> get ignisChallenge >>= \case
      Nothing -> return Nothing
      Just challenge -> do
        private <- get ignisPrivate

        set ignisSentChallengeResponse True

        Just
          . (:[])
          . (Pr.Cred Pr.version $ Cr.fromPublic $ Cr.derivePublic private)
          <$> sign private challenge

getPublishedTrie = D.getDesSanitized . getIgnisDeserializer

getPublic = Cr.derivePublic <$> get ignisPrivate

update update atomicUpdate = do
  head <- update <$> get ignisHead

  makeDiff (atomicUpdate head) >>= set ignisDiff

  set ignisHead head

receive = \case
  Pr.Challenge { Pr.getChallengeProtocolVersion = v
               , Pr.getChallengeBody = body } -> do
    if v /= Pr.version then
      exception ("unexpected protocol version: " ++ show v)
      else do
      set ignisChallenge $ Just body

      set ignisSentChallengeResponse False

      maybeAuthenticate

    return False

  nack@(Pr.Nack {}) -> do
    updatePublisher

    with (ignisPipe . Pi.pipePublisher) $ Pu.receive (const True) nack

    return False

  message -> do
    old <- get (ignisPipe . Pi.pipeSubscriber . Su.subscriberPublished)

    with (ignisPipe . Pi.pipeSubscriber) $ Su.receive message

    new <- get (ignisPipe . Pi.pipeSubscriber . Su.subscriberPublished)

    let rev = Pr.getForestRevision . Su.getForestMessage

    if rev new /= rev old then do
      oldTrie <- get (ignisDeserializer . D.desSanitized)

      with ignisDeserializer $ D.deserialize old new

      newTrie <- get (ignisDeserializer . D.desSanitized)

      head <- get ignisHead

      set ignisHead $ VT.mergeL Pr.localVersion oldTrie newTrie head

      -- The following tells the caller that it should call "update"
      -- again (with the latest atomic updates, if any), which will
      -- ensure the latest diff is published to the server.  After
      -- that, the caller may query the latest published forest, head,
      -- and atomic head to determine which callbacks should be
      -- notified.
      return True
      else
      return False

nextMessages now =
  maybeAuthenticate >>= \case
    Nothing -> return []

    Just [] -> updatePublisher >>= \case
      True -> do
        ping <- (:[]) . Pr.Published
          <$> get (ignisPipe . Pi.pipePublisher . Pu.publisherPublished)

        with ignisPipe $ Pi.nextMessages now ping

      False -> return []

    Just messages -> return messages

updatePublisher = do
  published <- get (ignisPipe
                    . Pi.pipeSubscriber
                    . Su.subscriberPublished)

  let revision = 1 + (Pr.getForestRevision $ Su.getForestMessage published)

  if revision == 0 then
    -- we haven't received a complete, valid revision from the server
    -- yet, nor have we initialized the stream as an admin, but we
    -- need an admin-signed ACL to publish with, so we must wait until
    -- we receive one
    return False
    else do
    diff <- get ignisDiff

    rejected <- D.getDesRejected <$> get ignisDeserializer

    serialized <- lend ignisPRNG Se.serializerPRNG ignisSerializer
                  $ Se.serialize published rejected revision diff

    with (ignisPipe . Pi.pipePublisher) $ Pu.update serialized

    return True

makeDiff :: VT.VTrie BS.ByteString Pr.Value ->
            Co.Action Ignis (T.Trie BS.ByteString Pr.Value,
                             T.Trie BS.ByteString Pr.Value)
makeDiff head = do
  me <- getPublic

  deserializer <- get ignisDeserializer

  published <- get (ignisPipe . Pi.pipeSubscriber . Su.subscriberPublished)

  validateDiff
    me
    (Pr.getForestRevision $ Su.getForestMessage published)
    (D.getDesRules deserializer)
    (D.getDesDefaultACL deserializer)
    head
    (T.diff (D.getDesSanitized deserializer) head)

validateDiff :: Cr.PublicKey ->
                Integer ->
                R.Rules ->
                ACL.ACL ->
                VT.VTrie BS.ByteString Pr.Value ->
                (T.Trie BS.ByteString Pr.Value,
                 T.Trie BS.ByteString Pr.Value) ->
                Co.Action Ignis (T.Trie BS.ByteString Pr.Value,
                                 T.Trie BS.ByteString Pr.Value)
validateDiff me revision rules acl head (obsolete, new) =
  foldM (validate rules acl env union obsolete new root) (T.empty, T.empty)
  $ T.triples union where

    union = T.union obsolete new
    root = R.rootVisitor $ T.trie head
    env = M.empty

    validate rules acl env union obsolete new visitor
      (obsoleteResult, newResult) (k, v, _) =
      (case v of
          Nothing -> descend
          Just _ ->
            if valid then
              if ACL.isWriter me acl' then
                descend
              else
                exception "write access denied"
            else
              exception "invalid write") where

        (rules', wildcard) = R.subRules k rules

        env' = if BS.null wildcard then env else M.insert wildcard k env

        union' = T.sub k union

        obsolete' = T.sub k obsolete

        new' = T.sub k new

        visitor' = R.visitorChild k visitor

        context = R.context revision env' root visitor'

        (readACL, _) = R.getRulesRead rules' context acl

        (acl', _) = R.getRulesWrite rules' context readACL

        (valid, _) = fromMaybe (True, T.empty)
                     (const (R.getRulesValidate rules' context) <$> v)

        obsoleteValue = T.value obsolete'

        newValue = (L.set Pr.valueACL acl') <$> T.value new'

        descend = do
          (obsoleteSubs, newSubs) <-
            foldM (validate rules' acl' env' union' obsolete' new' visitor')
            (T.empty, T.empty) $ T.triples union'

          return
            (T.union (T.superValue obsoleteValue k obsoleteSubs)
             obsoleteResult,

             T.union (T.superValue newValue k newSubs)
             newResult)
