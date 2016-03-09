module Database.Concelo.Ignis
  ( ignis ) where

import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Database.Concelo.Crypto as C
import qualified Database.Concelo.Protocol as Pro
import qualified Database.Concelo.Revision as R
import qualified Database.Concelo.Pipe as Pipe
import qualified Database.Concelo.Serializer as Ser
import qualified Database.Concelo.Deserializer as Des
import qualified Database.Concelo.Path as Path
import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.Trie as T

import Database.Concelo.Control (get, set, getThenUpdate, with, bindMaybe,
                                 bindMaybe2, exception)

-- todo: use opaque data structures to represent keys and keypairs, so
-- the crypto library doesn't have to convert between ByteStrings and
-- its preferred model for every operation
data Cred = Cred { getCredPrivate :: BS.ByteString
                 , getCredPublic :: BS.ByteString
                 , getCredRequest :: Int
                 , getCredSent :: Bool }

data Ignis = Ignis { getIgnisCred :: Maybe Cred
                   , getIgnisChallenge :: Maybe BS.ByteString
                   , getIgnisNextRequest :: Int
                   , getIgnisHead :: R.Revision
                   , getIgnisDiff :: R.ValueTrie
                   , getIgnisPipe :: Pipe.Pipe
                   , getIgnisSerializer :: Ser.Serializer
                   , getIgnisDeserializer :: Des.Deserializer
                   , getIgnisPRNG :: C.PRNG }

ignisCred =
  L.lens getIgnisCred (\x v -> x { getIgnisCred = v })

ignisChallenge =
  L.lens getIgnisChallenge (\x v -> x { getIgnisChallenge = v })

ignisNextRequest =
  L.lens getIgnisNextRequest (\x v -> x { getIgnisNextRequest = v })

ignisHead =
  L.lens getIgnisHead (\x v -> x { getIgnisHead = v })

ignisDiff =
  L.lens getIgnisDiff (\x v -> x { getIgnisDiff = v })

ignisPipe =
  L.lens getIgnisPipe (\x v -> x { getIgnisPipe = v })

ignisSerializer =
  L.lens getIgnisSerializer (\x v -> x { getIgnisSerializer = v })

ignisDeserializer =
  L.lens getIgnisDeserializer (\x v -> x { getIgnisDeserializer = v })

ignisPRNG =
  L.lens getIgnisPRNG (\x v -> x { getIgnisPRNG = v })

data Credentials = PrivateKey { getPKPrivateKey :: BS.ByteString
                              , getPKPassword :: BS.ByteString }

                 | EmailPassword { getEPEmail :: BS.ByteString
                                 , getEPPassword :: BS.ByteString }

ignis seed = Ignis Nothing 1 (C.makePRNG seed)

authenticate = \case
  PrivateKey key password ->
    authenticateWithPrivateKey $ C.decryptPrivate password key

  EmailPassword email password ->
    authenticateWithPrivateKey $ C.deriveKey password email

authenticateWithPrivateKey private = do
  request <- get ignisNextRequest

  set ignisCred $ Just
    $ Cred private (C.derivePublic private) Nothing request False

  nextRequest

nextRequest = getThenUpdate ignisNextRequest (+1)

sign private challenge = with ignisPRNG $ C.sign private challenge

maybeAuthenticate =
  bindMaybe2 (get ignisCred) (get ignisChallenge) where
    try cred challenge =
      if getCredSent cred then
        return $ Just []
      else do
        set ignisCred $ Just $ L.set credSent True cred

        sign (getCredPrivate cred) challenge
          >>= Just
          [P.Cred P.version (getCredRequest cred) (getCredPublic cred)]

getPublic = bindMaybe getCredPublic $ get ignisCred

unAuth = set ignisCred Nothing

update now update atomicUpdate = do
  head <- update <$> L.set revisionUpdateTime now <$> get ignisHead

  makeDiff (atomicUpdate head) >>= set ignisDiff

  set ignisHead head

receive = \case
  P.Challenge { P.getChallengeProtocolVersion = v
              , P.getChallengeBody = body } -> do
    if v /= P.version then
      exception "unexpected protocol version: " ++ show v
      else do
      set ignisChallenge $ Just body

      bindMaybe (set ignisCred . Just . L.set credSent False) (get ignisCred)

      maybeAuthenticate

    return False

  nack@(P.Nack {}) -> do
    updatePublisher

    with (Pipe.pipePublisher . ignisPipe) $ Pub.receive (const True) nack

    return False

  message -> do
    let published = Sub.subscriberPublished . Pipe.pipeSubscriber . ignisPipe

    old <- get published

    with (Pipe.pipeSubscriber . ignisPipe) $ Sub.receive message

    new <- get published

    if Sub.getForestRevision new /= Sub.getForestRevision old then do
      oldTrie <- get (Des.deserializerTrie . ignisDeserializer)

      with ignisDeserializer $ Des.deserialize old new

      newTrie <- get (Des.deserializerTrie . ignisDeserializer)

      update ignisHead $ VT.mergeL P.localVersion oldTrie newTrie

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

    Just [] -> do
      updatePublisher

      with ignisPipe
      $ Pipe.nextMessages now
      ((:[]) . P.Published
       <$> get (Pub.publisherPublished . Pipe.pipePublisher . ignisPipe))

    Just messages -> return messages

updatePublisher = do
  diff <- get ignisDiff

  serialized <- with ignisSerializer $ Ser.serialize diff

  with (Pipe.pipePublisher . ignisPipe) $ Pub.update serialized

whoAmI = fromMaybe $ error "I don't know who I am!"

makeDiff head = do
  me <- (fmap credPublic <$> get ignisCred) >>= whoAmI

  base <- get (Des.deserializerTrie . ignisDeserializer)

  published <- get (Sub.subscriberPublished . Pipe.pipeSubscriber . ignisPipe)

  validateDiff
    me
    (Sub.getForestRevision published)
    (Sub.getForestRules published)
    (Sub.getForestPermitNone published)
    head
    (VT.diff base head)

validateDiff me revision rules acl head (obsolete, new) =
  foldM (validate rules acl env union obsolete new root) (T.empty, T.empty)
  $ T.triples union where

    union = T.union obsolete new
    root = R.rootVisitor head
    env = M.empty

    validate rules acl env union obsolete new visitor (k, v, sub)
      (obsoleteResult, newResult) =
      (case v of
          Nothing -> descend
          Just value ->
            if valid then
              if ACL.isWriter me acl' then
                descend
              else
                exception "write access denied"
            else
              exception "invalid write") where

        (rules', wildcard) = R.subRules k rules

        env' = if null wildcard then env else M.insert wildcard k env

        union' = T.sub k union

        obsolete' = T.sub k obsolete

        new' = T.sub k new

        visitor' = R.visitorChild k visitor

        context = R.Context T.empty revision env' root visitor' T.empty

        (readACL, _) = R.getRulesRead rules' context acl

        (acl', _) = R.getRulesWrite rules' context readACL

        (valid, _) = fromMaybe (True, T.empty)
                     (const (R.getRulesValidate rules' context) <$> v)

        (obsoleteSubs, newSubs)
          = foldM (validate rules' acl' env' union' obsolete' new' visitor')
            (T.empty, T.empty) $ T.triples union'

        obsoleteValue = T.value obsolete'

        newValue = (L.set P.valueACL acl') <$> T.value new'

        descend =
          (T.union (T.superValue obsoleteValue k obsoleteSubs) obsoleteResult
           T.union (T.superValue newValue k newSubs) newResult)
