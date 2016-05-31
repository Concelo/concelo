{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Database.Concelo.Simulation
  ( runTests ) where

import Database.Concelo.Control (get, run, exec, update, Exception(Success),
                                 updateM, eitherToAction, overM, set,
                                 exception, eval, patternFailure, bsShow)

import Data.List (inits)
import Debug.Trace

import qualified Database.Concelo.VTrie as VT
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Relay as R
import qualified Database.Concelo.Ignis as I
import qualified Database.Concelo.Crypto as Cr
import qualified Database.Concelo.Control as Co
import qualified Control.Monad.State as St
import qualified Control.Monad.Except as E
import qualified Control.Monad as M
import qualified Data.ByteString as BS
import qualified Data.Sequence as Q
import qualified Test.QuickCheck as QC
import qualified Control.Lens as L

instance QC.Arbitrary Pr.ValueBody where
  arbitrary = QC.oneof [ return Pr.NullBody
                       , Pr.NumberBody <$> QC.arbitrary
                       , Pr.StringBody <$> QC.arbitrary
                       , Pr.BooleanBody <$> QC.arbitrary ]

  shrink v = Pr.NullBody : case v of
    Pr.NullBody -> []
    Pr.NumberBody n -> Pr.NumberBody <$> QC.shrink n
    Pr.StringBody s -> Pr.StringBody <$> QC.shrink s
    Pr.BooleanBody b -> Pr.BooleanBody <$> QC.shrink b

instance QC.Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> QC.arbitrary

  shrink = fmap BS.pack <$> QC.shrink . BS.unpack

instance QC.Arbitrary Pr.Value where
  arbitrary = Pr.value Pr.defaultPriority <$> QC.arbitrary

  shrink v = Pr.value Pr.defaultPriority <$> QC.shrink (Pr.getValueBody v)

instance QC.Arbitrary (Pa.Path BS.ByteString Pr.Value) where
  arbitrary = Pa.toPath <$> QC.arbitrary <*> QC.arbitrary

  shrink p = zipWith Pa.toPath (QC.shrink $ Pa.keys p) (QC.shrink $ Pa.value p)

data Client = Client { getClientId :: BS.ByteString
                     , getClientIsWriter :: Bool
                     , getClientRelay :: R.Relay
                     , getClientIgnis :: I.Ignis }
              deriving (Show)

-- clientId :: L.Lens' Client BS.ByteString
-- clientId =
--   L.lens getClientId (\x v -> x { getClientId = v })

-- clientIsWriter :: L.Lens' Client Bool
-- clientIsWriter =
--   L.lens getClientIsWriter (\x v -> x { getClientIsWriter = v })

clientRelay :: L.Lens' Client R.Relay
clientRelay =
  L.lens getClientRelay (\x v -> x { getClientRelay = v })

clientIgnis =
  L.lens getClientIgnis (\x v -> x { getClientIgnis = v })

data MessageType = ToRelay | ToIgnis deriving (Show)

data Message = Message { _getMessageClientId :: BS.ByteString
                       , _getMessageType :: MessageType
                       , _getMessageValue :: Pr.Message }
               deriving (Show)

data State = State { getStateMessages :: Q.Seq Message
                   , getStateClients :: [Client]
                   , getStateNow :: Integer
                   , getStateTasks :: [Task]
                   , getStateTries :: [VT.VTrie BS.ByteString Pr.Value]
                   , getStatePublished :: VT.VTrie BS.ByteString Pr.Value }

instance Show State where
  show s = concat ["state(",
                   show $ getStateMessages s, " ",
                   show $ getStateClients s, " ",
                   show $ getStateTries s, " ",
                   show $ getStateNow s, ")"]

stateMessages :: L.Lens' State (Q.Seq Message)
stateMessages =
  L.lens getStateMessages (\x v -> x { getStateMessages = v })

stateClients :: L.Lens' State [Client]
stateClients =
  L.lens getStateClients (\x v -> x { getStateClients = v })

stateNow :: L.Lens' State Integer
stateNow =
  L.lens getStateNow (\x v -> x { getStateNow = v })

stateTasks :: L.Lens' State [Task]
stateTasks =
  L.lens getStateTasks (\x v -> x { getStateTasks = v })

stateTries :: L.Lens' State [VT.VTrie BS.ByteString Pr.Value]
stateTries =
  L.lens getStateTries (\x v -> x { getStateTries = v })

statePublished :: L.Lens' State (VT.VTrie BS.ByteString Pr.Value)
statePublished =
  L.lens getStatePublished (\x v -> x { getStatePublished = v })

deliver (Message clientId type_ value) =
  (filter ((clientId ==) . getClientId) <$> get stateClients) >>= \case
    [client] -> case type_ of
      ToRelay -> do
        (newSubscriber, relay) <-
          eitherToAction $ run (R.receive value) (getClientRelay client)

        updateM stateClients $ \clients ->
          let clients' = replaceClient (L.set clientRelay relay client) clients
          in case newSubscriber of
            Just sub ->
              mapM (overM clientRelay
                    (eitherToAction . exec (R.setSubscriber sub))) clients'
            Nothing ->
              return clients'

      ToIgnis -> do
        ignis <- eitherToAction
                 $ exec (I.receive value) (getClientIgnis client)
        update stateClients $ replaceClient $ L.set clientIgnis ignis client

    _ -> return ()

getClient which predicate =
  choose which . filter predicate <$> get stateClients

choose which xs = pick index xs where
  count = length xs
  index = clamp (count - 1) (floor ((fromIntegral count) * which))

  clamp limit value = if value > limit then limit else value

  pick index = \case
    [] -> Nothing
    (x:xs) -> if index <= 0 then Just x else pick (index - 1) xs

removeClient client = filter ((getClientId client /=) . getClientId)

replaceClient client = (client :) . removeClient client

consistent (State { getStateClients = clients
                  , getStatePublished = published }) =
  foldr check True clients where
    check client = -- trace ("published " ++ show published ++ "\nclient published " ++ show (I.getPublishedTrie (getClientIgnis client))) .
      ((published == I.getPublishedTrie (getClientIgnis client)) &&)

type WhichClient = Double

data Task = SendFirst
          | SendLast
          | DropFirst
          | Flush [Task]
          | Reconnect [Task] WhichClient
          | Publish [Task] WhichClient
          | QueueMessages WhichClient

instance Show Task where
  show = \case
    SendFirst -> "SendFirst"
    SendLast -> "SendLast"
    DropFirst -> "DropFirst"
    Flush {} -> "Flush"
    Reconnect {} -> "Reconnect"
    Publish {} -> "Publish"
    QueueMessages {} -> "QueueMessages"

arbitraryTries = next 0 VT.empty where
  next revision trie = do
    let paths = VT.paths trie
        pathCount = length paths

    deleteCount <- QC.choose (0, pathCount)
    toDelete <- take deleteCount <$> QC.shuffle paths

    addCount <- QC.choose (0, pathCount + 10)
    toAdd <- QC.vectorOf addCount
             (QC.arbitrary :: QC.Gen (Pa.Path BS.ByteString Pr.Value))

    let trie' = foldr (VT.union revision)
                (foldr (VT.subtract revision) trie toDelete) toAdd

    (trie':) <$> next (revision + 1) trie'

arbitraryTasks =
  QC.infiniteListOf
  (QC.frequency [ (40, QueueMessages <$> QC.choose (0, 1.0))

                , (40, return SendFirst)

                , (10, Flush <$> arbitraryTasks)

                , (10, Publish <$> arbitraryTasks <*> QC.choose (0, 1.0))

                , ( 0{-5-}, return SendLast)

                , ( 0{-5-}, return DropFirst)

                , ( 0{-1-}, do delay <- QC.choose (10, 1000)
                               tasks <- take delay <$> arbitraryTasks
                               Reconnect tasks <$> QC.choose (0, 1.0)) ])

stream = "my stream"

admin = makeClient True "0"

adminPublicKeys = [Cr.fromPublic
                   $ Cr.derivePublic
                   $ I.getIgnisPrivate
                   $ getClientIgnis admin]

makeClient writer id =
  case (Client id writer (R.relay (Cr.makePRNG id) adminPublicKeys stream id)
        <$> I.ignis adminPublicKeys (I.EmailPassword id id) stream id) of
    Left e -> error $ show e
    Right v -> v

state clients tasks tries =
  State Q.empty clients 0 tasks tries VT.empty

interval :: Int -> Int -> [Int]
interval a b = if a >= b then [] else a : interval (a + 1) b

arbitraryState' readerCount writerCount trieCount = do
  tries <- take trieCount <$> arbitraryTries
  tasks <- arbitraryTasks

  let writers = map (makeClient True . bsShow) $ interval 1 writerCount

      readers = map (makeClient False . bsShow)
                $ interval writerCount (writerCount + readerCount)

      admin' = L.over clientRelay initAdmin admin

      public = Cr.derivePublic . I.getIgnisPrivate . getClientIgnis

      initAdmin relay =
        case exec (R.initAdmin
                   (I.getIgnisPrivate $ getClientIgnis admin)
                   (public <$> readers)
                   (public <$> (admin : writers))) relay of
          Left e -> error $ show e
          Right v -> v

  return $ state (admin' : (writers ++ readers)) tasks tries

-- arbitraryState readerCount writerCount =
--   QC.choose (10, 1000) >>= arbitraryState' readerCount writerCount

shrinkNonEmpty = \case
  [] -> []
  [_] -> []
  x:xs -> (x:) <$> inits xs

shrinkClients clients = case writers of
  [] -> []
  [w] -> case readers of
    [] -> []
    _ -> (w:) <$> inits readers
  ws -> (++) <$> shrinkNonEmpty ws <*> inits readers
  where
    (writers, readers) = foldr visit ([], []) clients
    visit client (ws, rs) = if getClientIsWriter client then
                              (client:ws, rs)
                            else
                              (ws, client:rs)

shrinkState (State { getStateClients = clients
                   , getStateTasks = tasks
                   , getStateTries = tries }) = do
  case clients of
    [_] -> case tries of
      [_] -> []
      _ -> state clients tasks <$> shrinkNonEmpty tries
    _ -> case tries of
      [_] -> (\c -> state c tasks tries) <$> shrinkClients clients
      _ -> (\c t -> state c tasks t)
           <$> shrinkClients clients <*> shrinkNonEmpty tries

clientHasMessages now client = case messages of
  Right (_:_) -> True
  _ -> False
  where
    messages = (++)
      <$> eval (R.nextMessages now) (getClientRelay client)
      <*> eval (I.nextMessages now) (getClientIgnis client)

apply task = do
  -- messages <- get stateMessages

  -- traceM ("apply " ++ show task ++ " message count: " ++ show (Q.length messages))

  update stateNow (+100)

  apply' task

apply' = \case
  SendFirst -> Q.viewl <$> get stateMessages >>= \case
    message Q.:< messages -> deliver message >> set stateMessages messages
    _ -> return ()

  SendLast -> Q.viewr <$> get stateMessages >>= \case
    messages Q.:> message -> deliver message >> set stateMessages messages
    _ -> return ()

  DropFirst -> Q.viewl <$> get stateMessages >>= \case
    _ Q.:< messages -> set stateMessages messages
    _ -> return ()

  Flush tasks ->
    let flush :: [Task] -> Co.Action State ()
        flush = \case
          [] -> St.get >>= exception . ("flush failed: " ++) . show

          task:tasks -> do
            (consistent <$> St.get) >>= \case
              True -> trace "flush success" $ return ()
              False -> apply task >> flush tasks

        flushable = \case
          Publish {} -> False
          Reconnect {} -> False
          Flush {} -> False
          _ -> True

    in do
      clientCount <- length <$> get stateClients

      flush $ take (100 * clientCount) $ filter flushable tasks

  Reconnect tasks whichClient -> do
    getClient whichClient (const True) >>= \case
      Nothing -> return ()
      Just client -> do
        traceM ("reconnect " ++ show (length tasks))

        update stateClients $ removeClient client

        mapM_ apply tasks

        traceM "reconnect complete"

        let old = getClientRelay client

        update stateClients
          (L.set clientRelay
           (R.relay (R.getRelayPRNG old) adminPublicKeys stream
            $ R.getRelayChallenge old) client :)

  Publish tasks whichClient ->
    getClient whichClient getClientIsWriter >>= \case
      Nothing -> return ()
      Just writer ->
        get stateTries >>= \case
          [] -> apply (Flush tasks) >> E.throwError Co.Success

          trie:tries -> do
            set stateTries tries
            set statePublished trie

            ignis' <- eitherToAction
                      $ exec (I.setHead trie)
                      $ getClientIgnis writer

            update stateClients
              $ replaceClient $ L.set clientIgnis ignis' writer

  QueueMessages whichClient -> do
    -- todo: serialize and deserialize messages to maximize code
    -- coverage
    now <- get stateNow
    getClient whichClient (clientHasMessages now) >>= \case
      Nothing -> return ()
      Just client -> do
        (relayMessages, relay') <-
          eitherToAction $ run (R.nextMessages now) $ getClientRelay client

        (ignisMessages, ignis') <-
          eitherToAction $ run (I.nextMessages now) $ getClientIgnis client

        -- M.when (not $ null relayMessages)
        --   $ traceM ("relay to ignis: " ++ show relayMessages)

        -- M.when (not $ null ignisMessages)
        --   $ traceM ("ignis to relay: " ++ show ignisMessages)

        let append messageType list sequence =
              foldr (flip (Q.|>)) sequence
              (Message (getClientId client) messageType <$> list)

        update stateMessages
          $ append ToIgnis relayMessages
          . append ToRelay ignisMessages

        update stateClients $ replaceClient
          $ L.set clientIgnis ignis'
          $ L.set clientRelay relay' client

applyNext = get stateTasks >>= \case
  task:tasks -> set stateTasks tasks >> apply task
  _ -> patternFailure

simulate state = trace (" *** start simulation: " ++ show state) $
  let limit = 1000 * length (getStateTries state) in
  case exec (M.replicateM_ limit applyNext) state of
    Left result -> case result of
      Success -> QC.property True
      error -> QC.counterexample (show error) False
    Right _ -> error ("failed to complete after " ++ show limit ++ " steps")

-- todo: assert no Nacks are sent when packets are never lost or reordered

-- todo: assert that node never sends messages it should know the
-- remote already has

runTests :: (forall t. QC.Testable t => String -> t -> IO ()) -> IO ()
runTests check = do
  check "one reader, one writer"
    $ QC.forAllShrink (arbitraryState' 0 1 1) shrinkState simulate
