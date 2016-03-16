module Database.Concelo.Simulation
  ( runTests ) where

import Database.Concelo.Control (exec, update, Exception(Success))

import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.Relay as R
import qualified Database.Concelo.Ignis as I
import qualified Database.Concelo.Crypto as C
import qualified Control.Monad.State.Class as St
import qualified Data.ByteString as BS
import qualified Data.Sequence as Q
import qualified Test.QuickCheck as QC
import qualified Control.Lens as L

instance QC.Arbitrary Pr.ValueBody where
  arbitrary = oneof [ gen Pr.NullBody
                    , Pr.NumberBody <$> arbitrary
                    , Pr.StringBody <$> arbitrary
                    , Pr.BooleanBody <$> arbitrary ]

  shrink v = Pr.NullBody : case v of
    Pr.NullBody -> []
    Pr.NumberBody n -> Pr.NumberBody <$> shrink n
    Pr.StringBody s -> Pr.StringBody <$> shrink s
    Pr.BooleanBody b -> Pr.BooleanBody <$> shrink b

instance QC.Arbitrary Pr.Value where
  arbitrary = Pr.value Pr.defaultPriority <$> arbitrary

  shrink v = Pr.value Pr.defaultPriority <$> shrink (Pr.getValueBody v)

instance QC.Arbitrary (Pa.Path BS.ByteString Pr.Value) where
  arbitrary = Pa.toPath <$> arbitrary <*> arbitrary

  shrink p = zipWith Pa.toPath (shrink $ keys p) (shrink $ findValue p)

data Client = Client { getClientId :: BS.ByteString
                     , getClientIsWriter :: Bool
                     , getClientRelay :: R.Relay
                     , getClientIgnis :: I.Ignis }
              deriving (Show)

clientId =
  L.lens getClientId (\x v -> x { getClientId = v })

clientIsWriter =
  L.lens getClientIsWriter (\x v -> x { getClientIsWriter = v })

clientRelay =
  L.lens getClientRelay (\x v -> x { getClientRelay = v })

clientIgnis =
  L.lens getClientIgnis (\x v -> x { getClientIgnis = v })

data MessageType = ToRelay | ToIgnis deriving (Show)

data Message = Message { getMessageClientId :: BS.ByteString
                       , getMessageType :: MessageType
                       , getMessageValue :: Pr.Message }
               deriving (Show)

data State = State { getStateMessages :: Q.Sequence Message
                   , getStateClients :: [Client]
                   , getStateNow :: Integer }
             deriving (Show)

stateMessages =
  L.lens getStateMessages (\x v -> x { getStateMessages = v })

stateClients =
  L.lens getStateClients (\x v -> x { getStateClients = v })

stateNow =
  L.lens getStateNow (\x v -> x { getStateNow = v })

gen = QC.elements . (:[])

deliver (Message clientId type_ value) =
  (filter ((clientId /=) . getClientId) <$> get stateMessages) >>= \case
    [client] -> case type_ of
      ToRelay -> do
        (newSubscriber, relay) <-
          try $ run (R.receive relays value) (getClientRelay client)

        updateM stateClients
          $ return . replaceClient (L.set clientRelay relay client)
          >>= case newSubscriber of
            Just sub ->
              mapM $ overM clientRelay $ try $ exec R.setSubscriber sub
            Nothing ->
              return . id

      ToIgnis -> do
        ignis <- try $ exec (I.receive value) (getClientIgnis client)
        update stateClients $ replaceClient $ L.set clientIgnis ignis client

    _ -> return ()

randomClient predicate = do
  clients <- get stateClients
  if null clients then
    return Nothing
    else
    Just <$> (generate $ QC.elements $ filter predicate clients)

removeClient client = filter ((getClientId client /=) . getClientId)

replaceClient client = (L.set clientRelay relay client :) . removeClient client

generate = liftIO . QC.generate

consistent (State _ clients _) = case clients of
  client:clients -> consistentWith client clients
  _ -> True
  where
    revision = I.getPublishedRevision . getClientIgnis

    consistentWith client = \case
      c:cs -> revision c == revision client && consistentWith client cs
      _ -> True

apply = (update stateNow (+100) >>) . apply'

data Task = SendFirst
          | SendLast
          | DropFirst
          | Flush
          | Reconnect
          | Publish
          | QueueMessages

instance QC.Arbitrary Task where
  arbitrary = QC.frequency [ (40, gen QueueMessages)
                           , (30, gen SendFirst)
                           , (10, gen Flush)
                           , (10, gen Publish)
                           , ( 5, gen SendLast)
                           , ( 5, gen DropFirst)
                           , ( 1, gen Reconnect) ]

apply' = \case
  SendFirst -> Q.viewl <$> get stateMessages >>= \case
    message :< messages -> deliver message >> set stateMessages messages
    _ -> return ()

  SendLast -> Q.viewr <$> get stateMessages >>= \case
    messages :> message -> deliver message >> set stateMessages messages
    _ -> return ()

  DropFirst -> Q.viewl <$> get stateMessages >>= \case
    _ :< messages -> set stateMessages messages
    _ -> return ()

  Flush ->
    let flush = \case
          [] -> St.get >>= exception . ("flush failed: " ++) . show

          task:tasks ->
            (consistent <$> St.get) >>= \case
              True -> return ()
              False -> apply task >> flush tasks

        flushable = \case
          Publish -> False
          Reconnect -> False
          Flush -> False
          _ -> True

    in do
      clientCount <- length <$> get stateClients

      (take (1000 * clientCount) . filter flushable
       <$> generate QC.infiniteList)
        >>= flush

  Reconnect ->
    randomClient (const True) >>= \case
      Nothing -> return ()
      Just client -> do
        delay <- generate $ QC.choose (10, 1000)

        update stateClients $ removeClient client

        mapM_ apply (generate $ QC.vector delay)

        update stateClients (L.set clientRelay R.empty client :)

  Publish ->
    randomClient getClientIsWriter >>= \case
      Nothing -> return ()
      Just writer -> do
        let ignis = getClientIgnis writer
            paths = T.paths $ I.head ignis
            pathCount = length paths

        deleteCount <- generate $ QC.choose (0, pathCount)
        toDelete <- take deleteCount <$> (generate $ QC.shuffle paths)

        addCount <- generate $ QC.choose (0, pathCount)
        toAdd <- generate $ QC.vector addCount

        now <- get stateNow

        ignis' <-
          try $ exec (I.update now (T.union toAdd . T.subtract toDelete) id)
          ignis

        update stateClients $ replaceClient $ L.set clientIgnis ignis' writer

  QueueMessages -> do
    get stateNow >>= randomClient . clientHasMessages >>= \case
      Nothing -> return ()
      Just client -> do
        (relayMessages, relay') <-
          try $ run R.nextMessages $ getClientRelay client

        (ignisMessages, ignis') <-
          try $ run R.nextMessages $ getClientIgnis client

        let append = foldr $ flip (|>)

        update stateMessages $ append relayMessages . append ignisMessages

        update stateClients $ replaceClient
          $ L.set clientIgnis ignis'
          $ L.set clientRelay relay' client

makeClient writer =
  seed <- BS.pack <$> QC.generate $ QC.vector C.seedSize
  return $ Client seed writer R.empty $ I.ignis seed

run readerCount writerCount = do
  readers <- replicateM readerCount $ makeClient false
  writers <- replicateM writerCount $ makeClient true

  let state = State Q.empty (readers ++ writers) 0

  QC.ioProperty case exec (mapM_ (liftIO . apply) tasks) state of
    Left result -> case result of
      Success -> QC.property True
      error -> QC.counterexample (show error) False
    Right _ -> undefined

runTests check = do
  check "one reader, one writer" $ run 1 1
