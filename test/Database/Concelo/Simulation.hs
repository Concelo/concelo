module Database.Concelo.Simulator
  ( runTests ) where

import Database.Concelo.Control (exec, update, Exception(Success))

import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.Relay as R
import qualified Database.Concelo.Ignis as I
import qualified Database.Concelo.Crypto as C
import qualified Control.Monad.State.Class as St
import qualified Data.ByteString as BS
import qualified Data.Sequence as Q
import qualified Test.QuickCheck as QC
import qualified Control.Lens as L

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
                       , getMessageValue :: P.Message }
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

deliver (Message clientId type_ value) =
  (filter ((clientId /=) . getClientId) <$> get stateMessages) >>= \case
    [client] -> case type_ of
      ToRelay -> do
        -- todo: we need some way to return the updated relays from
        -- R.receive, or else we need to restructure the control flow
        relays <- fmap getClientRelay <$> get stateClients
        relay <- try $ exec (R.receive relays value) (getClientRelay client)
        update stateClients (L.set clientRelay relay client :)
      ToIgnis -> do
        ignis <- try $ exec (I.receive value) (getClientIgnis client)
        update stateClients (L.set clientIgnis ignis client :)
    _ -> return ()

randomClient predicate = do
  clients <- get stateClients
  if null clients then
    return Nothing
    else
    Just <$> (generate $ QC.elements $ filter predicate clients)

generate = liftIO . QC.generate

consistent (State _ clients _) = case clients of
  client:clients -> consistentWith client clients
  _ -> True
  where
    head = I.head . getClientIgnis

    consistentWith client = \case
      c:cs -> head c == head client && consistentWith client cs
      _ -> True

apply = (update stateNow (+100) >>) . apply'

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

        update stateClients $ filter ((getClientId client /=) . getClientId))

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

        ignis' <-
          try $ exec (I.setHead $ T.union toAdd $ T.subtract toDelete paths)
          ignis

        update stateClients (L.set clientIgnis ignis' writer :)

  QueueMessages -> do
    now <- get stateNow
    randomClient (clientHasMessages now) >>= \case
      Nothing -> return ()
      Just client -> do
        (relayMessages, relay') <-
          try (run R.nextMessages $ getClientRelay client)

        (ignisMessages, ignis') <-
          try (run R.nextMessages $ getClientIgnis client)

        let append = foldr $ flip (|>)

        update stateMessages $ append relayMessages . append ignisMessages

        update stateClients (L.set clientIgnis ignis'
                             (L.set clientRelay relay' client) :)

makeClient writer =
  seed <- BS.pack <$> QC.generate $ QC.vector C.seedSize
  return Client seed writer R.empty $ I.ignis seed

run readerCount writerCount = do
  readers <- replicateM readerCount (makeClient false)
  writers <- replicateM writerCount (makeClient true)

  let state = State Q.empty (readers ++ writers) 0

  QC.ioProperty case exec (mapM_ (liftIO . apply) tasks) state of
    Left result -> case result of
      Success -> QC.property True
      error -> QC.counterexample (show error) False
    Right _ -> undefined

runTests test = do
  test "one reader, one writer" $ run 1 1
