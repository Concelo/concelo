module Database.Concelo.Pipe
  ( Pipe
  , empty
  , pipePublisher
  , pipeSubscriber
  , nextMessages ) where

import Database.Concelo.Control (with, getAndSet)

import qualified Control.Lens as L

data Pipe = Pipe { getPipePublisher :: Pub.Publisher
                 , getPipeSubscriber :: Pub.Subscriber
                 , getPipeLastPing :: Integer
                 , getPipePublisherSent :: Bool }

empty = Pipe Pub.empty Sub.empty 0 False

pipePublisher = L.lens getPipePublisher (\x v -> x { getPipePublisher = v })

pipeSubscriber = L.lens getPipeSubscriber (\x v -> x { getPipeSubscriber = v })

pipeLastPing = L.lens getPipeLastPing (\x v -> x { getPipeLastPing = v })

pipePublisherSent =
  L.lens getPipePublisherSent (\x v -> x { getPipePublisherSent = v })

pingInterval = 1000

nextMessages now ping = do
  subMessage <- with pipeSubscriber Sub.nextMessage
  pubMessage <- with pipePublisher Pub.nextMessage

  pubMessages <- case pubMessage of
    Nothing -> do
      pubSent <- getThenSet pipePublisherSent False
      if pubSent then
        ping
        else do
        last <- get pipeLastPing
        if now - last > pingInterval then do
          set pipeLastPing now
          ping
          else
          return []

    Just message -> do
      set pipePublisherSent True
      return [message]

  return maybe pubMessages (:pubMessages) subMessage
