{-# LANGUAGE RankNTypes #-}
module Database.Concelo.Pipe
  ( Pipe
  , pipe
  , pipePublisher
  , pipeSubscriber
  , nextMessages ) where

import Database.Concelo.Control (with, getThenSet, get, set)

import qualified Control.Lens as L
import qualified Database.Concelo.Publisher as P
import qualified Database.Concelo.Subscriber as S

data Pipe = Pipe { getPipePublisher :: P.Publisher
                 , getPipeSubscriber :: S.Subscriber
                 , getPipeLastPing :: Integer
                 , getPipePublisherSent :: Bool }

pipe adminACL publicKey stream =
  Pipe P.publisher (S.subscriber adminACL publicKey stream) 0 False

pipePublisher :: L.Lens' Pipe P.Publisher
pipePublisher = L.lens getPipePublisher (\x v -> x { getPipePublisher = v })

pipeSubscriber :: L.Lens' Pipe S.Subscriber
pipeSubscriber = L.lens getPipeSubscriber (\x v -> x { getPipeSubscriber = v })

pipeLastPing :: L.Lens' Pipe Integer
pipeLastPing = L.lens getPipeLastPing (\x v -> x { getPipeLastPing = v })

pipePublisherSent :: L.Lens' Pipe Bool
pipePublisherSent =
  L.lens getPipePublisherSent (\x v -> x { getPipePublisherSent = v })

pingInterval = 1000

nextMessages now ping = do
  subMessage <- with pipeSubscriber S.nextMessage
  pubMessage <- with pipePublisher P.nextMessage

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

  return $ maybe pubMessages (:pubMessages) subMessage
