module Database.Concelo.PubSub
  ( nextMessages ) where

pingInterval = 1000

nextMessages now pub sub sent lastPing ping = do
  subMessage <- with sub Sub.nextMessage
  pubMessage <- with pub Pub.nextMessage

  pubMessages <- case pubMessage of
    Nothing -> do
      pubSent <- getAndSet sent False
      if pubSent then
        ping
      else do
        last <- get lastPing
        if now - last > pingInterval then do
          set lastPing now
          ping
        else
          return []

    Just message -> do
      set sent True
      return [message]

  return maybe pubMessages (:pubMessages) subMessage
