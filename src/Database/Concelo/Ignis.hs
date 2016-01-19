{-# LANGUAGE OverloadedStrings #-}

module Database.Concelo.Ignis (main) where

import Haste.Foreign (export, toOpaque)
import Haste.Prim (JSAny())

data Auth = Auth
  { authUid :: String
  , authProvider :: String
  , authExpires :: Int }

data Ignis = Ignis
  { ignisPointer :: JSAny
  , ignisConnection :: JSAny }

io :: a -> IO a
io = return

main = do
  export "ignis" $ \pointer connection ->
    io $ toOpaque $ Ignis pointer connection
