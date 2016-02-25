module Database.Concelo.Control
  ( get
  , set
  , update
  , updateThenGet
  , getThenUpdate
  , getThenSet
  , with
  , error
  , patternFailure
  , mapPair
  , bindMaybe
  , bindMaybe2 ) where

import Data.ByteString (ByteString)

import qualified Control.Lens as L
import qualified Control.Monad.State.Class as S

data Exception = Error ByteString
               | PatternFailure

run = runIdentity . runErrorT . runStateT

get lens = L.view lens <$> S.get

set lens value = S.state $ \s -> ((), L.set lens value s)

update lens update = S.state $ \s -> ((), L.over lens update s)

updateThenGet lens update =
  S.state $ \s -> let v = update $ L.get lens s in (v, L.set lens v s)

getThenUpdate lens update =
  S.state $ \s -> (L.get lens s, L.over lens update s)

getThenSet lens value = S.state $ \s -> (L.get lens s, L.set lens value s)

with lens action =
  run action <$> get lens >>= \case
    Left error -> throwError error
    Right (result, state) -> do
      set lens state
      return result

error s = throwError $ Error s

patternFailure = throwError PatternFailure

mapPair f (x, y) = (f x, f y)

bindMaybe f x =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> f x'

bindMaybe2 f x y =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> y >>= \case
      Nothing -> return Nothing
      Just y' -> f x' y'
