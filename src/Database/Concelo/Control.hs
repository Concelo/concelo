module Database.Concelo.Control
  ( get
  , set
  , getThenUpdate
  , with ) where

import qualified Control.Lens as L
import qualified Control.Monad.State.Class as S

run = runIdentity . runErrorT . runStateT

get lens = L.view lens <$> S.get

set lens value = S.state $ \s -> ((), L.set lens value s)

getThenUpdate lens update =
  S.state $ \s -> (L.get lens s, L.over lens update s)

with lens action =
  run action <$> get lens >>= \case
    Left error -> throwError error
    Right (result, state) -> do
      set lens state
      return result
