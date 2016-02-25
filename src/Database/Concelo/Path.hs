module Database.Concelo.Path
  ( Path()
  , empty
  , sub
  , super
  , singleton ) where

import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M

newtype Path k v = Path { run :: T.Trie k v }

instance Functor Trie where
  fmap f = Path . fmap f . run

instance M.FoldableWithKey Path where
  foldrWithKey visit seed = foldrWithKey visit seed . run

empty = Path T.empty

sub k = Path . T.sub k . run

super k = Path . T.super k . run

singleton k v = Path . T.singleton k v
