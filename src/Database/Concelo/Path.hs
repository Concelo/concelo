module Database.Concelo.Path
  ( Path()
  , empty
  , toPath
  , sub
  , super
  , singleton ) where

import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M

newtype Path k v = Path { keys :: [k]
                        , value :: Maybe v }

instance Functor Trie where
  fmap f (Path ks v) = Path ks (fmap f v)

empty = Path [] Nothing

toPath ks v = Path ks $ Just v

sub = \case
  Path (k:ks@(_:_)) v -> Path ks v
  _ -> empty

super k (Path ks v) = Path (k:ks) v

singleton k v = Path [k] v
