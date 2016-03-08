module Database.Concelo.Path
  ( Path()
  , leaf
  , singleton
  , toPath
  , keys
  , value
  , sub
  , super ) where

import qualified Database.Concelo.TrieLike as TL

data Path k v = Path { getPathKeys :: [k]
                     , getPathValue :: v }

instance Functor Path where
  fmap f (Path ks v) = Path ks (f v)

instance Foldable (Path k) where
  foldr visit seed (Path _ v) = visit v seed

leaf v = Path [] v

singleton k v = Path [k] v

keys = getPathKeys

value = getPathValue

toPath = Path

sub = \case
  Path (k:ks) v -> Just (Path ks v)
  p -> Nothing

super k (Path ks v) = Path (k:ks) v
