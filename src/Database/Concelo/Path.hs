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

instance TL.TrieLike Path where
  value = value'
  member (Path as _) (Path bs _) = as == bs
  foldrPairs visit seed = \case
    Path (k:ks) v -> visit (k, Path ks v) seed
    _ -> seed

leaf v = Path [] v

singleton k v = Path [k] v

keys = getPathKeys

value' = \case
  Path [] v -> Just v
  _ -> Nothing

value = value'

toPath = Path

sub = \case
  Path (k:ks) v -> Just (Path ks v)
  p -> Nothing

super k (Path ks v) = Path (k:ks) v
