{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Path
  ( Path()
  , leaf
  , singleton
  , toPath
  , keys
  , value
  , valueHere
  , sub
  , super ) where

data Path k v = Path { getPathKeys :: [k]
                     , getPathValue :: v }

instance Functor (Path k) where
  fmap f (Path ks v) = Path ks (f v)

instance Foldable (Path k) where
  foldr visit seed (Path _ v) = visit v seed

leaf v = Path [] v

singleton k v = Path [k] v

keys = getPathKeys

value = getPathValue

valueHere = \case
  Path [] v -> Just v
  _ -> Nothing

toPath = Path

sub = \case
  Path (k:ks) v -> Just (k, Path ks v)
  _ -> Nothing

super k (Path ks v) = Path (k:ks) v
