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
  , super
  , append
  , Database.Concelo.Path.init
  , initLast ) where

data Path k v = Path { getPathKeys :: [k]
                     , getPathValue :: v } deriving (Eq, Show)

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

append (Path ks v) k = Path (ks ++ [k]) v

initLast' = \case
  [] -> error "empty list"
  [k] -> ([], k)
  k:ks -> let (init, last) = initLast' ks in (k:init, last)

initLast (Path ks v) = (Path init v, last) where
  (init, last) = initLast' ks

init = fst . initLast
