{-# LANGUAGE LambdaCase #-}
module Database.Concelo.TrieLike
  ( TrieLike(value, member, foldrPairs, foldrPaths) ) where

import qualified Database.Concelo.Path as P

class TrieLike t where
  value :: t k v -> Maybe v
  member :: Ord k => P.Path k v0 -> t k v -> Bool
  foldrPairs :: ((k, t k v) -> b -> b) -> b -> t k v -> b
  foldrPaths :: (P.Path k v -> b -> b) -> b -> t k v -> b

instance TrieLike P.Path where
  value = P.valueHere

  member a b = (P.keys a) == (P.keys b)

  foldrPairs visit seed p = case P.keys p of
    k:ks -> visit (k, P.toPath ks $ P.value p) seed
    _ -> seed

  foldrPaths visit seed p = visit p seed
