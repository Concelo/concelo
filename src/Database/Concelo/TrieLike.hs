module Database.Concelo.TrieLike
  ( TrieLike(value, member, foldrPairs) ) where

import qualified Database.Concelo.Path as P

class TrieLike t where
  value :: t k v -> Maybe v
  member :: P.Path k v0 -> t k v -> Maybe v
  foldrPairs :: ((k, TrieLike t0) -> b -> b) -> b -> t k v -> b
