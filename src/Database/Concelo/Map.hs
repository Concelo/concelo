module Database.Concelo.Map
  ( Map()
  , empty
  , member
  , Database.Concelo.Map.lookup
  , first
  , foldrPairs
  , pairs
  , insert
  , modify
  , delete
  , index ) where

import qualified Database.Concelo.VMap as V

newtype Map k v = Map { run :: V.VMap k v }

instance Foldable (Map k) where
  foldr visit seed = foldr visit seed . run

noVersion = -1

empty = Map V.empty

member k = V.member k . run

lookup k = V.lookup k . run

first = V.first . run

foldrPairs visit seed = V.foldrPairs visit seed . run

pairs = V.pairs . run

insert k v = Map . V.insert noVersion k v . run

modify k f = Map . V.modify noVersion k f . run

delete k = Map . V.delete noVersion k . run

index f = Map . V.index noVersion f
