module Database.Concelo.Map
  ( Map()
  , empty
  , key
  , value
  , member
  , Database.Concelo.Map.lookup
  , first
  , pairs
  , insert
  , modify
  , delete
  , index ) where

import qualified Database.Concelo.VMap as V

newtype Map k v = Map { run :: V.VMap k v }

noVersion = -1

empty = Map V.empty

key = V.key . run

value = V.value . run

member k = V.member k . run

lookup k = V.lookup k . run

first = V.first . run

pairs = V.pairs . run

insert k v = Map . V.insert noVersion k v . run

modify k f = Map . V.modify noVersion k f . run

delete k = Map . V.delete noVersion k . run

index f = Map . V.index noVersion f
