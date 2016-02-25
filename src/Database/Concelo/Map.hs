module Database.Concelo.Map
  ( Map()
  , FoldableWithKey(foldrWithKey)
  , empty
  , key
  , value
  , member
  , lookup
  , first
  , insert
  , modify
  , delete ) where

import qualified Database.Concelo.VMap as V

newtype Map k v = Map { run :: V.VMap k v }

class FoldableWithKey t where
  foldrWithKey :: (k -> a -> b -> b) -> b -> t k a -> b

instance FoldableWithKey Map where
  foldrWithKey visit seed = foldrWithKey visit seed . run

empty = Map V.empty

key = V.key . run

value = V.value . run

member k = V.member k . run

lookup k = V.lookup k . run

first = V.first . run

insert k v = Map . V.insert 0 k v . run

modify k f = Map . V.modify 0 k f . run

delete k = Map . V.delete 0 k . run
