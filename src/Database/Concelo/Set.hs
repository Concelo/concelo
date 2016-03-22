module Database.Concelo.Set
  ( Set()
  , empty
  , member
  , insert
  , delete
  , union ) where

import qualified Database.Concelo.Map as M

newtype Set v = Set { run :: M.Map v () }

instance Foldable Set where
  foldr visit seed = M.foldrPairs (visit . fst) seed . run

empty = Set M.empty

member :: Ord v => v -> Set v -> Bool
member v = M.member v . run

insert v = Set . M.insert v () . run

delete v = Set . M.delete v . run

union small large = foldr insert large small
