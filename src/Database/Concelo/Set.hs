module Database.Concelo.Set
  ( Set()
  , empty
  , member
  , insert
  , delete
  , union
  , Database.Concelo.Set.subtract ) where

import qualified Database.Concelo.Map as M

import Data.Foldable (toList)
import Prelude hiding (foldr)
import Data.Foldable (Foldable(foldr))

newtype Set v = Set { run :: M.Map v () }

instance Show v => Show (Set v) where
  show = show . toList

instance Foldable Set where
  foldr visit seed = M.foldrPairs (visit . fst) seed . run

empty = Set M.empty

member :: Ord v => v -> Set v -> Bool
member v = M.member v . run

insert v = Set . M.insert v () . run

delete v = Set . M.delete v . run

union small large = foldr insert large small

subtract small large = foldr delete large small
