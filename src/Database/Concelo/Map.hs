{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
module Database.Concelo.Map
  ( Map()
  , empty
  , singleton
  , member
  , Database.Concelo.Map.lookup
  , first
  , firstValue
  , foldrKeys
  , keys
  , foldrPairs
  , pairs
  , insert
  , modify
  , delete
  , index
  , union
  , Database.Concelo.Map.subtract
  , diff ) where

import qualified Data.ByteString.Char8 as BS
import qualified Database.Concelo.VMap as V
import Prelude hiding (foldr)
import Data.Foldable (Foldable(foldr))

newtype Map k v = Map { run :: V.VMap k v }

instance (Show k, Show v) => Show (Map k v) where
  show = show . run

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
  Show v => Show (Map BS.ByteString v) where
  show = show . run

instance Ord k => Functor (Map k) where
  fmap f = Map . fmap f . run

instance Foldable (Map k) where
  foldr visit seed = foldr visit seed . run

noVersion = -1

empty = Map V.empty

singleton k v = Map $ V.singleton noVersion k v

member k = V.member k . run

lookup k = V.lookup k . run

first = V.first . run

firstValue = V.firstValue . run

foldrKeys visit seed = V.foldrKeys visit seed . run

keys = V.keys . run

foldrPairs visit seed = V.foldrPairs visit seed . run

pairs = V.pairs . run

insert k v = Map . V.insert noVersion k v . run

modify k f = Map . V.modify noVersion k f . run

delete k = Map . V.delete noVersion k . run

index f = Map . V.index noVersion f

union small large = foldrPairs (\(k, v) -> insert k v) large small

subtract small large = foldrKeys delete large small

diff a b = (Map obsolete, Map new) where
  (obsolete, new) = V.diff noVersion a b
