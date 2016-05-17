{-# LANGUAGE FlexibleInstances #-}
module Database.Concelo.Trie
  ( Trie()
  , trie
  , isEmpty
  , empty
  , isLeaf
  , leaf
  , value
  , setValue
  , firstPath
  , firstValue
  , lastPath
  , lastValue
  , findTrie
  , findValue
  , member
  , hasAll
  , hasAny
  , foldrPairs
  , pairs
  , foldrPaths
  , paths
  , foldrPathsAndValues
  , pathsAndValues
  , foldrKeys
  , keys
  , foldrTriples
  , triples
  , fromTrieLike
  , sub
  , super
  , singleton
  , index
  , union
  , intersectL
  , intersectR
  , Database.Concelo.Trie.subtract
  , diff ) where

import qualified Data.ByteString.Char8 as BS
import qualified Database.Concelo.VTrie as V
import qualified Database.Concelo.TrieLike as TL

newtype Trie k v = Trie { run :: V.VTrie k v } deriving (Eq)

instance {-# OVERLAPPABLE #-} (Show k, Show v) => Show (Trie k v) where
  show = show . run

instance {-# OVERLAPPING #-} Show v => Show (Trie BS.ByteString v) where
  show = show . run

instance Ord k => Functor (Trie k) where
  fmap f = Trie . fmap f . run

instance Foldable (Trie k) where
  foldr visit seed = foldr visit seed . run

instance Ord k => Traversable (Trie k) where
  traverse f = foldrPathsAndValues visit (pure empty) where
    visit (p, v) t = union . (<$> p) . const <$> f v <*> t

instance TL.TrieLike Trie where
  value = TL.value . run
  member path = TL.member path . run
  foldrPairs visit seed =
    TL.foldrPairs (\(k, a) -> visit (k, Trie a)) seed . run
  foldrPaths visit seed = TL.foldrPaths visit seed . run

noVersion = -1

trie = Trie

isEmpty = V.isEmpty . run

empty = Trie $ V.empty

isLeaf = V.isLeaf . run

leaf = Trie . V.leaf noVersion

value = V.value . run

setValue v = Trie . V.setValue noVersion v . run

firstPath = V.firstPath . run

firstValue = V.firstValue . run

lastPath = V.lastPath . run

lastValue = V.lastValue . run

findTrie path = Trie . V.findTrie path . run

findValue path = V.findValue path . run

member path = V.member path . run

hasAll a b = V.hasAll (run a) (run b)

hasAny a b = V.hasAny (run a) (run b)

foldrPairs visit seed = V.foldrPairs visit seed . run

pairs = fmap (\(k, v) -> (k, Trie v)) . V.pairs . run

foldrPaths visit seed = V.foldrPaths visit seed . run

paths = V.paths . run

foldrPathsAndValues visit seed = V.foldrPathsAndValues visit seed . run

pathsAndValues = V.pathsAndValues . run

foldrKeys visit seed = V.foldrKeys visit seed . run

keys = V.keys . run

foldrTriples visit seed = V.foldrTriples visit seed . run

triples = V.triples . run

fromTrieLike :: (TL.TrieLike t, Ord k) =>
                t k v ->
                Trie k v
fromTrieLike = Trie . V.fromTrieLike noVersion

sub k = Trie . V.sub k . run

super k = Trie . V.super noVersion k . run

singleton k = Trie . V.singleton noVersion k

index f = Trie . V.index noVersion f

union a = Trie . V.union noVersion a . run

intersectL a = Trie . V.intersectL noVersion a . run

intersectR a = Trie . V.intersectR noVersion a . run

subtract a = Trie . V.subtract noVersion a . run

diff a b = (Trie obsolete, Trie new) where
  (obsolete, new) = V.diff noVersion a b
