module Database.Concelo.Trie
  ( Trie()
  , trie
  , isEmpty
  , empty
  , isLeaf
  , leaf
  , value
  , firstPath
  , firstValue
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
  , superValue
  , singleton
  , index
  , union
  , intersectL
  , intersectR
  , Database.Concelo.Trie.subtract ) where

import qualified Database.Concelo.VTrie as V
import qualified Database.Concelo.TrieLike as TL

newtype Trie k v = Trie { run :: V.VTrie k v }

instance Ord k => Functor (Trie k) where
  fmap f = Trie . fmap f . run

instance Foldable (Trie k) where
  foldr visit seed = foldr visit seed . run

instance TL.TrieLike Trie where
  value = TL.value . run
  member path = TL.member path . run
  foldrPairs visit seed =
    TL.foldrPairs (\(k, a) -> visit (k, Trie a)) seed . run
  foldrPaths visit seed = TL.foldrPaths visit seed . run

noRevision = -1

trie = Trie

isEmpty = V.isEmpty . run

empty = Trie $ V.empty

isLeaf = V.isLeaf . run

leaf = Trie . V.leaf noRevision

value = V.value . run

firstPath = V.firstPath . run

firstValue = V.firstValue . run

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
fromTrieLike = Trie . V.fromTrieLike noRevision

sub k = Trie . V.sub k . run

superValue k v = Trie . V.superValue noRevision k v . run

super k = Trie . V.super noRevision k . run

singleton k = Trie . V.singleton noRevision k

index f = Trie . V.index noRevision f

union a = Trie . V.union noRevision a . run

intersectL a = Trie . V.intersectL noRevision a . run

intersectR a = Trie . V.intersectR noRevision a . run

subtract a = Trie . V.subtract noRevision a . run
