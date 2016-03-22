module Database.Concelo.Trie
  ( Trie()
  , trie
  , isEmpty
  , empty
  , isLeaf
  , leaf
  , firstPath
  , first
  , find
  , findValue
  , member
  , hasAll
  , hasAny
  , foldrPaths
  , paths
  , foldrPathsAndValues
  , pathsAndValues
  , sub
  , super
  , superValue
  , union
  , intersectL
  , intersectR
  , Database.Concelo.Trie.subtract
  , subtractAll ) where

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

noRevision = -1

trie = Trie

isEmpty = V.isEmpty . run

empty = Trie $ V.empty

isLeaf = V.isLeaf . run

leaf v = Trie $ V.leaf v

firstPath = V.firstPath . run

first = V.first . run

find path = Trie . V.find path . run

findValue path = V.findValue path . run

member path = V.member path . run

hasAll a b = V.hasAll (run a) (run b)

hasAny a b = V.hasAny (run a) (run b)

foldrPaths visit seed = V.foldrPaths visit seed . run

paths = V.paths . run

foldrPathsAndValues visit seed = V.foldrPathsAndValues visit seed . run

pathsAndValues = V.pathsAndValues . run

sub k = Trie . V.sub k . run

superValue k v = Trie . V.superValue noRevision k v . run

super k = Trie . V.super noRevision k . run

union a = Trie . V.union noRevision a . run

intersectL a = Trie . V.intersectL noRevision a . run

intersectR a = Trie . V.intersectR noRevision a . run

subtract a = Trie . V.subtract noRevision a . run

subtractAll a = Trie . V.subtractAll noRevision a . run
