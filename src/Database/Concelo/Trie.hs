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
  , values
  , sub
  , super
  , superValue
  , union
  , intersectL
  , intersectR
  , subtract
  , subtractAll ) where

import qualified Database.Concelo.VTrie as V
import qualified Database.Concelo.TrieLike as TL

newtype Trie k v = Trie { run :: V.VTrie k v }

instance Functor (Trie k) where
  fmap f = Trie . fmap f . run

instance Foldable (Trie k) where
  foldr visit seed = foldr visit seed . run

instance TL.TrieLike Trie where
  value = value . run
  member path = member path . run
  foldrPairs visit seed = foldrPairs visit seed . run

trie = Trie

isEmpty = V.isEmpty . run

empty = Trie $ V.empty

isLeaf = V.isLeaf . run

leaf v = Trie $ V.leaf v

firstPath = V.firstPath . run

first = V.first . run

find path = V.find path . run

findValue path = V.findValue path . run

member path = V.member path . run

hasAll a b = V.hasAll (run a) (run b)

triples trie = (\(k, v, sub) -> (k, v, Trie sub)) <$> run trie

paths = V.paths . run

pathsAndValues = V.pathsAndValues . run

insert k v (Trie r t) = Trie r $ V.insert r k v t

modify k f (Trie r t) = Trie r $ V.modify r k f t

delete k (Trie r t) = Trie r $ V.delete r k t

sub k (Trie r t) = Trie r $ V.sub k t

superKV k v (Trie r t) = Trie r $ V.superKV r k v t

super k (Trie r t) = Trie r $ V.super r k t

singleton r k v = Trie r $ V.singleton r k v

union a (Trie r t) = Trie r $ V.union r a t

intersectL a (Trie r t) = Trie r $ V.intersectL r a t

intersectR a (Trie r t) = Trie r $ V.intersectR r a t

subtract a (Trie r t) = Trie r $ V.subtract r a t

subtractAll a (Trie r t) = Trie r $ V.subtractAll r a t
