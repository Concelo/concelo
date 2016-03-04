module Database.Concelo.Trie
  ( Trie()
  , trie
  , empty
  , key
  , value
  , firstPath
  , first
  , find
  , findValue
  , findTrie
  , member
  , hasAll
  , hasAny
  , triples
  , paths
  , pathsAndValues
  , insert
  , update
  , modify
  , delete
  , sub
  , super
  , singleton
  , union
  , intersectL
  , intersectR
  , subtract
  , subtractAll ) where

import qualified Database.Concelo.VTrie as V
import qualified Database.Concelo.Map as M

data Trie k v = Trie { version :: Integer
                     , run :: V.VTrie k v }

instance Functor VTrie where
  fmap f (Trie v t) = Trie v $ fmap f t

instance M.FoldableWithKey Trie where
  foldrWithKey visit seed = foldrWithKey visit seed . run

trie = Trie

empty r = Trie r V.empty

data Element k v = Element { key :: Maybe k
                           , value :: Maybe v
                           , subTrie :: Trie k v }

root = Element Nothing Nothing

firstPath = V.firstPath . run

first = V.first . run

find path = V.find path . run

findValue path = V.findValue path . run

findTrie path (Trie r t) = Trie r $ V.findTrie path t

member path = V.member path . run

isSuperSetOf super sub = V.isSuperSetOf (run super) (run sub)

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
