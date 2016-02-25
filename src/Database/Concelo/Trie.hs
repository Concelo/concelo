module Database.Concelo.Trie
  ( Trie()
  , empty
  , key
  , value
  , firstPath
  , first
  , insert
  , modify
  , delete
  , sub
  , super
  , singleton
  , union
  , intersectL
  , intersectR
  , subtract ) where

import qualified Database.Concelo.VTrie as V
import qualified Database.Concelo.Map as M

newtype Trie k v = Trie { run :: V.VTrie k v }

instance M.FoldableWithKey Trie where
  foldrWithKey visit seed = foldrWithKey visit seed . run

empty = Trie V.empty

key = V.key . run

value = V.value . run

subTrie = Trie . V.subTrie . run

firstPath = V.firstPath . run

first = V.first . run

paths = V.paths . run

insert k v = Trie . V.insert 0 k v . run

modify k f = Trie . V.modify 0 k f . run

delete k = Trie . V.delete 0 k . run

sub k = Trie . V.sub k . run

superKV k v = Trie . V.superKV 0 k v . run

super k = Trie . V.super 0 k . run

singleton k v = Trie . V.singleton 0 k v

union a = Trie . V.union 0 a . run

intersectL a = Trie . V.intersectL 0 a . run

intersectR a = Trie . V.intersectR 0 a . run

subtract a = Trie . V.subtract 0 a . run
