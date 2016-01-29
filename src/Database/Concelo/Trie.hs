module Trie
  ( groupBy ) where

groupBy getKey trie = foldrWithKey fold empty trie where
  fold key item result = insert newKey item result where
    newKey = getKey item <| key
