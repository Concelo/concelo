{-# LANGUAGE RankNTypes #-}
module Database.Concelo.BiTrie
  ( empty
  , insert
  , insertTrie
  , reverseDelete
  , delete
  , reverseSubtract
  , Database.Concelo.BiTrie.subtract
  , reverseFind
  , find ) where

import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as P
import qualified Control.Lens as L

type Trie k = T.Trie k ()

data BiTrie k = BiTrie { getForward :: Trie k
                       , getReverse :: Trie k }

forward :: L.Lens' (BiTrie k) (Trie k)
forward = L.lens getForward (\x v -> x { getForward = v })

reverse' :: L.Lens' (BiTrie k) (Trie k)
reverse' = L.lens getReverse (\x v -> x { getReverse = v })

empty = BiTrie T.empty T.empty

append a b = foldr P.super b $ P.keys a

insert key value biTrie = BiTrie
  (T.union (value `append` key) (getForward biTrie))
  (T.union (key `append` value) (getReverse biTrie))

insertTrie key trie biTrie =
  T.foldrPaths (insert key) biTrie trie

reverseDelete value = delete' value reverse' forward

delete key = delete' key forward reverse'

delete' :: Ord k =>
           P.Path k () ->
           L.Lens' (BiTrie k) (Trie k) ->
           L.Lens' (BiTrie k) (Trie k) ->
           BiTrie k ->
           BiTrie k
delete' path a b biTrie =
  L.set b (T.foldrPaths (\p -> T.subtract (p `append` path)) (L.view b biTrie)
           $ T.find path $ L.view a biTrie)
  $ L.set a (T.subtractAll path $ L.view a biTrie) biTrie

reverseSubtract values = subtract' values reverseDelete

subtract keys = subtract' keys delete

subtract' trie visit biTrie =
  foldr visit biTrie $ T.paths trie

reverseFind value = T.find value . getReverse

find key = T.find key . getForward
