module Database.Concelo.BiTrie
  ( empty
  , insert
  , insertTrie
  , reverseDelete
  , delete
  , reverseSubtract
  , subtract
  , reverseFind
  , find ) where

import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as P
import qualified Control.Lens as L

type Trie k = T.Trie k ()

data BiTrie k = BiTrie { getForward :: Trie k
                       , getReverse :: Trie k }

forward = L.lens getForward (\x v -> x { getForward = v })
reverse = L.lens getReverse (\x v -> x { getReverse = v })

empty = BiTrie T.empty T.empty

append a b =
  case P.key a of
    Nothing -> b
    Just k -> P.super k (P.sub a `append` b)

insert key value biTrie = BiTrie
  (T.union (value `append` key) (getForward biTrie))
  (T.union (key `append` value) (getReverse biTrie))

insertTrie key trie biTrie =
  foldr (insert key) biTrie $ T.paths trie

reverseDelete value = delete' value reverse forward

delete key = delete' key forward reverse

delete' path a b biTrie =
  L.set b (foldr (\p -> T.subtract (p `append` value)) (b biTrie)
           $ T.paths $ T.findTrie value $ a biTrie)
  $ L.set a (T.subtractTries value $ a biTrie) biTrie

reverseSubtract values = subtract' values reverseDelete

subtract keys = subtract' keys delete

subtract' trie visit biTrie =
  foldr visit biTrie $ T.paths trie

reverseFind value = T.findTrie value . getReverse

find key = T.findTrie key . getForward
