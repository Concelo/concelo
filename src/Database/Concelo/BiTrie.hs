module Database.Concelo.BiTrie
  ( empty ) where

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

reverseDelete value = delete' value reverse forward

delete value = delete' value forward reverse

delete' path a b biTrie =
  L.set b (foldr (\p -> T.subtract (p `append` value)) (b biTrie)
           $ T.paths $ T.findTrie value $ a biTrie)
  $ L.set a (T.subtractTries value $ a biTrie) biTrie

reverseFind value = T.findTrie value . getReverse

find key = T.findTrie key . getForward
