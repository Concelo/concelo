{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
module Database.Concelo.BiTrie
  ( BiTrie()
  , empty
  , trie
  , forwardTrie
  , reverseTrie
  , insert
  , insertTrie
  , reverseDelete
  , delete
  , reverseSubtract
  , Database.Concelo.BiTrie.subtract
  , reverseFind
  , find ) where

import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Char8 as BS
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as P
import qualified Control.Lens as L

data BiTrie k = BiTrie { getForward :: T.Trie k (T.Trie k ())
                       , getReverse :: T.Trie k (T.Trie k ()) }

instance {-# OVERLAPPABLE #-} Show k => Show (BiTrie k) where
  show = show . getForward

instance {-# OVERLAPPING #-} Show (BiTrie BS.ByteString) where
  show = show . getForward

forward :: L.Lens' (BiTrie k) (T.Trie k (T.Trie k ()))
forward = L.lens getForward (\x v -> x { getForward = v })

reverse' :: L.Lens' (BiTrie k) (T.Trie k (T.Trie k ()))
reverse' = L.lens getReverse (\x v -> x { getReverse = v })

empty = BiTrie T.empty T.empty

trie = getForward

forwardTrie = getForward
reverseTrie = getReverse

insert' key value trie =
  T.union (const values' <$> key) trie where
    values' = case T.findValue key trie of
      Nothing -> T.fromTrieLike value
      Just values -> T.union value values

insert key value biTrie = BiTrie
  (insert' key value (getForward biTrie))
  (insert' value key (getReverse biTrie))

insertTrie key trie biTrie =
  T.foldrPaths (insert key) biTrie trie

reverseDelete value = deleteFromBoth value reverse' forward

delete key = deleteFromBoth key forward reverse'

deleteFromBoth :: Ord k =>
                  P.Path k v ->
                  L.Lens' (BiTrie k) (T.Trie k (T.Trie k ())) ->
                  L.Lens' (BiTrie k) (T.Trie k (T.Trie k ())) ->
                  BiTrie k ->
                  BiTrie k
deleteFromBoth path a b biTrie =
  L.over b (deleteFromEach path
            $ fromMaybe T.empty
            $ T.findValue path
            $ L.view a biTrie)
  $ L.over a (T.subtract path) biTrie

deleteFromEach value keys trie =
  T.foldrPaths visit trie keys where
    visit key = case T.findValue key trie of
      Nothing -> id
      Just values ->
        let values' = T.subtract value values in
        if null values' then
          T.subtract key
        else
          T.union (const values' <$> key)

reverseSubtract values = subtract' values reverseDelete

subtract keys = subtract' keys delete

subtract' trie visit biTrie =
  T.foldrPaths visit biTrie trie

reverseFind value = fromMaybe T.empty . T.findValue value . getReverse

find key = fromMaybe T.empty . T.findValue key . getForward
