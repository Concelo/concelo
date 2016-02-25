module Database.Concelo.Trie
  ( Trie()
  , empty
  , key
  , value
  , insert
  , modify
  , delete
  , sub
  , super
  , diff ) where

import qualified Database.Concelo.Map as M
import qualified Control.Lens as L

newtype Trie k v = Trie (M.Map k (Cell k v))

data Cell k v = Cell { getCellValue :: Maybe v
                     , getCellSubTrie :: Trie k v }

cellValue = L.lens getCellValue (\x v -> x { getCellValue = v })
cellSubTrie = L.lens getCellSubTrie (\x v -> x { getCellSubTrie = v })

empty = Trie M.empty

key (Trie map) = M.key map

value (Trie map) = M.value map >>= getCellValue

subTrie (Trie map) = getCellSubTrie <$> M.value map

insert revision key value trie = modify revision key (const $ Just value) trie

modify revision key transform (Trie map) =
  Trie $ M.modify revision key
  (\case
      Nothing -> case transform Nothing of
        Nothing -> Nothing

        v -> Just $ Cell v empty

      Just (Cell v sub) -> case transform v of
        Nothing -> if null sub then
                     Nothing
                   else
                     Just $ Cell Nothing sub

        v -> Just $ Cell v sub)
  map

delete revision key trie = modify revision key (const Nothing) trie

sub key (Trie map) = maybe empty getCellSubTrie $ find key map

super revision key value trie =
  Trie $ M.insert revision key (Cell value trie) empty

insertCell (Just (M.Cell r k v)) (Trie map) = Trie $ M.insert r k v map
insertCell Nothing trie = trie

diff (Trie a) (Trie b) =
  M.foldrDiff
  (\maybeA maybeB result@(as, bs) ->
    case (maybeA, maybeB) of
      (Just a, Nothing) -> (insertCell a as, bs)
      (Nothing, Just b) -> (as, insertCell b bs)
      (Just a, Just b) ->
        let (subAs, subBs) = diff (getCellSubTrie a) (getCellSubTrie b) in
        (insertCell (L.set cellSubTrie subAs a) as,
         insertCell (L.set cellSubTrie subBs a) bs)

      _ -> result)
  (empty, empty) a b
