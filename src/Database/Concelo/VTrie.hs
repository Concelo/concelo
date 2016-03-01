module Database.Concelo.VTrie
  ( VTrie()
  , empty
  , key
  , value
  , firstPath
  , first
  , find
  , findValue
  , findTrie
  , member
  , paths
  , insert
  , modify
  , delete
  , sub
  , super
  , singleton
  , union
  , intersectL
  , intersectR
  , subtract
  , subtractAll
  , diff ) where

import qualified Database.Concelo.VMap as V
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Path as P
import qualified Control.Lens as L

newtype VTrie k v = VTrie { run :: V.VMap k (Cell k v) }

instance Functor VTrie where
  fmap f = VTrie . foldr visit V.empty . V.triples . run where
    visit (r, k, (Cell v sub)) = V.insert r k (Cell (f v) (fmap f sub))

instance M.FoldableWithKey VTrie where
  foldrWithKey visit seed = foldrWithKey visit seed . run

data Cell k v = Cell { getCellValue :: Maybe v
                     , getCellSubTrie :: VTrie k v }

cellValue = L.lens getCellValue (\x v -> x { getCellValue = v })
cellSubTrie = L.lens getCellSubTrie (\x v -> x { getCellSubTrie = v })

cellIsEmpty (Cell v sub) = isNothing v && null sub

empty = VTrie V.empty

key = V.key . run

value (VTrie map) = V.value map >>= getCellValue

subTrie (VTrie map) = getCellSubTrie <$> V.value map

firstPath (VTrie map) =
  V.first map >>= \(k, (Cell v sub)) ->
  if null sub then
    Just $ P.singleton k v
  else
    P.super k Nothing <$> firstPath sub

first (VTrie map) =
  V.first map >>= \(_, (Cell v sub)) ->
  if null sub then
    Just v
  else
    first sub

find path trie@(VTrie map) =
  case P.key path of
    Nothing -> Nothing
    Just k ->
      let subPath = P.sub path
      if null subPath then
        (\v -> (P.singleton k v, v)) <$> value trie
      else
        (\(p, v) -> (P.super k, v))
        <$> ((getCellSubTrie <$> V.lookup k map) >>= find subPath)

findValue path trie = snd <$> find path trie

member path = isJust . find path

paths trie@(VTrie map) =
  M.foldrWithKey visit [] map where
    visit key (Cell v sub) result =
      (P.super key <$> paths sub)
      ++ maybe result (\v -> P.singleton key v : result) v

findTrie path trie =
  case P.key path of
    Nothing -> trie
    Just k -> find' (P.sub path) (sub k trie)

insert version key value trie = modify version key (const $ Just value) trie

modify version key transform (VTrie map) =
  VTrie $ V.modify version key
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

delete version key trie = modify version key (const Nothing) trie

sub key (VTrie map) = maybe empty getCellSubTrie $ find key map

superKV version key value trie =
  let new = Cell value trie in
  if cellIsEmpty new then
    empty
  else
    VTrie $ V.insert version key (Cell value trie) empty

super version key = superKV version key Nothing

singleton version key value = superKV version key (Just value) empty

union version small (VTrie large) =
  VTrie $ M.foldrWithKey visit large small where
    visit k cell@(Cell sv ss) result = case V.lookup k large of
      Nothing ->
        V.insert version k cell result
      Just (Cell lv ls) ->
        V.insert version k (Cell (sv <|> lv) (union version ss ls)) result

intersectL = instersect $ \a _ -> a

intersectR = instersect $ \_ b -> b

intersect pick version small (VTrie large) =
  VTrie $ M.foldrWithKey visit empty small where
    visit k cell@(Cell sv ss) result = case V.lookup k large of
      Nothing ->
        result
      Just (Cell lv ls) ->
        let new = Cell (sv >> lv >> pick sv lv) (intersect version ss ls) in
        if cellIsEmpty new then
          result
        else V.insert version k new result

subtract version small (VTrie large) =
  VTrie $ M.foldrWithKey visit large small where
    visit k cell@(Cell sv ss) result = case V.lookup k large of
      Nothing ->
        result
      Just (Cell lv ls) ->
        let new = Cell (if isNothing sv then lv else Nothing)
                  (subtract version ss ls) in
        if cellIsEmpty new then
          V.delete version k result
        else
          V.insert version k new result

subtractAll version small (VTrie large) =
  VTrie $ M.foldrWithKey visit large small where
    visit k cell@(Cell sv ss) result = case V.lookup k large of
      Nothing ->
        result
      Just (Cell lv ls) ->
        let new = Cell lv (subtract version ss ls) in
        if isJust sv || cellIsEmpty new then
          V.delete version k result
        else
          V.insert version k new result

insertCell (Just (V.Cell r k v)) (VTrie map) = VTrie $ V.insert r k v map
insertCell Nothing trie = trie

diff (VTrie a) (VTrie b) =
  V.foldrDiff
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
