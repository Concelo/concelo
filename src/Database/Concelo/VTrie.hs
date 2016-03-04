module Database.Concelo.VTrie
  ( VTrie()
  , empty
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
  , diff
  , mergeL
  , mergeR ) where

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

hasAll large small =
  findAll $ paths small where
    findAll [] = True
    findAll (p:ps) = member p large && findAll ps

hasAny large small =
  findAny $ paths small where
    findAny [] = False
    findAny (p:ps) = member p large || findAny ps

triples trie@(VTrie map) =
  M.foldrWithKey visit [] map where
    visit key (Cell v sub) result = (key, v, sub) : result

pathsAndValues trie@(VTrie map) =
  M.foldrWithKey visit [] map where
    visit key (Cell v sub) result =
      ((\(p, v) -> (P.super key p, v) <$> pathsAndValues sub)
      ++ maybe result (\v -> (P.singleton key v, v) : result) v

paths trie = fst <$> pathsAndValues trie

findTrie path trie =
  case P.key path of
    Nothing -> trie
    Just k -> find' (P.sub path) (sub k trie)

insert version key value = modify version key (const $ Just value)

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

delete version key = modify version key (const Nothing)

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

insertCell k v sub (VTrie map) = VTrie $ V.insert 0 k (Cell v sub) map

diff (VTrie a) (VTrie b) =
  V.foldrDiff visit (empty, empty) a b where
    visit key maybeA maybeB result@(obsoletes, news) = case maybeA of
      Just (aVersion, Cell aValue aSub) -> case maybeB of
        Just (bVersion, Cell bValue bSub) ->
          let (obsolete, new) = diff aSub bSub in
          if aVersion == bVersion then
            if null obsolete then
              if null new then
                result
              else
                (obsoletes,
                 insertCell key Nothing new news)
            else
              if null new then
                (insertCell key Nothing obsolete obsoletes,
                 news)
              else
                (insertCell key Nothing obsolete obsoletes,
                 insertCell key Nothing new news)
          else
            (insertCell key aValue obsolete obsoletes,
             insertCell key bValue new news)

        Nothing -> (insertCell key aValue aSub obsoletes,
                    news)
      Nothing -> case maybeB of
        Just b -> (obsoletes,
                   insertCell key bValue bSub news)
        Nothing -> result

data MergePref = LeftPref | RightPref

mergeL = merge LeftPref
mergeR = merge RightPref

insertCellVersioned tv vv k v sub =
  VTrie $ V.insertVersioned tv vv k (Cell v sub) . run

deleteCellVersioned tv k = VTrie $ V.delete tv k . run

merge pref version base left right =
  walk base left right where
    walk (VTrie base) (VTrie left) (Vtrie right) =
      V.foldrDiff (visit base) right left right

    visit base key maybeLeft maybeRight result =
      case V.lookupVersioned key base of
        Just (baseVersion, Cell baseValue baseSub) -> case maybeLeft of
          Just (leftVersion, Cell leftValue leftSub) ->
            let (rightVersion, Cell rightValue rightSub) =
                  fromMaybe (version, Cell Nothing empty) maybeRight

                sub = walk baseSub leftSub rightSub

                (valueVersion, value) =
                  if baseVersion == leftVersion || pref == RightPref then
                    (rightVersion, rightValue)
                  else
                    (leftVersion, leftValue) in

            if null sub && isNothing value then
              deleteCellVersioned version key result
            else
              insertCellVersioned version valueVersion key value sub result

          Nothing -> case maybeRight of
            Just (rightVersion, Cell rightValue rightSub) ->
              if baseVersion == rightVersion || pref == LeftPref then
                deleteCellVersioned version key result
              else
                result

            Nothing ->
              result

        Nothing -> case maybeLeft of
          Just (leftVersion, Cell leftValue leftSub) ->
            let left = insertCellVersioned version leftVersion key leftValue
                       leftSub result in
            case maybeRight of
              Just (rightVersion, Cell rightValue rightSub) -> case pref of
                LeftPref -> left
                RightPref -> result
            Nothing -> left
          Nothing -> result
