module Database.Concelo.VTrie
  ( VTrie()
  , vtrie
  , isEmpty
  , empty
  , isLeaf
  , leaf
  , value
  , firstPath
  , first
  , find
  , findValue
  , member
  , hasAll
  , hasAny
  , foldrPaths
  , paths
  , foldrPathsAndValues
  , pathsAndValues
  , foldrKeys
  , keys
  , foldrTriples
  , triples
  , values
  , sub
  , super
  , superValue
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
import qualified Database.Concelo.TrieLike as TL
import qualified Database.Concelo.Path as P
import qualified Control.Lens as L

data Versioned v = { getVersionedVersion :: Integer
                   , getVersionedValue :: v }

data VTrie k v = VTrie { getVTrieVersioned :: Maybe (Versioned v)
                       , getVTrieMap :: V.VMap k (VTrie k v) }

instance Functor Versioned where
  fmap = L.over versionedValue

instance Functor (VTrie k) where
  fmap f (VTrie v m) = VTrie (fmap f v) (fmap (fmap f) m)

instance Foldable (VTrie k) where
  foldr visit seed (VTrie v m) = case v of
    Nothing -> below
    Just value -> visit value below
  where
    below = foldr (\t r -> foldr visit r t) seed m

instance TrieLike VTrie where
  value = value'
  member = member'
  foldrPairs = foldrPairs'

vtrie = VTrie

isEmpty (VTrie v m) = isNothing v && null m

empty = VTrie Nothing V.empty

isLeaf (VTrie _ m) = null m

leaf v = VTrie (Just v) V.empty

value = value'

value trie = getVersionedValue <$> getVTrieVersioned trie

firstPath (VTrie v m)
  | null m = P.root . getVersionedValue <$> v
  | otherwise = ((k, t) -> P.super k <$> firstPath t) <$> V.first m

first (VTrie v m)
  | null m = getVersionedValue <$> v
  | otherwise = first . snd <$> V.first m

find path trie = case P.subPair path of
  Nothing -> trie
  Just (k, sub) -> find sub $ sub k trie

findValue path trie =
  getVersionedValue <$> (getVTrieVersioned =<< find path trie)

member = member'

member' path = isJust . findValue path

hasAll large = foldrPaths (\p -> (TL.member p large &&)) True

hasAny large = foldrPaths (\p -> (TL.member p large ||)) False

foldrPairs' visit seed (VTrie _ m) = V.foldrKeysAndValues visit seed m

foldrPathsAndValues visit seed (VTrie v m) =
  case v of
    Nothing -> below
    Just value -> (P.root value, value) : below
  where
    below =
      V.foldrKeysAndValues
      (\(k, v) r ->
        foldrPathsAndValues
        (\(p, v) ->
          visit (P.super k p, v))
       r t) m

pathsAndValues = foldrPathsAndValues (:) []

foldrKeys visit seed (VTrie _ m) = V.foldrKeysAndValues (visit . fst) seed m

keys = foldrKeys (:) []

foldrTriples visit seed (VTrie _ m) =
  V.foldrKeysAndValues
  (\(k, VTrie v m) -> visit (k, getVersionedValue <$> v, m)) seed m

triples = foldrTriples (:) []

foldrPaths visit = foldrPathsAndValues (visit . fst)

paths trie = foldrPaths (:) []

values = foldr (:) []

sub key = fromMaybe empty . V.lookup key . map

single version key trie
  | isEmpty trie = V.empty
  | otherwise = V.singleton version key trie

superValue version value key =
  VTrie (Just (Versioned version value)) . single version key

super version key = VTrie Nothing . single version key

union version small (VTrie largeValue largeMap) =
  let v = (TL.value small <|> = largeValue in

  VTrie v $ TL.foldrPairs visit largeMap small where
    visit (k, a) = V.insert version k case V.lookup k largeMap of
      Nothing -> a
      Just b -> union version a b

intersectL = instersect $ \a _ -> a

intersectR = instersect $ \_ b -> b

intersect pick version small (VTrie largeValue largeMap) =
  let sv = TL.value small
      lv = largeValue
      v = sv >> lv >> pick sv lv in

  VTrie v $ TL.foldrPairs visit V.empty small where
    visit (k, a) = case V.lookup k largeMap of
      Nothing -> id
      Just b -> let trie = intersect pick version a b in
        if isEmpty trie then id else V.insert version k trie

subtract version small (VTrie largeValue largeMap) =
  let sv = TL.value small
      lv = largeValue
      v = if isNothing sv then lv else Nothing in

  VTrie v $ TL.foldrPairs visit large small where
    visit (k, a) = case V.lookup k largeMap of
      Nothing -> id
      Just b -> let trie = subtract version a b in
        if isEmpty trie then V.delete version k else V.insert version k trie

subtractAll version small (VTrie largeValue largeMap) =
  let sv = TL.value small
      lv = largeValue in

  if isJust sv then
    empty
  else
    VTrie lv $ TL.foldrPairs visit large small where
      visit (k, a) = case V.lookup k largeMap of
        Nothing -> id
        Just b -> let trie = subtract version a b in
          if isEmpty trie then V.delete version k else V.insert version k trie

diffMaps a b =
  V.foldrDiff visit (M.empty, M.empty) a b where
    visit key maybeA maybeB result@(obsoletes, news) = case maybeA of
      Just a -> case maybeB of
        Just b -> let (obsolete, new) = diff a b in
          (if null obsolete then
             obsoletes
           else
             M.insert key obsolete obsoletes,

           if null new then
             news
           else
             M.insert key new news)

        Nothing -> (M.insert key a obsoletes, news)

      Nothing -> case maybeB of
        Just b -> (obsoletes, M.insert key b news)

        Nothing -> result

diff (VTrie aV aM) (VTrie bV bM) =

  let (obsolete, new) = diffMaps aM bM in

  fromJust (T.trie aV obsolete, T.trie bV new) do
    (Versioned aR _) <- aV
    (Versioned bR _) <- bV

    if aR == aV then Just (T.Trie Nothing obsolete, T.Trie Nothing new)
      else Nothing

data MergePref = LeftPref | RightPref

mergeL = merge LeftPref
mergeR = merge RightPref

merge pref version base left right =
  mergeTries base left right where

    mergeTries (VTrie bV bM) (VTrie lV lM) (VTrie rV rM) =
      VTrie v $ mergeMaps bM lM rM where
        v = case bV of
          Just (Versioned bR _) -> case lV of
            Just (Versioned lR _) ->
              if lR == bR || pref == RightPref then rV else lV

            Nothing -> case rV of
              Just (Versioned rR _) ->
                if rR == bR || pref == LeftPref then lV else rV

              Nothing -> Nothing

          Nothing ->
            if isNothing lV || pref == RightPref then rV else lV

    mergeMaps b l r =
      V.foldrDiff (visit b) (if pref == LeftPref then l else r) l r

    maybeEmpty = fromMaybe empty

    visit base key maybeL maybeR =
      let merged = mergeTries
                   (maybeEmpty $ V.lookup key base)
                   (maybeEmpty maybeL)
                   (maybeEmpty maybeR) in

      if isEmpty merged then
        V.delete version key
      else
        V.insert version key merged
