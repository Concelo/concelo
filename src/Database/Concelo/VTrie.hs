{-# LANGUAGE LambdaCase #-}
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
  , sub
  , singleton
  , super
  , superValue
  , isolate
  , index
  , union
  , intersectL
  , intersectR
  , Database.Concelo.VTrie.subtract
  , subtractAll
  , diff
  , mergeL
  , mergeR ) where

import qualified Database.Concelo.VMap as V
import qualified Database.Concelo.TrieLike as TL
import qualified Database.Concelo.Path as P
import qualified Control.Lens as L

import Data.Maybe (isNothing, isJust, fromMaybe)
import Control.Applicative ((<|>))

data Versioned v = Versioned { _getVersionedVersion :: Integer
                             , getVersionedValue :: v }

versionedValue = L.lens getVersionedValue (\x v -> x { getVersionedValue = v })

data VTrie k v = VTrie { getVTrieVersioned :: Maybe (Versioned v)
                       , getVTrieMap :: V.VMap k (VTrie k v) }

instance Functor Versioned where
  fmap = L.over versionedValue

instance Ord k => Functor (VTrie k) where
  fmap f (VTrie v m) = VTrie (fmap (fmap f) v) (fmap (fmap f) m)

instance Foldable (VTrie k) where
  foldr visit seed (VTrie v m) = case v of
    Nothing -> below
    Just (Versioned _ value) -> visit value below
    where below = foldr (\t r -> foldr visit r t) seed m

instance TL.TrieLike VTrie where
  value = value'
  member = member'
  foldrPairs = foldrPairs'

vtrie = VTrie

isEmpty (VTrie v m) = isNothing v && null m

empty = VTrie Nothing V.empty

isLeaf (VTrie _ m) = null m

leaf v = VTrie (Just v) V.empty

value = value'

value' trie = getVersionedValue <$> getVTrieVersioned trie

firstPath (VTrie v m)
  | null m = P.leaf . getVersionedValue <$> v
  | otherwise = V.first m >>= (\(k, t) -> P.super k <$> firstPath t)

first (VTrie v m)
  | null m = getVersionedValue <$> v
  | otherwise = V.first m >>= first . snd

find path trie = case P.sub path of
  Nothing -> trie
  Just (k, p) -> find p $ sub k trie

findValue path trie =
  getVersionedValue <$> (getVTrieVersioned $ find path trie)

member :: Ord k => P.Path k v0 -> VTrie k v -> Bool
member = member'

member' path = isJust . findValue path

hasAll large = foldrPaths (\p -> (TL.member p large &&)) True

hasAny large = foldrPaths (\p -> (TL.member p large ||)) False

foldrPairs' visit seed (VTrie _ m) = V.foldrPairs visit seed m

foldrPathsAndValues visit seed (VTrie v m) =
  case v of
    Nothing -> below
    Just (Versioned _ value) -> visit (P.leaf value, value) below
  where
    below =
      V.foldrPairs
      (\(k, t) r ->
        foldrPathsAndValues
        (\(p, v) ->
          visit (P.super k p, v))
        r t) seed m

pathsAndValues = foldrPathsAndValues (:) []

foldrKeys visit seed (VTrie _ m) = V.foldrPairs (visit . fst) seed m

keys = foldrKeys (:) []

foldrTriples visit seed (VTrie _ m) =
  V.foldrPairs (\(k, VTrie v m) -> visit (k, getVersionedValue <$> v, m))
  seed m

triples = foldrTriples (:) []

foldrPaths visit = foldrPathsAndValues (visit . fst)

paths = foldrPaths (:) []

sub key = fromMaybe empty . V.lookup key . getVTrieMap

isolate version key = super version key . sub key

singleton version key = super version key . leaf

single version key trie
  | isEmpty trie = V.empty
  | otherwise = V.singleton version key trie

superValue version value key =
  VTrie (Just (Versioned version value)) . single version key

super version key = VTrie Nothing . single version key

index version f = foldrPathsAndValues visit empty where
  visit (p, v) = union version $ f p v

union :: (TL.TrieLike t, Ord k) =>
         Integer -> t k v -> VTrie k v -> VTrie k v
union version small (VTrie largeVersioned largeMap) =
  let v = TL.value small <|> (getVersionedValue <$> largeVersioned) in

  VTrie (Versioned version <$> v) $ TL.foldrPairs visit largeMap small where
    visit (k, a) = V.insert version k $ union version a $ fromMaybe empty
                   $ V.lookup k largeMap

intersectL :: (TL.TrieLike t, Ord k) =>
              Integer -> t k v -> VTrie k v -> VTrie k v
intersectL = intersect $ \a _ -> a

intersectR :: (TL.TrieLike t, Ord k) =>
              Integer -> t k v -> VTrie k v -> VTrie k v
intersectR = intersect $ \_ b -> b

intersect :: (TL.TrieLike t, Ord k) =>
             (Maybe v -> Maybe v -> Maybe v) ->
             Integer ->
             t k v ->
             VTrie k v ->
             VTrie k v
intersect pick version small (VTrie largeVersioned largeMap) =
  let sv = TL.value small
      lv = getVersionedValue <$> largeVersioned
      v = sv >> lv >> pick sv lv in

  VTrie (Versioned version <$> v) $ TL.foldrPairs visit V.empty small where
    visit (k, a) = case V.lookup k largeMap of
      Nothing -> id
      Just b -> let trie = intersect pick version a b in
        if isEmpty trie then
          id
        else
          V.insert version k trie

subtract :: (TL.TrieLike t, Ord k) =>
            Integer -> t k v -> VTrie k v -> VTrie k v
subtract = subtract'

subtract' :: (TL.TrieLike t, Ord k) =>
             Integer -> t k v -> VTrie k v -> VTrie k v
subtract' version small (VTrie largeVersioned largeMap) =
  let sv = TL.value small
      lv = getVersionedValue <$> largeVersioned
      v = if isNothing sv then lv else Nothing in

  VTrie (Versioned version <$> v) $ TL.foldrPairs visit largeMap small where
    visit (k, a) = case V.lookup k largeMap of
      Nothing -> id
      Just b -> let trie = subtract' version a b in
        if isEmpty trie then
          V.delete version k
        else
          V.insert version k trie

subtractAll :: (TL.TrieLike t, Ord k) =>
               Integer -> t k v -> VTrie k v -> VTrie k v
subtractAll version small (VTrie largeVersioned largeMap) =
  let sv = TL.value small
      lv = getVersionedValue <$> largeVersioned in

  if isJust sv then
    empty
  else
    VTrie (Versioned version <$> lv) $ TL.foldrPairs visit largeMap small where
      visit (k, a) = case V.lookup k largeMap of
        Nothing -> id
        Just b -> let trie = subtractAll version a b in
          if isEmpty trie then
            V.delete version k
          else
            V.insert version k trie

diffMaps version a b =
  V.foldrDiff visit (V.empty, V.empty) a b where
    visit key maybeA maybeB result@(obsoletes, news) = case maybeA of
      Just a -> case maybeB of
        Just b -> let (obsolete, new) = diff version a b in
          (if null obsolete then
             obsoletes
           else
             V.insert version key obsolete obsoletes,

           if null new then
             news
           else
             V.insert version key new news)

        Nothing -> (V.insert version key a obsoletes, news)

      Nothing -> case maybeB of
        Just b -> (obsoletes, V.insert version key b news)

        Nothing -> result

diff version (VTrie aV aM) (VTrie bV bM) =

  let (obsolete, new) = diffMaps version aM bM in

  fromMaybe (VTrie aV obsolete, VTrie bV new)
    (do (Versioned aR _) <- aV
        (Versioned bR _) <- bV

        if aR == bR then
          Just (VTrie Nothing obsolete, VTrie Nothing new)
          else
          Nothing)

data MergePref = LeftPref | RightPref deriving (Eq)

mergeL :: Ord k => Integer -> VTrie k v -> VTrie k v -> VTrie k v -> VTrie k v
mergeL = merge LeftPref

mergeR :: Ord k => Integer -> VTrie k v -> VTrie k v -> VTrie k v -> VTrie k v
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
