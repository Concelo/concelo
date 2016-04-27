{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
module Database.Concelo.VTrie
  ( VTrie()
  , isEmpty
  , empty
  , isLeaf
  , leaf
  , value
  , firstPath
  , firstValue
  , lastPath
  , lastValue
  , findTrie
  , findValue
  , member
  , hasAll
  , hasAny
  , foldrPairs
  , pairs
  , foldrPaths
  , paths
  , foldrPathsAndValues
  , pathsAndValues
  , foldrKeys
  , keys
  , foldrTriples
  , triples
  , fromTrieLike
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

instance (Show k, Show v) => Show (VTrie k v) where
  show = show . paths

instance (Eq k, Eq v) => Eq (VTrie k v) where
  a == b = paths a == paths b

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
  foldrPaths = foldrPaths'

isEmpty (VTrie v m) = isNothing v && null m

empty = VTrie Nothing V.empty

isLeaf (VTrie _ m) = null m

leaf version value = VTrie (Just (Versioned version value)) V.empty

value = value'

value' trie = getVersionedValue <$> getVTrieVersioned trie

firstPath (VTrie v m)
  | null m = P.leaf . getVersionedValue <$> v
  | otherwise = V.first m >>= (\(k, t) -> P.super k <$> firstPath t)

firstValue (VTrie v m)
  | null m = getVersionedValue <$> v
  | otherwise = V.first m >>= firstValue . snd

lastPath (VTrie v m)
  | null m = P.leaf . getVersionedValue <$> v
  | otherwise = V.last m >>= (\(k, t) -> P.super k <$> lastPath t)

lastValue (VTrie v m)
  | null m = getVersionedValue <$> v
  | otherwise = V.last m >>= lastValue . snd

findTrie path trie = case P.sub path of
  Nothing -> trie
  Just (k, p) -> findTrie p $ sub k trie

findValue path trie =
  getVersionedValue <$> (getVTrieVersioned $ findTrie path trie)

member = member'

member' path = isJust . findValue path

hasAll large = foldrPaths (\p -> (TL.member p large &&)) True

hasAny large = foldrPaths (\p -> (TL.member p large ||)) False

foldrPairs = foldrPairs'

foldrPairs' visit seed (VTrie _ m) = V.foldrPairs visit seed m

pairs = foldrPairs (:) []

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

foldrPaths = foldrPaths'

foldrPaths' visit = foldrPathsAndValues (visit . fst)

paths = foldrPaths (:) []

fromTrieLike version trieLike = union version trieLike empty

sub key = fromMaybe empty . V.lookup key . getVTrieMap

isolate version key = super version key . sub key

singleton version key = super version key . leaf version

single version key trie
  | isEmpty trie = V.empty
  | otherwise = V.singleton version key trie

superValue version maybeValue key =
  VTrie (Versioned version <$> maybeValue) . single version key

super version key = VTrie Nothing . single version key

index version f = foldr visit empty where
  visit v = union version $ f v

union version small (VTrie largeVersioned largeMap) =
  let v = TL.value small <|> (getVersionedValue <$> largeVersioned) in

  VTrie (Versioned version <$> v) $ TL.foldrPairs visit largeMap small where
    visit (k, a) = V.insert version k $ union version a $ fromMaybe empty
                   $ V.lookup k largeMap

intersectL = intersect $ \a _ -> a

intersectR = intersect $ \_ b -> b

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

subtract = subtract'

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

mergeL :: Ord k =>
          Integer ->
          VTrie k v ->
          VTrie k v ->
          VTrie k v ->
          VTrie k v
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
