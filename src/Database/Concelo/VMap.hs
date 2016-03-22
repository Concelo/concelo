{-# LANGUAGE LambdaCase #-}
module Database.Concelo.VMap
  ( VMap()
  , empty
  , singleton
  , key
  , value
  , member
  , Database.Concelo.VMap.lookup
  , first
  , insert
  , modify
  , delete
  , foldrPairs
  , pairs
  , foldrTriples
  , triples
  , index
  , foldrDiff ) where

import qualified Data.Tree.RBTree as T
import qualified Control.Lens as L
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

newtype VMap k v = VMap { run :: T.RBTree (Cell k v) }

instance Ord k => Functor (VMap k) where
  fmap f = foldrTriples (\(r, k, v) -> insert r k $ f v) empty

instance Foldable (VMap k) where
  foldr visit seed = foldr (\(Cell _ _ v) -> visit v) seed . run

data Cell k v = Cell { getCellVersion :: Integer
                     , getCellKey :: k
                     , getCellValue :: v }

cellVersion =
  L.lens getCellVersion (\x v -> x { getCellVersion = v })

instance Eq k => Eq (Cell k v) where
  a == b = (getCellKey a) == (getCellKey b)

instance Ord k => Ord (Cell k v) where
  compare a b = compare (getCellKey a) (getCellKey b)

compareKey k c = compare k (getCellKey c)

cellVersionsEqual (Cell a _ _) (Cell b _ _) = a == b

empty = VMap T.Leaf

singleton version key value = insert version key value empty

maybeCell = \case
  VMap T.Leaf -> Nothing
  VMap (T.Node _ c _ _) -> Just c

key map = getCellKey <$> maybeCell map

value map = getCellValue <$> maybeCell map

member key map = maybe False (const True) $ find key map

lookup key map = getCellValue <$> find key map

find key (VMap tree) = T.searchFast (\k c -> compare k $ getCellKey c) tree key

first (VMap tree) = pairKV <$> T.first tree

insert version key value = modify version key (const $ Just value)

modify version key transform (VMap tree) =
  VMap $ T.modifyVersioned (L.set cellVersion version) compareKey tree
  key ((Cell version key <$>) . transform . (getCellValue <$>))

delete version key = modify version key (const Nothing)

foldrPairs visit seed = foldr (visit . pairKV) seed . run

pairs = foldrPairs (:) []

foldrTriples visit seed = foldr (visit . triple) seed . run

triples = foldrTriples (:) []

index version f = foldr (\v -> insert version (f v) v) empty

pairKV (Cell _ k v) = (k, v)

triple (Cell r k v) = (r, k, v)

visitor visit a b =
  visit
  (fromJust ((getCellKey <$> a) <|> (getCellKey <$> b)))
  (getCellValue <$> a)
  (getCellValue <$> b)

foldrDiff visit seed a b =
  T.foldrDiffVersioned cellVersionsEqual compare (visitor visit) seed
  (run a) (run b)
