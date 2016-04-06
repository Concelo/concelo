{-# LANGUAGE LambdaCase #-}
module Database.Concelo.VMap
  ( VMap()
  , empty
  , singleton
  , member
  , Database.Concelo.VMap.lookup
  , first
  , firstValue
  , insert
  , modify
  , delete
  , foldrKeys
  , keys
  , foldrPairs
  , pairs
  , foldrTriples
  , triples
  , index
  , union
  , Database.Concelo.VMap.subtract
  , foldrDiff
  , diff ) where

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

member key map = maybe False (const True) $ find key map

lookup key map = getCellValue <$> find key map

find key (VMap tree) = T.searchFast (\k c -> compare k $ getCellKey c) tree key

first (VMap tree) = pairKV <$> T.first tree

firstValue map = snd <$> first map

insert version key value = modify version key (const $ Just value)

modify version key transform (VMap tree) =
  VMap $ T.modifyVersioned (L.set cellVersion version) compareKey tree
  key ((Cell version key <$>) . transform . (getCellValue <$>))

delete version key = modify version key (const Nothing)

foldrKeys visit seed = foldr (visit . getCellKey) seed . run

keys = foldrKeys (:) []

foldrPairs visit seed = foldr (visit . pairKV) seed . run

pairs = foldrPairs (:) []

foldrTriples visit seed = foldr (visit . triple) seed . run

triples = foldrTriples (:) []

index version f = foldr (\v -> insert version (f v) v) empty

union version small large =
  foldrPairs (\(k, v) -> insert version k v) large small

subtract version small large = foldrKeys (delete version) large small

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

diff version a b = foldrDiff visit (empty, empty) a b where
  visit k a b (obsolete, new) =
    (maybe obsolete (flip (insert version k) obsolete) a,
     maybe new (flip (insert version k) new) b)
