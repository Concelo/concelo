module Database.Concelo.VMap
  ( VMap()
  , empty
  , key
  , value
  , member
  , lookup
  , first
  , insert
  , modify
  , delete
  , pairs
  , triples
  , index
  , foldrDiff
  , diff ) where

import qualified Database.Concelo.Map as M
import qualified Data.Tree.RBTree as T
import qualified Control.Lens as L

newtype VMap k v = VMap { run :: T.RBTree (Cell k v) }

data Cell k v = Cell { getCellVersion :: Integer
                     , getCellKey :: k
                     , getCellValue :: v }

cellVersion =
  L.lens getCellVersion (\x v -> x { getCellVersion = v })

cellKey =
  L.lens getCellKey (\x v -> x { getCellKey = v })

cellValue =
  L.lens getCellValue (\x v -> x { getCellValue = v })

instance Ord k => Ord (Cell k v) where
  compare a b = compare (getCellKey a) (getCellKey b)

compareKey k c = compare k (getCellKey c)

cellVersionsEqual (Cell a _ _) (Cell b _ _) = a == b

empty = VMap T.Leaf

maybeCell = \case
  VMap T.Leaf -> Nothing
  VMap (T.Node _ c _ _) -> Just c

key map = getCellKey <$> maybeCell map

value map = getCellValue <$> maybeCell map

member key map = maybe False (const True) $ find key map

lookup key map = getCellValue <$> find key map

find key (VMap tree) = T.searchFast (\k c -> compare k $ getCellKey c) tree key

first (VMap tree) = pairKV <$> maybeCell (T.first tree)

insert version key value map = modify version key (const $ Just value) map

modify version key transform (VMap tree) =
  VMap $ T.modifyVersioned (L.set cellVersion version) compareKey tree
  key ((Cell version key <$>) . transform . (getCellValue <$>))

delete version key map = modify version key (const Nothing) map

foldrPairs visit seed = foldr (visit . pairKV) seed . run

pairs = foldrPairs (:) []

foldrTriples visit seed = foldr (visit . triple) seed . run

triples = foldrTriples (:) []

index version f = foldr (\v -> insert version (f v) v) empty

pairKV (Cell _ _ k v) = (k, v)

pairRV (Cell _ r _ v) = (r, v)

triple (Cell _ r k v) = (r, k, v)

visitor visit a b =
  visit
  (fromJust ((getCellKey <$> a) <|> (getCellKey <$> b)))
  (pairRV <$> a)
  (pairRV <$> b)

foldrDiff visit seed a b =
  T.foldrDiffVersioned cellVersionsEqual compare (visitor visit) seed a b

insertCell (Just (_, k, v)) map = M.insert k v map
insertCell _ map = map

diff a b =
  foldrDiff (\a b (as, bs) -> (insertCell a as, insertCell b bs))
  (M.empty, M.empty) a b
