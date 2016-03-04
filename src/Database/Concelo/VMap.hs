module Database.Concelo.VMap
  ( VMap()
  , empty
  , key
  , value
  , member
  , lookup
  , first
  , insert
  , insertVersioned
  , modify
  , modifyVersioned
  , delete
  , triples
  , index
  , foldrDiff
  , diff ) where

import qualified Database.Concelo.Map as M
import qualified Data.Tree.RBTree as T
import qualified Control.Lens as L

newtype VMap k v = VMap { run :: T.RBTree (Cell k v) }

instance M.FoldableWithKey VMap where
  foldrWithKey visit seed = foldr (\(Cell _ _ k v) r -> visit k v r) seed . run

data Cell k v = Cell { getCellTreeVersion :: Integer
                     , getCellValueVersion :: Integer
                     , getCellKey :: k
                     , getCellValue :: v }

cellValueVersion =
  L.lens getCellValueVersion (\x v -> x { getCellValueVersion = v })

cellTreeVersion =
  L.lens getCellTreeVersion (\x v -> x { getCellTreeVersion = v })

cellKey =
  L.lens getCellKey (\x v -> x { getCellKey = v })

cellValue =
  L.lens getCellValue (\x v -> x { getCellValue = v })

instance Ord k => Ord (Cell k v) where
  compare a b = compare (getCellKey a) (getCellKey b)

compareKey k c = compare k (getCellKey c)

cellTreeVersionsEqual (Cell a _ _ _) (Cell b _ _ _) = a == b

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

insert version key value map = insertVersioned version key version value map

insertVersioned treeVersion key valueVersion value map =
  modifyVersioned treeVersion key valueVersion (const $ Just value) map

modify version key value map = modifyVersioned version key version value map

modifyVersioned treeVersion key valueVersion transform (VMap tree) =
  VMap $ T.modifyVersioned (L.set cellTreeVersion treeVersion) compareKey tree
  key ((Cell treeVersion valueVersion key <$>) . transform
       . (getCellValue <$>))

delete version key map = modify version key (const Nothing) map

triples = fmap triple . toList . run

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
  T.foldrDiffVersioned cellTreeVersionsEqual compare (visitor visit) seed a b

insertCell (Just (_, k, v)) map = M.insert k v map
insertCell _ map = map

diff a b =
  foldrDiff (\a b (as, bs) -> (insertCell a as, insertCell b bs))
  (M.empty, M.empty) a b
