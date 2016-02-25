module Database.Concelo.Map
  ( Map()
  , Cell(Cell)
  , getCellRevision
  , getCellKey
  , getCellValue
  , cellRevision
  , cellKey
  , cellValue
  , empty
  , key
  , value
  , member
  , lookup
  , insert
  , modify
  , delete
  , foldrDiff
  , diff ) where

import qualified Data.Tree.RBTree as T
import qualified Control.Lens as L

newtype Map k v = Map (T.RBTree (Cell k v))

data Cell k v = Cell { getCellRevision :: Integer
                     , getCellKey :: k
                     , getCellValue :: v }

cellRevision = L.lens getCellRevision (\x v -> x { getCellRevision = v })
cellKey = L.lens getCellKey (\x v -> x { getCellKey = v })
cellValue = L.lens getCellValue (\x v -> x { getCellValue = v })

instance Ord k => Ord (Cell k v) where
  compare a b = compare (getCellKey a) (getCellKey b)

compareKey k c = compare k (getCellKey c)

cellRevisionsEqual (Cell a _ _) (Cell b _ _) = a == b

empty = Map T.Leaf

maybeCell = \case
  Map T.Leaf -> Nothing
  Map (T.Node _ c _ _) -> Just c

key tree = getCellKey <$> maybeCell tree

value tree = getCellValue <$> maybeCell tree

member key map = maybe False (const True) $ find key map

lookup key map = getCellValue <$> find key map

find key (Map tree) = T.searchFast (\k c -> compare k $ getCellKey c) tree key

insert revision key value map = modify key (const $ Just value) map

modify revision key transform (Map tree) =
  Map $ T.modifyVersioned (L.set cellRevision revision) compareKey tree key
  ((Cell revision key <$>) . transform . (getCellValue <$>))

delete revision key map = modify key (const Nothing) map

insertCell (Just (Cell r k v)) map = insert r k v map
insertCell _ map = map

foldrDiff visit seed a b =
  T.foldrDiffVersioned cellRevisionsEqual compare visit seed a b

diff a b =
  T.foldrDiffVersioned cellRevisionsEqual compare
  (\a b (as, bs) -> (insertCell a as, insertCell b bs)) (empty, empty) a b
