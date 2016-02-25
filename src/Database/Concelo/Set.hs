module Database.Concelo.Set
  ( Set()
  , Cell(Cell)
  , getCellRevision
  , getCellValue
  , cellRevision
  , cellValue
  , empty
  , value
  , insert
  , delete
  , foldrDiff
  , diff ) where

import qualified Database.Concelo.Map as M
import qualified Control.Lens as L

newtype Set v = Set (M.Map v ())

data Cell v = Cell { getCellRevision :: Integer
                     , getCellValue :: v }

cellRevision = L.lens getCellRevision (\x v -> x { getCellRevision = v })
cellValue = L.lens getCellValue (\x v -> x { getCellValue = v })

empty = Set M.empty

value (Set map) = M.key map

insert revision value (Set map) = Set $ M.insert revision value () map

delete revision value (Set map) = Set $ M.delete revision value map

myCell (M.Cell r k _) = Cell r k

insertCell (Just (M.Cell r k _)) set = insert r k set
insertCell _ set = set

foldrDiff visit seed (Set a) (Set b) =
  M.foldrDiff (\a b r -> visit (myCell <$> a) (myCell <$> b) r) seed a b

diff (Set a) (Set b) =
  M.foldrDiff (\a b (as, bs) -> (insertKey a as, insertKey b bs))
  (empty, empty) a b
