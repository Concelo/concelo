--
-- Module : RBTree
-- Author : Wu Xingbo
-- Copyright (c) 2010, 2011 Wu Xingbo (wuxb45@gmail.com)
-- New BSD License

{-# LANGUAGE BangPatterns #-}
-- |
--  Pure Haskell Red-Black tree implementation
--
module Data.Tree.RBTree (
  -- * Tree Types
  Color (Red, Black), RBTree (Node, Leaf), emptyRB,
  -- * Interval Types
  Interval (Interval), RealOrd (PInfinity, NInfinity, RealValue),
  -- * Insertion
  (<</), insert, insertOrd, insertOrdList, insertVersioned, unionVersioned,
  union,
  -- * Delete
  (<<\), delete, deleteOrd, deleteOrdList, deleteVersioned, subtractVersioned,
  intersectVersioned, rbSubtract, intersect,
  -- * Modify
  modify, modifyVersioned,
  -- * Search
  (<<?), search, searchOrd, searchFast, searchMax, searchMin,
  searchInterval, searchIntervalOrd, first, Data.Tree.RBTree.last,
  -- * Conversion
  fromList,
  -- * Difference
  foldrDiffVersioned, diffVersioned, diff,
  -- * Verification
  vD, vR,
)
where

import Prelude hiding (foldr)
import Control.Monad(liftM2)
import Data.Foldable (Foldable(foldr))

import qualified Data.Foldable as F

-- |Color of a 'Node'.
--  Leaf is assumed to be Black.
data Color =
    Red
  | Black
  deriving (Eq)

-- |Basic RBTree Structure.
data RBTree a = Node Color a !(RBTree a) !(RBTree a) -- ^A Node that holds an element and has two leaves.
              | Leaf  -- ^A Black leaf.

-- |Represents the direction of one step.
data Direction = ToLeft | ToRight deriving (Eq)

-- |Records the one step from a parent node to one of its children nodes.
data Step a = Step Color a Direction !(RBTree a) deriving (Show)

-- |A Path is a series of Steps.
type Path a = [Step a]

-- |RBTree in a 'Zip' mode.
--
--  Current Node can start from any node inside the tree, with a Path back to Root node.
--  RBZip is equivalent to RBTree in Logic.
--  All RBZip can be convert to a RBTree by Trace back to Root point.
data RBZip a =
  RBZip !(RBTree a) !(Path a)  -- ^ RBZip sub-tree path
  deriving (Show)

-- |used for range query.
data Interval a = Interval (RealOrd a, RealOrd a)

-- |Interval value from -INF to +INF.
data RealOrd a =
    PInfinity  -- ^positive infinity
  | NInfinity  -- ^positive infinity
  | RealValue a  -- ^Normal value, not need to be Ord.

-- |Simply show tree in (), hard to read but easy to parse.
instance Show a => Show (RBTree a) where
    show (Node c v l r) = "(" ++ show l ++ show v ++ show c ++ show r ++ ")"
    show Leaf = "."

-- |for distinguish Red/Black, show \'*\' for Red and nothing for Black.
instance Show Color where
    show Red = "*"
    show Black = ""

instance Show Direction where
    show ToLeft = "L"
    show ToRight = "R"

instance Show a => Show (RealOrd a) where
    show PInfinity = "+INF"
    show NInfinity = "-INF"
    show (RealValue a) = show a

instance Show a => Show (Interval a) where
    show (Interval (l, r)) = "[" ++ show l ++ ", " ++ show r ++ "]"

-- |Gen an empty Tree.
emptyRB :: RBTree a
emptyRB = Leaf

-- |Get the root node Color of current sub-tree, Leaf is also Black.
getColor :: RBTree a -> Color
getColor (Node c _ _ _) = c
getColor Leaf = Black

-- |Set current Root to Black.
setBlack :: RBTree a -> RBTree a
setBlack (Node _ v l r) = Node Black v l r
setBlack Leaf = Leaf

-- |Set current Root to Red.
setRed :: RBTree a -> RBTree a
setRed (Node _ v l r) = Node Red v l r
setRed Leaf = Leaf -- never happen

-- |Conversion : RBTree \<==> RBZip.
toZip :: RBTree a -> RBZip a
toZip t = RBZip t []

-- |convert a zip to tree.
toTree :: (a -> a) -> RBZip a -> RBTree a
toTree updateVersion z = tree
    where (RBZip tree _) = topMostZip updateVersion z

-- |Zip up.
topMostZip :: (a -> a) -> RBZip a -> RBZip a
topMostZip updateVersion (RBZip s ((Step c v d s1):path)) = case d of
        ToLeft -> topMostZip updateVersion (RBZip (Node c (updateVersion v) s s1) path)
        ToRight -> topMostZip updateVersion (RBZip (Node c (updateVersion v) s1 s) path)
topMostZip _ z = z

-- |Get the Left-most non-leaf node from a Zip, or get Leaf if it is a Leaf.
leftMostZip :: RBZip a -> RBZip a
leftMostZip this@(RBZip (Node _ _ Leaf _) _) = this
leftMostZip (RBZip (Node c v l r) path) = leftMostZip (RBZip l ((Step c v ToLeft r):path))
leftMostZip z = z --only when leaf itself from start over

-- |Get the Right-most non-leaf node from a Zip, or get Leaf if it is a Leaf.
rightMostZip :: RBZip a -> RBZip a
rightMostZip this@(RBZip (Node _ _ _ Leaf) _) = this
rightMostZip (RBZip (Node c v l r) path) = rightMostZip (RBZip r ((Step c v ToRight l):path))
rightMostZip z = z --leaf itself

-- |Zip up until the sub-tree has a left-parent, used to find the biggest lower-order element of the current node.
leftParentZip :: RBZip a -> RBZip a
leftParentZip (RBZip l ((Step c v ToLeft r):path)) = leftParentZip (RBZip (Node c v l r) path)
leftParentZip (RBZip r ((Step c v ToRight l):path)) = RBZip (Node c v l r) path
leftParentZip (RBZip _ []) = RBZip Leaf [] -- no such parent, return a empty zip

-- |Zip up until the sub-tree has a right-parent, used to find the smallest upper-order element of the current node.
rightParentZip :: RBZip a -> RBZip a
rightParentZip (RBZip r ((Step c v ToRight l):path)) = rightParentZip (RBZip (Node c v l r) path)
rightParentZip (RBZip l ((Step c v ToLeft r):path)) = RBZip (Node c v l r) path
rightParentZip (RBZip _ []) = RBZip Leaf [] -- no such parent, return a empty zip

-- |find predecessor of a node/leaf.
predZip :: RBZip a -> RBZip a
predZip (RBZip (Node c v l@(Node _ _ _ _) r) path) = rightMostZip (RBZip l ((Step c v ToLeft r):path))
predZip z@(RBZip Leaf _) = case lp of
  RBZip Leaf [] -> z -- itself
  _ -> lp
  where lp = leftParentZip z
predZip z@(RBZip (Node c v l r) path) = case lp of
  RBZip Leaf [] -> RBZip l ((Step c v ToLeft r):path)
  _ -> lp
  where lp = leftParentZip z

-- |find successor of a node/leaf.
succZip :: RBZip a -> RBZip a
succZip (RBZip (Node c v l r@(Node _ _ _ _)) path) = leftMostZip (RBZip r ((Step c v ToRight l):path))
succZip z@(RBZip Leaf _) = case lp of
  RBZip Leaf [] -> z -- itself
  _ -> lp
  where lp = rightParentZip z
succZip z@(RBZip (Node c v l r) path) = case lp of
  RBZip Leaf [] -> RBZip r ((Step c v ToRight l):path)
  _ -> lp
  where lp = rightParentZip z

-- |Get the Leftmost non-leaf node's value from a Zip.
-- [@param 1@] current node's value.
-- [@param 2@] current node's left child.
leftmostV :: a -> RBTree a -> a
leftmostV v Leaf = v
leftmostV _ (Node _ vl l _) = leftmostV vl l

-- Insertion functions. x will be in left of y if x equals to y and y has already in the tree.

-- |Insert \'Ord\' things.
insertOrd :: (Ord a) => RBTree a -> a -> RBTree a
insertOrd = insert compare

-- |Insert a bunch of \'Ord\' things.
insertOrdList :: (Ord a) => RBTree a -> [a] -> RBTree a
insertOrdList = foldl insertOrd

-- |Insert anything.
-- |you have to provide a compare function.
insert :: (a -> a -> Ordering) -> RBTree a -> a -> RBTree a
insert = insertVersioned id

-- |Insert a versioned element such that the values of any nodes
-- modified during insertion are also given updated versions.  This
-- information can later be used to efficiently calculate the
-- difference between two versioned trees which share structure.
insertVersioned :: (a -> a) -> (a -> a -> Ordering) -> RBTree a -> a -> RBTree a
insertVersioned updateVersion f t v =
  modifyVersioned updateVersion f t v $ const $ Just v

-- |Insert, delete, or modify an element of the tree.
--
-- Note that the specified transformation function must not change
-- the element's ordering relative to the other elements or else the
-- resulting tree will be corrupted.
modify :: (b -> a -> Ordering) -> RBTree a -> b -> (Maybe a -> Maybe a) -> RBTree a
modify = modifyVersioned id

-- |Insert, delete, or modify a versioned element such that the values
-- of any nodes modified during insertion are also given updated
-- versions.  This information can later be used to efficiently
-- calculate the difference between two versioned trees which share
-- structure.
--
-- Note that the specified transformation function must not change
-- the element's ordering relative to the other elements or else the
-- resulting tree will be corrupted.
modifyVersioned :: (a -> a) -> (b -> a -> Ordering) -> RBTree a -> b -> (Maybe a -> Maybe a) -> RBTree a
modifyVersioned updateVersion f t key transform =
  case searchZipTrace f (toZip t) key of
    RBZip Leaf path -> case transform Nothing of
      Nothing -> t
      Just new -> setBlack . toTree updateVersion . insertFixup updateVersion
                  $ RBZip (Node Red (updateVersion new) Leaf Leaf) path
    z@(RBZip (Node c v l r) path) -> case transform $ Just v of
      Nothing -> toTree updateVersion . deleteZip updateVersion $ z
      Just new -> toTree updateVersion $ RBZip (Node c new l r) path

-- |Insert Operator for insertOrd
(<</) :: (Ord a) => RBTree a -> a -> RBTree a
t <</ e = insertOrd t e

updateNode :: (a -> a) -> RBTree a -> RBTree a
updateNode updateVersion (Node color v d s) = Node color (updateVersion v) d s
updateNode _ t = t

-- insertFixup
--
-- a : current node
-- b : parent of a
-- c : parent of b
-- d : brother of b
-- vx : value of x
-- dx : direction of x
-- sx : sub-tree of x in the path
-- sxy : sub-tree of x in y side
insertFixup :: (a -> a) -> RBZip a -> RBZip a
insertFixup updateVersion (RBZip a@(Node Red _ _ _) ((Step Red vb db sb):(Step Black vc dc d@(Node Red _ _ _)):path)) =
    insertFixup updateVersion (RBZip newC path)
    where newC = Node Red (updateVersion vc) newCL newCR
          (newCL,newCR) = case dc of
              ToLeft -> (newB,newD)
              ToRight -> (newD,newB)
          newB = Node Black (updateVersion vb) newBL newBR
          (newBL,newBR) = case db of
              ToLeft -> (a,sb)
              ToRight -> (sb,a)
          !newD = updateNode updateVersion (setBlack d)
insertFixup updateVersion (RBZip a@(Node Red va sal sar) ((Step Red vb db sb):(Step Black vc dc d):path)) =
    RBZip newZ (newP:path)
    where (newZ, newP) = case (dc,db) of
              (ToLeft,ToLeft) -> (a,Step Black vb dc (Node Red (updateVersion vc) sb d))
              (ToLeft,ToRight) -> (Node Red (updateVersion vb) sb sal, Step Black va dc (Node Red (updateVersion vc) sar d))
              (ToRight,ToLeft) -> (Node Red (updateVersion vb) sar sb, Step Black va dc (Node Red (updateVersion vc) d sal))
              (ToRight,ToRight) -> (a,Step Black vb dc (Node Red (updateVersion vc) d sb))
insertFixup _ t = t

-- Search functions. return \'Just result\' on success, otherwise Nothing.

-- |Search for \'Ord\' things. see 'search'
searchOrd :: (Ord a) => RBTree a -> a -> Maybe a
searchOrd = search compare

-- |search for any thing, you should provide proper compare function.
search :: (b -> a -> Ordering) -> RBTree a -> b -> Maybe a
search f t v = case rZip of
    Just (RBZip (Node _ v' _ _) _) -> Just v'
    _ -> Nothing
    where rZip = searchZip f (toZip t) v

-- |Search operator for searchOrd
(<<?) :: (Ord a) => RBTree a -> a -> Maybe a
t <<? e = searchOrd t e

-- |a faster 'search' function implemetation. strongly recommanded.
searchFast :: (b -> a -> Ordering) -> RBTree a -> b -> Maybe a
searchFast f (Node _ v l r) vs = case f vs v of
    LT -> searchFast f l vs
    GT -> searchFast f r vs
    EQ -> Just v
searchFast _ Leaf _ = Nothing

-- |Search the Maximum value in the tree, equals to get the right-most element.
searchMax :: (Ord a) => RBTree a -> Maybe a
searchMax t = case r of
    RBZip (Node _ v _ _) _ -> Just v
    _ -> Nothing
    where r = rightMostZip . toZip $ t

-- |Search the Minimum value in the tree, equals to get the left-most element.
searchMin :: (Ord a) => RBTree a -> Maybe a
searchMin t = case r of
    RBZip (Node _ v _ _) _ -> Just v
    _ -> Nothing
    where r = leftMostZip . toZip $ t


searchZip :: (b -> a -> Ordering) -> RBZip a -> b -> Maybe (RBZip a)
searchZip _ (RBZip Leaf _) _ = Nothing
searchZip f this@(RBZip (Node c v l r) path) vs = case f vs v of
    LT -> searchZip f (RBZip l ((Step c v ToLeft r):path)) vs
    GT -> searchZip f (RBZip r ((Step c v ToRight l):path)) vs
    EQ -> Just this

-- searchZipTrace : always returns the current point that the search stops.
-- returns a Zip-Node on equal, otherwise a Zip-Leaf
searchZipTrace :: (b -> a -> Ordering) -> RBZip a -> b -> RBZip a
searchZipTrace _ z@(RBZip Leaf _) _ = z
searchZipTrace f this@(RBZip (Node c v l r) path) vs = case f vs v of
    LT -> searchZipTrace f (RBZip l ((Step c v ToLeft r):path)) vs
    GT -> searchZipTrace f (RBZip r ((Step c v ToRight l):path)) vs
    EQ -> this

-- |Search \'Ord\' things, see 'searchInterval'
searchIntervalOrd :: (Ord a) => RBTree a -> a -> Interval a
searchIntervalOrd t a = searchInterval compare t a

-- |Search for a Interval.
--
--  For example: tree has 1,3,5,7. search for 3 returns [3,3] that indicates itself
--      search for 4 returns [3,5] indicates that 4 is between the element 3 and 5
--
--  The given value be or not be an element of the tree.
searchInterval :: (b -> a -> Ordering) -> RBTree a -> b -> Interval a
searchInterval f t a = case r of
    RBZip Leaf _ -> Interval (toNRealOrd (predZip r), toPRealOrd (succZip r))
    _ -> Interval (toNRealOrd r, toPRealOrd r)
    where r = searchZipTrace f (toZip t) a
          toNRealOrd (RBZip Leaf _) = NInfinity
          toNRealOrd (RBZip (Node _ v _ _) _) = RealValue v
          toPRealOrd (RBZip Leaf _) = PInfinity
          toPRealOrd (RBZip (Node _ v _ _) _) = RealValue v

-- Delete functions.

-- |Delete an \'Ord\' thing. see 'delete'.
deleteOrd :: (Ord a) => RBTree a -> a -> RBTree a
deleteOrd = delete compare

-- |Delete a sequence of elements.
deleteOrdList :: (Ord a) => RBTree a -> [a] -> RBTree a
deleteOrdList = foldl deleteOrd

-- |If there is no relevant element in tree, tree will be returned unmodified.
delete :: (a -> a -> Ordering) -> RBTree a -> a -> RBTree a
delete = deleteVersioned id

-- |Delete a versioned element such that the values of any nodes
-- modified during deletion are given updated versions.  This
-- information can later be used to efficiently calculate the
-- difference between two versioned trees which share structure.
deleteVersioned :: (a -> a) -> (b -> a -> Ordering) -> RBTree a -> b -> RBTree a
deleteVersioned updateVersion f t key =
  modifyVersioned updateVersion f t key $ const Nothing

-- |Delete Operator for deleteOrd
(<<\) :: (Ord a) => RBTree a -> a -> RBTree a
t <<\ e = deleteOrd t e

deleteZip :: (a -> a) -> RBZip a -> RBZip a
deleteZip _ z@(RBZip Leaf _) = z

-- case 1: left null
deleteZip updateVersion (RBZip (Node c _ Leaf r) path) = case c of --r may be Leaf
    Red -> RBZip r path
    Black -> deleteFixup updateVersion (RBZip r path)

-- case 2: right null
deleteZip updateVersion (RBZip (Node c _ l Leaf) path) = case c of
    Red -> RBZip l path
    Black -> deleteFixup updateVersion (RBZip l path)

-- case 3: both not null
deleteZip updateVersion (RBZip (Node c _ l r@(Node _ vr srl _)) path) =
  deleteZip updateVersion newX
    where !newX = leftMostZip (RBZip r ((Step c newV ToRight l):path))
          !newV = leftmostV vr srl

-- |fixup.
deleteFixup :: (a -> a) -> RBZip a -> RBZip a

-- endcase : 'a' may be Leaf!
deleteFixup updateVersion (RBZip a@(Node Red _ _ _) path) = RBZip (updateNode updateVersion (setBlack a)) path

-- case 1: brother of x is Red
deleteFixup updateVersion (RBZip a ((Step _ vb db (Node Red vd l r)):path)) =
    deleteFixup updateVersion $ RBZip a ((Step Red vb db newW):(Step Black vd db newS):path)
    where (!newW, !newS) = case db of
              ToLeft -> (l,r)
              ToRight -> (r,l)

-- case 4: x's brother s is black, but s's outter child is Red
-- c may be leaf
deleteFixup updateVersion (RBZip a ((Step cb vb ToLeft (Node Black vd c e@(Node Red _ _ _))):path)) =
    deleteFixup updateVersion . topMostZip updateVersion $ RBZip (Node cb (updateVersion vd) (Node Black (updateVersion vb) a c) (updateNode updateVersion (setBlack e))) path
deleteFixup updateVersion (RBZip a ((Step cb vb ToRight (Node Black vd e@(Node Red _ _ _) c)):path)) =
    deleteFixup updateVersion . topMostZip updateVersion $ RBZip (Node cb (updateVersion vd) (updateNode updateVersion (setBlack e)) (Node Black (updateVersion vb) c a)) path

-- case 3: x's brother s is black, but s's inner child is Red
deleteFixup updateVersion (RBZip a ((Step cb vb ToLeft (Node Black vd (Node Red vc scl scr) e)):path)) =
    deleteFixup updateVersion $ RBZip a ((Step cb vb ToLeft (Node Black (updateVersion vc) scl (Node Red (updateVersion vd) scr e))):path)
deleteFixup updateVersion (RBZip a ((Step cb vb ToRight (Node Black vd e (Node Red vc scl scr))):path)) =
    deleteFixup updateVersion $ RBZip a ((Step cb vb ToRight (Node Black (updateVersion vc) (Node Red (updateVersion vd) e scl) scr)):path)

-- case 2: s's both children are not Red (Black or Leaf).
deleteFixup updateVersion (RBZip a ((Step cb vb db d@(Node Black _ _ _)):path)) =
    deleteFixup updateVersion $ (RBZip (Node cb (updateVersion vb) newL newR) path)
    where (!newL, !newR) = case db of
              ToLeft -> (a,d')
              ToRight -> (d',a)
          !d' = updateNode updateVersion (setRed d)

-- any other case: set current node to black and return.
deleteFixup updateVersion (RBZip a path) = RBZip (updateNode updateVersion (setBlack a)) path

-- Verification functions

-- |Verify black-depth are all the same.
--  Return Just \'depth\' on success, otherwise Nothing.
vD :: RBTree a -> Maybe Int
vD Leaf = Just 1
vD (Node c _ l r) =
    case dl == dr of
        True -> liftM2 (+) inc dl
        False -> Nothing
    where !dl = vD l
          !dr = vD r
          !inc = case c of
              Red -> Just 0
              Black -> Just 1

-- |vR : verify no \'red-red\' pattern in x and x\'s parent
vR :: RBTree a -> Bool
vR Leaf = True
vR (Node Black _ l r) = (vR l) && (vR r)
vR (Node Red _ l r) =
    (cl /= Red) && (cr /= Red) && (vR l) && (vR r)
    where !cl = getColor l
          !cr = getColor r

-- |Computes the difference between two versioned trees such that two
-- nodes with equal values and equal versions are assumed to be roots
-- of the same substructure, i.e. there are no differences at or below
-- that node.  This makes calculating the difference between two large
-- trees which share the majority of their structure (e.g. one is
-- derived from the other with a relatively small number of changes)
-- extremely fast.
--
-- Note that this will only work correctly if the trees have been
-- updated using only {insert|delete|modify}Versioned.
foldrDiffVersioned :: (a -> a -> Bool) -> (a -> a -> Ordering) -> (Maybe a -> Maybe a -> b -> b) -> b -> RBTree a -> RBTree a -> b
foldrDiffVersioned versionsEqual compareValues visit seed oldTree newTree =
  diffV oldTree newTree where
    diffV a Leaf = start a Leaf
    diffV Leaf b = start Leaf b
    diffV a@(Node _ va _ _) b@(Node _ vb _ _)
      | compareValues va vb == EQ && versionsEqual va vb = seed
      | otherwise = start a b

    -- todo: could save a bit of time by starting only as far left as
    -- the leftmost changed nodes:
    start a b = walk (leftMostZip $ toZip a) (leftMostZip $ toZip b)

    walk a@(RBZip (Node _ va _ _) _) b@(RBZip (Node _ vb _ _) _) =
      case compareValues va vb of
        LT -> visit (Just va) Nothing $ walk (succZip a) b
        GT -> visit Nothing (Just vb) $ walk a (succZip b)
        EQ -> if versionsEqual va vb then
                walk (rightParentZip a) (rightParentZip b)
              else
                visit (Just va) (Just vb) $ walk (succZip a) (succZip b)

    walk a@(RBZip Leaf _) b@(RBZip (Node _ vb _ _) _) =
      visit Nothing (Just vb) $ walk a (succZip b)

    walk a@(RBZip (Node _ va _ _) _) b@(RBZip Leaf _) =
      visit (Just va) Nothing $ walk (succZip a) b

    walk _ _ = seed

-- |Uses \'foldrDiffVersioned\' to compute the difference between two
-- versioned trees.
--
-- The return value is a tuple \'(obsolete, new)\' where \'obsolete\'
-- is a tree containing all elements present in the first tree but not
-- the second tree and the \'new\' is a tree containing all the
-- elements present in the second tree but not the first tree.
diffVersioned :: (a -> a -> Bool) -> (a -> a -> Ordering) -> RBTree a -> RBTree a -> (RBTree a, RBTree a)
diffVersioned versionsEqual compareValues oldTree newTree =
  foldrDiffVersioned versionsEqual compareValues
  (\maybeA maybeB result@(as, bs) ->
    case (maybeA, maybeB) of
      (Just a, Nothing) -> (insert compareValues as a, bs)
      (Nothing, Just b) -> (as, insert compareValues bs b)
      _ -> result)
  (Leaf, Leaf) oldTree newTree

diff :: (a -> a -> Ordering) -> RBTree a -> RBTree a -> (RBTree a, RBTree a)
diff compareValues a b =
  (rbSubtract compareValues b a,
   rbSubtract compareValues a b)

unionVersioned :: (a -> a) -> (a -> a -> Ordering) -> RBTree a -> RBTree a -> RBTree a
unionVersioned updateVersion compareValues small large =
  F.foldr (flip $ insertVersioned updateVersion compareValues) large small

subtractVersioned :: (a -> a) -> (a -> a -> Ordering) -> RBTree a -> RBTree a -> RBTree a
subtractVersioned updateVersion compareValues subtrahend minuend =
  F.foldr (flip $ deleteVersioned updateVersion compareValues) minuend subtrahend

intersectVersioned :: (a -> a) -> (a -> a -> Ordering) -> RBTree a -> RBTree a -> RBTree a
intersectVersioned updateVersion compareValues small large =
  F.foldr visit small small where
    visit v r = case searchFast compareValues large v of
      Nothing -> deleteVersioned updateVersion compareValues r v
      _ -> r

union :: (a -> a -> Ordering) -> RBTree a -> RBTree a -> RBTree a
union = unionVersioned id

rbSubtract :: (a -> a -> Ordering) -> RBTree a -> RBTree a -> RBTree a
rbSubtract = subtractVersioned id

intersect :: (a -> a -> Ordering) -> RBTree a -> RBTree a -> RBTree a
intersect = intersectVersioned id

instance Foldable RBTree where
  foldr visit seed tree = walk $ leftMostZip $ toZip tree where
      walk z@(RBZip (Node _ v _ _) _) = visit v $ walk $ succZip z
      walk _ = seed

instance Eq a => Eq (RBTree a) where
  a == b = walk (leftMostZip $ toZip a) (leftMostZip $ toZip b) where
    walk a'@(RBZip (Node _ va _ _) _) b'@(RBZip (Node _ vb _ _) _) =
      va == vb && walk (succZip a') (succZip b')
    walk (RBZip Leaf _) (RBZip Leaf _) = True
    walk _ _ = False

fromList :: (a -> a -> Ordering) -> [a] -> RBTree a
fromList compareVersions = F.foldr (flip $ insert compareVersions) Leaf

value :: RBZip a -> Maybe a
value t = case t of
  RBZip (Node _ v _ _) _ -> Just v
  _ -> Nothing

first :: RBTree a -> Maybe a
first = value . leftMostZip . toZip

last :: RBTree a -> Maybe a
last = value . rightMostZip . toZip
