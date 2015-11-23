module Concelo.Tree
  ( Tree()
  , key
  , value
  , tree
  , children
  , showSet
  , make
  , leaf
  , fold
  , empty ) where

import Prelude (Eq, eq, Ord, compare, Show, show, ($), (++), (<<<), (<=), (+),
                (-), otherwise)
import Data.Set (Set())
import qualified Data.Set as S
import Data.List (List(Cons, Nil))
import Data.Foldable (foldr)
import Data.Monoid (Monoid, mempty)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (take, drop, length, null)

data Tree k v = Tree k v (Set (Tree k v))

instance eqTree :: (Eq k) => Eq (Tree k v) where
  eq a b = eq (key a) (key b)

instance ordTree :: (Ord k) => Ord (Tree k v) where
  compare a b = compare (key a) (key b)  

showSet :: forall v.
                Set (Tree String v) ->
                String

showSet set =
  "(" ++ (foldr (\t s -> show t ++ if null s then s else ", " ++ s)
          "" set) ++ ")"

instance showTree :: Show (Tree String v) where
  show (Tree key value children) =
    "[" ++ drop 56 key ++
    " " ++ showSet children ++ "]"

key (Tree k _ _) = k

value (Tree _ v _) = v

children (Tree _ _ c) = c

foreign import hashStrings :: List String -> String

foreign import hexToInt :: String -> Int

foreign import intToHex :: Int -> String

height key = hexToInt $ take 8 key

height' Nil = 0
height' (Cons key _) = height key

pad n string
  | n <= length string = string
  | otherwise = pad n ("0" ++ string)

setHeight height hash = pad 8 (intToHex height) ++ drop 8 hash

makeKey :: forall v. (Show v) =>
            v ->
            Set (Tree String v) ->
            String
        
makeKey content children =
   setHeight (1 + height' childKeys)
   $ hashStrings
   $ Cons (show content) childKeys
   
   where childKeys =
           foldr (\tree result -> Cons (key tree) result) Nil children

tree :: forall k v.
        k ->
        v ->
        Set (Tree k v) ->
        Tree k v

tree key value children = Tree key value children

make :: forall v. (Show v) =>
        v ->
        Set (Tree String v) ->
        Tree String v
        
make content children = tree (makeKey content children) content children

leaf :: forall v. (Show v) =>
        v ->
        Tree String v
        
leaf content = make content S.empty

wrap :: forall k v. (Monoid v, Show v) =>
        k ->
        Tree k v

wrap key = tree key mempty S.empty

empty :: forall v. (Monoid v, Show v) =>
         Tree String v
         
empty = leaf mempty

fold f result tree = visit tree result
  where visit tree result = f tree $ foldr visit result (children tree)

