module Concelo.Tree
  ( Tree()
  , key
  , value
  , children
  , tree
  , leaf
  , fold
  , empty ) where

import Prelude (Eq, eq, Ord, compare, Show, show, ($), (++))
import Data.Set (Set())
import qualified Data.Set as S
import Data.List (List(Cons, Nil))
import Data.Foldable (foldr)
import Data.Monoid (Monoid, mempty)

data Tree k v = Tree k v (Set (Tree k v))

instance eqTree :: (Eq k) => Eq (Tree k v) where
  eq a b = eq (key a) (key b)

instance ordTree :: (Ord k) => Ord (Tree k v) where
  compare a b = compare (key a) (key b)  

instance showTree :: (Show k, Show v) => Show (Tree k v) where
  show (Tree key value children) =
    "(" ++ show key ++ " " ++ show value ++ " " ++ show children ++ ")"

key (Tree k _ _) = k

value (Tree _ v _) = v

children (Tree _ _ c) = c

foreign import hashStrings :: List String -> String

hash :: forall v. (Show v) =>
        v ->
        Set (Tree String v) ->
        String
        
hash content children =
  hashStrings $ Cons (show content)
    $ (foldr (\tree result -> Cons (key tree) result) Nil children)

tree :: forall v. (Show v) =>
        v ->
        Set (Tree String v) ->
        Tree String v
        
tree content children = Tree (hash content children) content children

leaf :: forall v. (Show v) =>
        v ->
        Tree String v
        
leaf content = tree content S.empty

empty :: forall v. (Monoid v, Show v) =>
         Tree String v
         
empty = leaf mempty

fold f result trees =
  recurse trees result
  where recurse trees result =
          foldr (\tree result -> f tree $ recurse (children tree) result)
            result
            trees
