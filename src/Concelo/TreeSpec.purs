module Concelo.TreeSpec
  ( make
  , key
  , toSpec
  , toTree
  , TreeSpec() ) where

import Concelo.Tree (Tree())
import qualified Concelo.Tree as T
import Prelude (Ord, compare, Show, show, ($), (++), (<<<), map)
import Data.Set (Set())
import qualified Data.Set as S
import Data.Map (Map())
import qualified Data.Map as M
import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Either (Either(Left, Right))
import Data.Foldable (foldr)
import Data.String (take)

data TreeSpec k v = TreeSpec
  { key :: k,
    value :: v,
    children :: Set k }

instance showTreeSpec :: (Show k, Show v) => Show (TreeSpec k v) where
  show (TreeSpec s) =
    "( " ++ (take 7 <<< show) s.key ++
    " " ++ show s.value ++
    " " ++ show (map (take 7 <<< show) $ S.toList s.children) ++ ")"

make :: forall k v. (Show v) =>
        k ->
        v ->
        Set k ->
        TreeSpec k v

make key content children = TreeSpec
  { key: key
  , value: content
  , children: children }

toSpec :: forall k v. (Ord k) =>
          Tree k v ->
          TreeSpec k v

toSpec tree = TreeSpec
  { key: T.key tree
  , value: T.value tree
  , children: foldr (\tree result -> S.insert (T.key tree) result)
                S.empty (T.children tree) }

toTree :: forall k v. (Ord k) =>
          k ->
          Map k (TreeSpec k v) ->
          Either (Set k) (Tree k v)

toTree root specs =
  resolve root

  where resolve key = case M.lookup key specs of
          Just (TreeSpec s) ->
            case iterate (S.toList s.children) (Right S.empty) of
              Right trees -> Right $ T.tree s.key s.value trees
              Left nacks -> Left nacks
          Nothing -> Left $ S.singleton key
          
        iterate (Cons key keys) result =
          iterate keys case resolve key of
            Right tree -> case result of
              Right trees -> Right $ S.insert tree trees
              left -> left
            Left newNacks -> case result of
              Right trees -> Left newNacks
              Left oldNacks -> Left $ S.union newNacks oldNacks
          
        iterate Nil result = result

key (TreeSpec s) = s.key
