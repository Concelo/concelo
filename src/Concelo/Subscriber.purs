module Concelo.Subscriber
  ( make
  , apply
  , root
  , next
  , nacks
  , Next(Next, End)
  , Subscriber()
  , Spec() ) where

import Prelude (($), otherwise, Show, show, Eq, eq, Ord, (++))
import Data.Either (Either(Left, Right))
import Data.Map (Map())
import qualified Data.Map as M
import Data.Set (Set())
import qualified Data.Set as S
import Data.List (List(Cons, Nil))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldr)
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T
import Concelo.Publisher (Update(Add, NewRoot))

data Spec k v = Spec
  { key :: k,
    value :: v,
    children :: Set k }

instance showSpec :: (Show k, Show v) => Show (Spec k v) where
  show (Spec s) =
    "spec key " ++ show s.key
    ++ " value " ++ show s.value
    ++ " children " ++ show s.children

data Subscriber k v = Subscriber
  { received :: Map k (Spec k v)
  , root :: (Tree k v)
  , nacks :: Set k
  , newRoot :: Maybe k }

data Next k v
  = Next k (Subscriber k v)
  | End

instance showSubscriber :: (Show k, Show v) => Show (Subscriber k v) where
  show (Subscriber subscriber) =
    "subscriber root " ++ show subscriber.root
    ++ " received " ++ show subscriber.received

instance eqSubscriber :: (Eq k) => Eq (Subscriber k v) where
  eq (Subscriber a) (Subscriber b) = eq a.root b.root

root (Subscriber s) = s.root

nacks (Subscriber s) = s.nacks

next :: forall k v. (Ord k) =>
        Subscriber k v ->
        Next k v

next (Subscriber s) =
  case S.toList s.nacks of
    Cons nack _ -> Next nack $ Subscriber s { nacks = S.delete nack s.nacks }
    Nil -> End

make :: forall k v. (Ord k) =>
        Tree k v ->
        Subscriber k v

make root = Subscriber
  { received: T.fold
      (\tree result -> M.insert (T.key tree) (treeToSpec tree) result)
      M.empty
      $ S.singleton root
  , root: root
  , nacks: S.empty
  , newRoot: Nothing }

spec :: forall v. (Show v) =>
        v ->
        Set String ->
        Spec String v

spec content children = Spec
  { key: T.hash content children
  , value: content
  , children: children }

treeToSpec :: forall k v. (Ord k) =>
              Tree k v ->
              Spec k v

treeToSpec tree = Spec
  { key: T.key tree
  , value: T.value tree
  , children: foldr (\tree result -> S.insert (T.key tree) result)
                S.empty (T.children tree) }

key (Spec s) = s.key

apply :: forall v. (Show v) =>
         Update String v ->
         Subscriber String v ->
         Subscriber String v
         
apply (Add content children) (Subscriber s) =
  case s.newRoot of
    Just newRoot -> apply (NewRoot newRoot) result
    Nothing -> result

  where spec' = spec content children
        
        addIfNack key nacks =
          if M.member key s.received then
            nacks else
            S.insert key nacks

        result = Subscriber s
          { received = M.insert (key spec') spec' s.received
          , nacks = S.delete (key spec') $ foldr addIfNack s.nacks children }
            
apply (NewRoot root) (Subscriber s) =
  case M.lookup root s.received of
    Just spec ->
      case specToTree spec of
        Just tree -> make tree
        Nothing -> Subscriber s { newRoot = Just root }

    Nothing ->
      Subscriber s { nacks = S.insert root s.nacks
                   , newRoot = Just root }

  where specToTree (Spec s) =
          case iterate S.empty (S.toList s.children) of
            Just trees -> Just (T.tree s.value trees)
            Nothing -> Nothing

        iterate trees (Cons key keys) =
          case M.lookup key s.received of
            Just spec ->
              case specToTree spec of
                Just tree -> iterate (S.insert tree trees) keys
                Nothing -> Nothing

            Nothing -> Nothing

        iterate trees Nil = Just trees
