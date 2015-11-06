module Concelo.Receiver
  ( receive
  , apply
  , Receiver(Receiver)
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
import Concelo.Subscriber (Update(Add, NewRoot))

data Spec k v = Spec
  { key :: k,
    value :: v,
    children :: Set k }

instance showSpec :: (Show k, Show v) => Show (Spec k v) where
  show (Spec s) =
    "spec key " ++ show s.key
    ++ " value " ++ show s.value
    ++ " children " ++ show s.children

data Receiver k v = Receiver
  { received :: Map k (Spec k v)
  , root :: (Tree k v)
  , nacks :: Set k
  , newRoot :: Maybe k }

instance showReceiver :: (Show k, Show v) => Show (Receiver k v) where
  show (Receiver receiver) =
    "receiver root " ++ show receiver.root
    ++ " received " ++ show receiver.received

instance eqReceiver :: (Eq k) => Eq (Receiver k v) where
  eq (Receiver a) (Receiver b) = eq a.root b.root

receive :: forall k v. (Ord k) =>
           Tree k v ->
           Receiver k v

receive root = Receiver
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
         Receiver String v ->
         Receiver String v
         
apply (Add content children) (Receiver r) =
  case r.newRoot of
    Just newRoot -> apply (NewRoot newRoot) result
    Nothing -> result

  where s = spec content children
        
        addIfNack key nacks =
          if M.member key r.received then
            nacks else
            S.insert key nacks

        result = Receiver r
          { received = M.insert (key s) s r.received
          , nacks = S.delete (key s) $ foldr addIfNack r.nacks children }
            
apply (NewRoot root) (Receiver r) =
  case M.lookup root r.received of
    Just spec ->
      case specToTree spec of
        Just tree -> receive tree
        Nothing -> Receiver r { newRoot = Just root }

    Nothing ->
      Receiver r { nacks = S.insert root r.nacks
                 , newRoot = Just root }

  where specToTree (Spec s) =
          case iterate S.empty (S.toList s.children) of
            Just trees -> Just (T.tree s.value trees)
            Nothing -> Nothing

        iterate trees (Cons key keys) =
          case M.lookup key r.received of
            Just spec ->
              case specToTree spec of
                Just tree -> iterate (S.insert tree trees) keys
                Nothing -> Nothing

            Nothing -> Nothing

        iterate trees Nil = Just trees
