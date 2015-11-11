module Concelo.Subscriber
  ( make
  , apply
  , root
  , next
  , nacks
  , Next(Next, End)
  , Subscriber() ) where

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
import Concelo.TreeSpec (TreeSpec())
import qualified Concelo.TreeSpec as TS
import Concelo.Publisher (Update(Add, NewRoot))

data Subscriber k v = Subscriber
  { received :: Map k (TreeSpec k v)
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
      (\tree result -> M.insert (T.key tree) (TS.toSpec tree) result)
      M.empty
      root
  , root: root
  , nacks: S.empty
  , newRoot: Nothing }

apply :: forall v. (Show v) =>
         Update String v ->
         Subscriber String v ->
 Subscriber String v
         
apply (Add content children) (Subscriber s) =
  case s.newRoot of
    Just newRoot -> apply (NewRoot newRoot) result
    Nothing -> result

  where spec' = TS.make content children
        
        addIfNack key nacks =
          if M.member key s.received then
            nacks else
            S.insert key nacks

        result = Subscriber s
          { received = M.insert (TS.key spec') spec' s.received
          , nacks =
            S.delete (TS.key spec') $ foldr addIfNack s.nacks children }
            
apply (NewRoot root) (Subscriber s) =
  if M.member root s.received then
    case TS.toTree root s.received of
      Just tree -> make tree
      Nothing -> Subscriber s { newRoot = Just root } else
    Subscriber s { nacks = S.insert root s.nacks
                 , newRoot = Just root }
