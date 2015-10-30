module Concelo.Receiver
  ( receive
  , apply
  , Receiver() ) where

import Prelude (($), otherwise, Show, show, Eq, eq, Ord, (++))
import Data.Either (Either(Left, Right))
import Data.Map (Map())
import qualified Data.Map as M
import Data.Set (Set())
import qualified Data.Set as S
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldr)
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T
import Concelo.Subscriber (Update(Add, NewRoot))

data Receiver k v = Receiver
  { received :: Map k (Tree k v)
  , root :: (Tree k v) }

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
  { received: T.fold (\tree result -> M.insert (T.key tree) tree result)
      M.empty
      $ S.singleton root
  , root: root }

apply :: forall v. (Show v) =>
         Update String v ->
         Receiver String v ->
         Either (Set String) (Receiver String v)
         
apply (Add content children) (Receiver receiver) =
  case foldr f (Right S.empty) children of
    Right trees ->
      let tree = T.tree content trees in
        Right $ Receiver receiver
          { received = M.insert (T.key tree) tree receiver.received }

    Left nacks ->
      Left nacks

  where f id result =
          case result of
            Right trees ->
              case M.lookup id receiver.received of
                Just tree -> Right $ S.insert tree trees
                Nothing -> Left $ S.singleton id

            Left nacks ->
              if M.member id receiver.received
              then result
              else Left $ S.insert id nacks
              
apply (NewRoot root) (Receiver receiver) =
  case M.lookup root receiver.received of
    Just tree -> Right $ receive tree
    Nothing -> Left $ S.singleton root
