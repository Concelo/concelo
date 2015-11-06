module Concelo.Subscriber
  ( subscribe
  , update
  , nack
  , Next(Next, End)
  , Update(Add, NewRoot)
  , Subscriber(Subscriber) ) where

import Prelude (($), (==), otherwise, Ord, compare, Show, show, (++))
import Data.List (List(Cons, Nil))
import Data.Set (Set())
import qualified Data.Set as S
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldr)
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T

data Subscriber k v = Subscriber
  { acks :: Set (Tree k v)
  , root :: Tree k v
  , next :: Next k v }

data Update k v
  = Add v (Set k)
  | NewRoot k

instance showUpdate :: (Show k, Show v) => Show (Update k v) where
  show (Add v children) = "add " ++ show v ++ " " ++ show children
  show (NewRoot k) = "new root " ++ show k

data Next k v
  = Next (Update k v) (Subscriber k v)
  | End

newRoot (Subscriber subscriber) root = Subscriber subscriber
  { acks = S.singleton root
  , next = Next (NewRoot (T.key root)) (subscribe root) }

add :: forall k v. (Ord k) =>
       Subscriber k v ->
       Tree k v ->
       Subscriber k v ->
       Subscriber k v

add (Subscriber subscriber) tree (Subscriber next) = Subscriber subscriber
  { next = Next (Add (T.value tree) (keys (T.children tree)))
             $ Subscriber next { acks = S.insert tree next.acks } }
  
  where keys trees =
          foldr (\tree result -> S.insert (T.key tree) result) S.empty trees

remove :: forall k v. (Ord k) =>
          Tree k v ->
          Set (Tree k v) ->
          Set (Tree k v)

remove tree acks =
  (recurse { acks: acks, found: Nothing }).acks
  
  where recurse result =
          if S.member tree result.acks
             
          then { acks: S.delete tree result.acks
               , found: Just tree }
               
          else iterate (S.toList result.acks)
               
          where iterate (Cons ack acks) =
                  case below.found of
                    Just found ->
                      { acks: S.delete ack
                          $ S.union result.acks
                          $ S.delete found (T.children ack)
                      , found: Just ack }

                    Nothing -> iterate acks
                      
                  where below = recurse result

                iterate Nil = { acks: result.acks
                              , found: Nothing }

subscribe root =
  Subscriber { acks: S.singleton root
             , root: root
             , next: End }

update :: forall k v. (Ord k) =>
          Subscriber k v ->
          Tree k v ->
          Subscriber k v

update subscriber@(Subscriber s) root
  | s.root == root = Subscriber s { next = End }

  | S.member root s.acks = newRoot subscriber root

  | otherwise =
      recurse (S.singleton root) $ newRoot subscriber root
      
      where acks = T.fold S.insert S.empty s.acks
            recurse trees result =
              foldr (\tree result ->
                      if S.member tree acks
                      then result
                      else recurse (T.children tree) $
                             add subscriber tree result)
                result
                trees

nack :: forall k v. (Ord k) =>
        Subscriber k v ->
        Tree k v ->
        Tree k v ->
        Subscriber k v

nack subscriber@(Subscriber s) tree root =
  update (Subscriber s { acks = remove tree s.acks }) root
