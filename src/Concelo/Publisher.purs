module Concelo.Publisher
  ( publisher
  , update
  , nack
  , Next(Next, End)
  , Update(Add, NewRoot)
  , Publisher(Publisher) ) where

import Prelude (($), (==), otherwise, Ord, compare, Show, show, (++))
import Data.List (List(Cons, Nil))
import Data.Set (Set())
import qualified Data.Set as S
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldr)
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T

data Publisher k v = Publisher
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
  = Next (Update k v) (Publisher k v)
  | End

newRoot (Publisher publisher) root = Publisher publisher
  { acks = S.singleton root
  , next = Next (NewRoot (T.key root)) (publisher root) }

add :: forall k v. (Ord k) =>
       Publisher k v ->
       Tree k v ->
       Publisher k v ->
       Publisher k v

add (Publisher publisher) tree (Publisher next) = Publisher publisher
  { next = Next (Add (T.value tree) (keys (T.children tree)))
             $ Publisher next { acks = S.insert tree next.acks } }
  
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

publisher root =
  Publisher { acks: S.singleton root
             , root: root
             , next: End }

update :: forall k v. (Ord k) =>
          Publisher k v ->
          Tree k v ->
          Publisher k v

update publisher@(Publisher p) root
  | p.root == root = Publisher p { next = End }

  | S.member root p.acks = newRoot publisher root

  | otherwise =
      recurse (S.singleton root) $ newRoot publisher root
      
      where acks = T.fold S.insert S.empty p.acks
            recurse trees result =
              foldr (\tree result ->
                      if S.member tree acks
                      then result
                      else recurse (T.children tree) $
                             add publisher tree result)
                result
                trees

nack :: forall k v. (Ord k) =>
        Publisher k v ->
        Tree k v ->
        Tree k v ->
        Publisher k v

nack publisher@(Publisher p) tree root =
  update (Publisher p { acks = remove tree p.acks }) root
