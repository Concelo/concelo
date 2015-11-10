module Concelo.Publisher
  ( make
  , publish
  , nack
  , root
  , next
  , Next(Next, End)
  , Update(Add, NewRoot)
  , Publisher() ) where

import Prelude (($), (==), otherwise, Ord, compare, Show, show, (++))
import Data.List (List(Cons, Nil))
import Data.Set (Set())
import qualified Data.Set as S
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldr)
import Data.Monoid (Monoid)
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

root (Publisher p) = p.root

next (Publisher p) = p.next

newRoot (Publisher p) root = Publisher p
  { acks = S.singleton root
  , next = Next (NewRoot (T.key root)) $ make root }

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

remove :: forall k v. (Ord k, Ord v) =>
          Tree k v ->
          Set (Tree k v) ->
          Set (Tree k v)

remove key acks =
  (recurse acks).acks
  
  where recurse acks =
          if S.member key acks then
            
            { acks: S.delete key acks
            , found: Just key } else
               
            iterate $ S.toList acks
               
          where iterate (Cons head tail) =
                  case below.found of
                    Just found ->
                      { acks: S.delete head
                        $ S.union acks
                        $ S.delete found
                        $ T.children head
                        
                      , found: Just head }

                    Nothing -> iterate tail
                      
                  where below = recurse $ T.children head

                iterate Nil = { acks: acks
                              , found: Nothing }

make root =
  Publisher { acks: S.singleton root
            , root: root
            , next: End }

publish :: forall k v. (Ord k) =>
           Publisher k v ->
           Tree k v ->
           Publisher k v

publish publisher@(Publisher p) root
  | p.root == root = Publisher p { next = End }

  | S.member root p.acks = newRoot publisher root

  | otherwise =
      recurse (S.singleton root) $ newRoot publisher root
      
      where acks = T.fold S.insert S.empty p.acks
            recurse trees result =
              foldr (\tree result ->
                      if S.member tree acks then
                        result else
                        recurse (T.children tree) $ add publisher tree result)
                result
                trees

nack :: forall v. (Monoid v, Ord v, Show v) =>
        Publisher String v ->
        String ->
        Publisher String v

nack publisher@(Publisher p) nack =
  publish (Publisher p { acks = remove (T.wrap nack) p.acks }) p.root
