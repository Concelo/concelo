module Concelo.Publisher
  ( make
  , publish
  , nack
  , root
  , next
  , consistent
  , add
  , newRoot
  , Next(Next, End)
  , Update(Add, NewRoot)
  , Publisher() ) where

import Prelude (($), (==), otherwise, Ord, Show, show, (++))
import Data.List (List(Cons, Nil), (:))
import qualified Data.List as L
import Data.Set (Set())
import qualified Data.Set as S
import Data.Map (Map())
import qualified Data.Map as M
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (foldr)
import Data.Monoid (Monoid)
import Data.String (drop, null)
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T

data Publisher k v = Publisher
  { acks :: Map k (Tree k v)
  , nacks :: Map k (Tree k v)
  , root :: Tree k v
  , nackList :: List (Tree k v)
  , newRoot :: Boolean }

showKeys :: forall v.
            Map String v ->
            String

showKeys map =
  "(" ++ (foldr (\k s -> drop 56 k ++ if null s then s else ", " ++ s)
          "" $ M.keys map) ++ ")"

instance showPublisher :: Show (Publisher String v) where
  show (Publisher p) =
    let root = T.fold S.insert S.empty p.root
        maps = foldr S.insert S.empty $ M.union p.acks p.nacks
        rootButNotMaps = S.difference root maps
        mapsButNotRoot = S.difference maps root
        inconsistent =
          (if S.isEmpty rootButNotMaps then "" else
             " in root but not maps: " ++ T.showSet rootButNotMaps) ++
          (if S.isEmpty mapsButNotRoot then "" else
             " in maps but not root: " ++ T.showSet mapsButNotRoot) in

    "publisher acks " ++ showKeys p.acks ++ " nacks "
    ++ showKeys p.nacks ++ " root "
    ++ show p.root
    ++ inconsistent

data Update k v
  = Add k v (Set k)
  | NewRoot k

instance showUpdate :: (Show v) => Show (Update String v) where
  show (Add k v children) =
    "add " ++ drop 56 k ++ " ("
    ++ foldr (\k s -> (drop 56 k) ++ if null s then s else ", " ++ s)
    "" children
    ++ ")"
    
  show (NewRoot k) =
    "new root " ++ drop 56 k

data Next k v
  = Next (Update k v) (Publisher k v)
  | End

root (Publisher p) = p.root

next :: forall k v. (Ord k) =>
        Publisher k v ->
        Next k v

next (Publisher p)
  | p.newRoot = Next (newRoot p.root) $ Publisher p { newRoot = false }

  | otherwise =
    case p.nackList of
      Cons nack nacks ->
        Next (add nack)
        $ Publisher p { acks = M.insert (T.key nack) nack p.acks
                      , nacks = M.delete (T.key nack) p.nacks
                      , nackList = nacks
                      , newRoot = L.null nacks }
  
      Nil -> 
        if M.isEmpty p.nacks then End else
          next $ Publisher p { nackList = M.values p.nacks }

newRoot root = NewRoot $ T.key root

add :: forall k v. (Ord k) =>
       Tree k v ->
       Update k v

add tree = Add (T.key tree) (T.value tree) $ keys (T.children tree)

  where keys trees =
          foldr (\tree result -> S.insert (T.key tree) result) S.empty trees

make :: forall k v. (Ord k) =>
        Tree k v ->
        Publisher k v

make root =
  Publisher { acks: T.fold
              (\tree result -> M.insert (T.key tree) tree result)
              M.empty
              root
              
            , nacks: M.empty
            , root: root
            , nackList: Nil
            , newRoot: true }

findOld :: forall v r.
           (Tree String v -> r -> r) ->
           (Tree String v -> Boolean) ->
           Tree String v ->
           r ->
           r

findOld onOld isInNext previous result =
  visit previous result

  where visit previous result
          | isInNext previous = result
          | otherwise =
            foldr visit (onOld previous result) $ T.children previous

findNewAndOld :: forall v r.
                 (Tree String v -> r -> r) ->
                 (Tree String v -> r -> r) ->
                 (Tree String v -> Boolean) ->
                 r ->
                 Tree String v ->
                 Tree String v ->
                 r

findNewAndOld onNew onOld isInPrevious result previous next =
  findOld onOld (\tree -> S.member tree state.found) previous state.result

  where state = visit next { found: S.empty, result: result }
        visit next state
          | isInPrevious next =
            state { found = S.insert next state.found }
          | otherwise =
            foldr visit state { result = onNew next state.result }
            $ T.children next

publish :: forall v.
           Publisher String v ->
           Tree String v ->
           Publisher String v

publish publisher@(Publisher p) root
  | p.root == root = publisher

  | otherwise = Publisher { root: root
                          , acks: maps.acks
                          , nacks: maps.nacks
                          , nackList: Nil
                          , newRoot: false }
    
    where maps = findNewAndOld
                 (\new maps ->
                   maps { nacks = M.insert (T.key new) new maps.nacks })
                 
                 (\old maps ->
                   { acks: M.delete (T.key old) maps.acks
                   , nacks: M.delete (T.key old) maps.nacks })
                 
                 (\tree ->
                   if M.member (T.key tree) p.acks then true else
                     M.member (T.key tree) p.nacks)
                 
                 { acks: p.acks
                 , nacks: p.nacks }
                 
                 p.root
                 root

consistent :: forall k v. (Ord k) =>
              Publisher k v ->
              Boolean

consistent (Publisher p) =
  (T.fold S.insert S.empty p.root)
  ==
  (foldr S.insert S.empty $ M.union p.acks p.nacks)

nack :: forall v. (Monoid v, Ord v, Show v) =>
        Publisher String v ->
        String ->
        Publisher String v

nack publisher@(Publisher p) nack =
  case M.lookup nack p.acks of
    Just tree -> Publisher p { acks = M.delete nack p.acks
                             , nacks = M.insert nack tree p.nacks
                             , nackList = tree : p.nackList }

    Nothing -> publisher
