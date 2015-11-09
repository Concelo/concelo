module Test.Main where

import Concelo.Publisher (Next(Next, End), update, publisher,
                           Publisher(), Update(Add, NewRoot))
import Concelo.Subscriber (apply, subscriber, Subscriber())
import Concelo.Tree (tree, leaf, key, value, empty, Tree())
import Prelude (($), (++), unit, (==), (/=), bind, show, return, Unit(), Show,
                Eq, flip)
import Data.Foldable (foldr, foldl)
import Data.Set (Set())
import Data.List (List(Cons, Nil))
import Data.Monoid (Monoid)
import qualified Data.Set as S
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import Test.Unit (assert, Assertion(), runTest, test, failure, success)

assertEqual :: forall v e. (Show v, Eq v) =>
               v ->
               v ->
               Assertion e
               
assertEqual a b = assert (show a ++ " is not equal to " ++ show b) (a == b)

checkReceived :: forall v e. (Show v, Eq v, Monoid v) =>
                 Tree String v ->
                 Subscriber String v ->
                 Assertion e

checkReceived tree subscriber =
  if S.isEmpty $ Rec.nacks subscriber then
    assertEqual subscriber $ subscriber tree else
    failure $ "got nacks " ++ show r.nacks ++ " when syncing " ++ show tree

sync :: forall v e. (Show v, Eq v, Monoid v) =>
        Tree String v ->
        Assertion e

sync tree = 
  checkReceived tree result
  
  where iterate publisher result =
          case Sub.next publisher of
            Next update publisher -> iterate publisher $ apply update result
            End -> result

        result = iterate
          (update (publisher empty) tree)
          (subscriber empty)

expectReceived :: forall v e. (Show v, Eq v, Monoid v) =>
               Tree String v ->
               List (Update String v) ->
               Assertion e

expectReceived tree updates =
  checkReceived tree $ foldl (flip apply) (subscriber empty) updates

checkNacks :: forall v e. (Show v, Eq v, Monoid v) =>
              Set String ->
              Subscriber String v ->
              Assertion e

checkNacks expected subscriber =
  if S.isEmpty $ Rec.nacks subscriber then
    failure $ "expected nacks " ++ show expected ++ ", but got "
    ++ show subscriber
  else
    assertEqual r.nacks expected

expectNacks :: forall v e. (Show v, Eq v, Monoid v) =>
               Set String ->
               List (Update String v) ->
               Assertion e

expectNacks expected updates =
  checkNacks expected $ foldl (flip apply) (subscriber empty) updates

leaf1 = leaf "leaf 1"
leaf2 = leaf "leaf 2"
leaf3 = leaf "leaf 3"
intermediate = tree "intermediate" $ S.insert leaf1 $ S.singleton leaf2
root = tree "root" $ S.insert leaf3 $ S.singleton intermediate

main = runTest do
  test "sync leaf" do
    sync leaf1
    
  test "sync tree" do
    sync root
    
  test "build leaf from updates" do
    expectReceived leaf1
      $ Add (value leaf1) S.empty
      : Add (value leaf2) S.empty
      : NewRoot (key leaf1)
      : Nil

  test "build tree from updates" do
    expectReceived root
      $ Add (value leaf3) S.empty
      : Add (value leaf2) S.empty
      : Add (value leaf1) S.empty
      : (Add (value intermediate)
         $ S.insert (key leaf2)
         $ S.singleton (key leaf1))
      : (Add (value root)
         $ S.insert (key intermediate)
         $ S.singleton (key leaf3))
      : NewRoot (key root)
      : Nil

  test "build tree from out-of-order updates plus unused update" do
    expectReceived root
      $ Add (value leaf2) S.empty
      : NewRoot (key root)
      : (Add (value intermediate)
         $ S.insert (key leaf2)
         $ S.singleton (key leaf1))
      : (Add "unused"
         $ S.insert "nonexistent"
         $ S.singleton (key leaf1))
      : Add (value leaf1) S.empty
      : (Add (value root)
         $ S.insert (key intermediate)
         $ S.singleton (key leaf3))
      : Add (value leaf3) S.empty
      : Nil

  test "nacks on missing updates" do
    expectNacks (S.insert (key leaf2)
                 $ S.singleton (key leaf3))
      $ Cons (Add (value leaf1) S.empty)
      $ Cons (Add (value intermediate)
              $ S.insert (key leaf2)
              $ S.singleton (key leaf1))
      $ Cons (Add (value root)
              $ S.insert (key intermediate)
              $ S.singleton (key leaf3))
      $ Cons (NewRoot (key root))
      Nil

  Simulator.main
