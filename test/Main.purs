module Test.Main where

import Concelo.Publisher (Next(Next, End), Publisher(), Update(Add, NewRoot))
import qualified Concelo.Publisher as Pub
import Concelo.Subscriber (Subscriber())
import qualified Concelo.Subscriber as Sub
import Concelo.Tree (tree, leaf, key, value, empty, Tree())
import Prelude (($), (++), unit, (==), (/=), bind, show, return, Unit(), Show,
                Eq, flip)
import Data.Foldable (foldr, foldl)
import Data.Set (Set())
import Data.List (List(Cons, Nil), (:))
import Data.Monoid (Monoid)
import qualified Data.Set as S
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Ref (REF())
import Test.Unit (assert, Assertion(), runTest, test)
import Test.QuickCheck.LCG (randomSeed, mkSeed)

assertEqual :: forall v e. (Show v, Eq v) =>
               v ->
               v ->
               Assertion e
               
assertEqual a b = assert (show a ++ " is not equal to " ++ show b) (a == b)

fail message = assert message false

checkReceived :: forall v e. (Show v, Eq v, Monoid v) =>
                 Tree String v ->
                 Subscriber String v ->
                 Assertion e

checkReceived tree subscriber =
  if S.isEmpty $ Sub.nacks subscriber then
    assertEqual subscriber $ Sub.make tree else
    fail $ "got nacks " ++ show (Sub.nacks subscriber) ++ " when syncing "
    ++ show tree

sync :: forall v e. (Show v, Eq v, Monoid v) =>
        Tree String v ->
        Assertion e

sync tree = 
  checkReceived tree result
  
  where iterate publisher result =
          case Pub.next publisher of
            Next update publisher ->
              iterate publisher $ Sub.apply update result
              
            End -> result

        result = iterate
          (Pub.publish (Pub.make empty) tree)
          (Sub.make empty)

expectReceived :: forall v e. (Show v, Eq v, Monoid v) =>
               Tree String v ->
               List (Update String v) ->
               Assertion e

expectReceived tree updates =
  checkReceived tree $ foldl (flip Sub.apply) (Sub.make empty) updates

checkNacks :: forall v e. (Show v, Eq v, Monoid v) =>
              Set String ->
              Subscriber String v ->
              Assertion e

checkNacks expected subscriber =
  if S.isEmpty $ Sub.nacks subscriber then
    fail $ "expected nacks " ++ show expected ++ ", but got "
    ++ show subscriber
  else
    assertEqual (Sub.nacks subscriber) expected

expectNacks :: forall v e. (Show v, Eq v, Monoid v) =>
               Set String ->
               List (Update String v) ->
               Assertion e

expectNacks expected updates =
  checkNacks expected $ foldl (flip Sub.apply) (Sub.make empty) updates

leaf1 = leaf "leaf 1"
leaf2 = leaf "leaf 2"
leaf3 = leaf "leaf 3"
intermediate = tree "intermediate" $ S.insert leaf1 $ S.singleton leaf2
root = tree "root" $ S.insert leaf3 $ S.singleton intermediate

tests = do
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

main = do
  -- seed <- randomSeed

  runTest do
    tests

    -- Test.Simulator.tests (mkSeed 42) -- seed
