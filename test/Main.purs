module Test.Main where

import Concelo.Publisher (Next(Next, End), Publisher(), Update(Add, NewRoot))
import qualified Concelo.Publisher as Pub
import Concelo.Subscriber (Subscriber())
import qualified Concelo.Subscriber as Sub
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T
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
               
assertEqual a b =
  assert ("\n" ++ show a ++ "\n\n is not equal to\n\n " ++ show b) (a == b)

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
          (Pub.publish (Pub.make T.empty) tree)
          (Sub.make T.empty)

expectReceived :: forall v e. (Show v, Eq v, Monoid v) =>
               Tree String v ->
               List (Update String v) ->
               Assertion e

expectReceived tree updates =
  checkReceived tree $ foldl (flip Sub.apply) (Sub.make T.empty) updates

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
  checkNacks expected $ foldl (flip Sub.apply) (Sub.make T.empty) updates

leaf1 = T.leaf "leaf 1"
leaf2 = T.leaf "leaf 2"
leaf3 = T.leaf "leaf 3"
intermediate = T.make "intermediate" $ S.insert leaf1 $ S.singleton leaf2
root = T.make "root" $ S.insert leaf3 $ S.singleton intermediate

tests = do
  test "remove nonexistent leaf from leaves" do
    let set = S.insert (T.key leaf3) $ S.singleton (T.key leaf1)
    assertEqual
      (Pub.removeAck "nonexistent" set root)
      set

  test "remove leaf from leaves" do
    let set = S.insert (T.key leaf3) $ S.singleton (T.key leaf1)    
    assertEqual
      (Pub.removeAck (T.key leaf3) set root)
      (S.singleton (T.key leaf1))

  test "remove nonexistent leaf from tree" do
    let set = S.singleton (T.key root)
    assertEqual
      (Pub.removeAck "nonexistent" set root)
      set

  test "remove leaf from tree" do
    let set = S.singleton (T.key root)
    assertEqual
      (Pub.removeAck (T.key leaf2) set root)
      (S.insert (T.key leaf3)
       $ S.insert (T.key intermediate)
       $ S.insert (T.key root)
       $ S.singleton (T.key leaf1))

  test "remove root from tree" do
    let set = S.singleton (T.key root)
    assertEqual
      (Pub.removeAck (T.key root) set root)
      (S.insert (T.key leaf3)
       $ S.insert (T.key intermediate)
       $ S.insert (T.key leaf2)
       $ S.singleton (T.key leaf1))
  
  test "sync leaf" do
    sync leaf1
    
  test "sync tree" do
    sync root
    
  test "build leaf from updates" do
    expectReceived leaf1
      $ Add (T.value leaf1) S.empty
      : Add (T.value leaf2) S.empty
      : NewRoot (T.key leaf1)
      : Nil

  test "build tree from updates" do
    expectReceived root
      $ Add (T.value leaf3) S.empty
      : Add (T.value leaf2) S.empty
      : Add (T.value leaf1) S.empty
      : (Add (T.value intermediate)
         $ S.insert (T.key leaf2)
         $ S.singleton (T.key leaf1))
      : (Add (T.value root)
         $ S.insert (T.key intermediate)
         $ S.singleton (T.key leaf3))
      : NewRoot (T.key root)
      : Nil

  test "build tree from out-of-order updates plus unused update" do
    expectReceived root
      $ Add (T.value leaf2) S.empty
      : NewRoot (T.key root)
      : (Add (T.value intermediate)
         $ S.insert (T.key leaf2)
         $ S.singleton (T.key leaf1))
      : (Add "unused"
         $ S.insert "nonexistent"
         $ S.singleton (T.key leaf1))
      : Add (T.value leaf1) S.empty
      : (Add (T.value root)
         $ S.insert (T.key intermediate)
         $ S.singleton (T.key leaf3))
      : Add (T.value leaf3) S.empty
      : Nil

  test "nacks on missing updates" do
    expectNacks (S.insert (T.key leaf2)
                 $ S.singleton (T.key leaf3))
      $ (Add (T.value leaf1) S.empty)
      : (Add (T.value intermediate)
              $ S.insert (T.key leaf2)
              $ S.singleton (T.key leaf1))
      : (Add (T.value root)
              $ S.insert (T.key intermediate)
              $ S.singleton (T.key leaf3))
      : NewRoot (T.key root)
      : Nil

main = do
  -- seed <- randomSeed

  runTest do
    tests

    -- Test.Simulator.tests (mkSeed 42) -- seed
