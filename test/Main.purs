module Test.Main where

import Concelo.Publisher (Update())
import qualified Concelo.Publisher as Pub
import Concelo.Subscriber (Subscriber())
import qualified Concelo.Subscriber as Sub
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T
import Prelude (($), (++), (==), bind, show, Show, Eq, flip)
import Data.Foldable (foldl)
import Data.Set (Set())
import Data.List (List(Nil), (:))
import Data.Monoid (Monoid)
import qualified Data.Set as S
import Data.Either (Either(Left, Right))
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
  case result of
    Left error -> fail error
    Right subscriber -> checkReceived tree subscriber
  
  where iterate publisher result =
          if Pub.consistent publisher then
            case Pub.next publisher of
              Pub.Next update publisher ->
                let subscriber = Sub.apply update result in
                case Sub.next subscriber of
                  Sub.End -> iterate publisher subscriber
                  _ -> Left $ "got nack while syncing " ++ show tree
              
              Pub.End -> Right result else

            Left "inconsistent publisher"

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
  test "sync leaf" do
    sync leaf1
    
  test "sync tree" do
    sync root
    
  test "build leaf from updates" do
    expectReceived leaf1
      $ Pub.add leaf1
      : Pub.add leaf2
      : Pub.newRoot leaf1
      : Nil

  test "build tree from updates" do
    expectReceived root
      $ Pub.add leaf3
      : Pub.add leaf2
      : Pub.add leaf1
      : Pub.add intermediate
      : Pub.add root
      : Pub.newRoot root
      : Nil

  test "build tree from out-of-order updates plus unused update" do
    expectReceived root
      $ Pub.add leaf2
      : Pub.newRoot root
      : Pub.add intermediate
      : (Pub.Add "unused" "unused"
         $ S.insert "nonexistent"
         $ S.singleton (T.key leaf1))
      : Pub.add leaf1
      : Pub.add root
      : Pub.add leaf3
      : Nil

  test "nacks on missing updates" do
    expectNacks (S.insert (T.key leaf2)
                 $ S.singleton (T.key leaf3))
      $ Pub.add leaf1
      : Pub.add intermediate
      : Pub.add root
      : Pub.newRoot root
      : Nil

main = do
  -- seed <- randomSeed

  runTest do
    tests

    Test.Simulator.tests (mkSeed 42) -- seed
