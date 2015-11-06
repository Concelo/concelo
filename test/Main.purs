module Test.Main where

import Concelo.Subscriber (Next(Next, End), update, subscribe,
                           Subscriber(Subscriber), Update(Add, NewRoot))
import Concelo.Receiver (apply, receive, Receiver(Receiver))
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
import Test.Unit (assert, Assertion(), runTest, test)

fail message = assert message false

assertEqual :: forall v e. (Show v, Eq v) =>
               v ->
               v ->
               Assertion e
               
assertEqual a b = assert (show a ++ " is not equal to " ++ show b) (a == b)

checkReceived :: forall v e. (Show v, Eq v, Monoid v) =>
                 Tree String v ->
                 Receiver String v ->
                 Assertion e

checkReceived tree receiver@(Receiver r) =
  if S.isEmpty r.nacks then
    assertEqual receiver $ receive tree else
    fail $ "got nacks " ++ show r.nacks ++ " when syncing " ++ show tree

sync :: forall v e. (Show v, Eq v, Monoid v) =>
        Tree String v ->
        Assertion e

sync tree = 
  checkReceived tree result
  
  where iterate subscriber@(Subscriber s) result =
          case s.next of
            Next update subscriber -> iterate subscriber $ apply update result
            End -> result

        result = iterate
          (update (subscribe empty) tree)
          (receive empty)


expectReceived :: forall v e. (Show v, Eq v, Monoid v) =>
               Tree String v ->
               List (Update String v) ->
               Assertion e

expectReceived tree updates =
  checkReceived tree $ foldl (flip apply) (receive empty) updates

checkNacks :: forall v e. (Show v, Eq v, Monoid v) =>
              Set String ->
              Receiver String v ->
              Assertion e

checkNacks expected receiver@(Receiver r) =
  if S.isEmpty r.nacks then
    fail $ "expected nacks " ++ show expected ++ ", but got " ++ show receiver
  else
    assertEqual r.nacks expected

expectNacks :: forall v e. (Show v, Eq v, Monoid v) =>
               Set String ->
               List (Update String v) ->
               Assertion e

expectNacks expected updates =
  checkNacks expected $ foldl (flip apply) (receive empty) updates

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
      $ Cons (Add (value leaf1) S.empty)
      $ Cons (Add (value leaf2) S.empty)      
      $ Cons (NewRoot (key leaf1))
      Nil

  test "build tree from updates" do
    expectReceived root
      $ Cons (Add (value leaf3) S.empty)
      $ Cons (Add (value leaf2) S.empty)      
      $ Cons (Add (value leaf1) S.empty)
      $ Cons (Add (value intermediate)
              $ S.insert (key leaf2)
              $ S.singleton (key leaf1))
      $ Cons (Add (value root)
              $ S.insert (key intermediate)
              $ S.singleton (key leaf3))
      $ Cons (NewRoot (key root))
      Nil

  test "build tree from out-of-order updates plus unused update" do
    expectReceived root
      $ Cons (Add (value leaf2) S.empty)      
      $ Cons (NewRoot (key root))
      $ Cons (Add (value intermediate)
              $ S.insert (key leaf2)
              $ S.singleton (key leaf1))
      $ Cons (Add "unused"
              $ S.insert "nonexistent"
              $ S.singleton (key leaf1))
      $ Cons (Add (value leaf1) S.empty)
      $ Cons (Add (value root)
              $ S.insert (key intermediate)
              $ S.singleton (key leaf3))
      $ Cons (Add (value leaf3) S.empty)
      Nil

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

-- todo: test full sync process with random trees, random packet loss, and random reconnects, flushing periodically and ensuring all subscribers converge on the correct value
