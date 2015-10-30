module Test.Main where

import Concelo.Subscriber (Next(Next, End), update, subscribe,
                           Subscriber(Subscriber), Update())
import Concelo.Receiver (apply, receive, Receiver())
import Concelo.Tree (tree, leaf, empty, Tree())
import Prelude (($), (++), unit, (==), (/=), bind, show, return, Unit(), Show,
                Eq)
import Data.Either (Either(Left, Right))
import Data.Foldable (foldr)
import Data.Set (Set())
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

sync :: forall v e. (Show v, Eq v, Monoid v) =>
        Tree String v ->
        Assertion e
        
sync tree = 
  case result of
    Right c -> assertEqual c $ receive tree
    Left nacks ->
      fail $ "got nacks " ++ show nacks ++ " when syncing " ++ show tree
      
  where apply' update (Right receiver) = apply update receiver
        apply' _ (Left nacks) = Left nacks
        
        iterate subscriber@(Subscriber s) result =
          case s.next of
            Next update subscriber -> iterate subscriber $ apply' update result
            End -> result

        result = iterate
          (update (subscribe empty) tree)
          (Right $ receive empty)

main = runTest do
  test "subscribe to leaf" do sync $ leaf "leaf"
    
  test "subscribe to tree" do
    sync $ tree "root"
      (S.insert (leaf "leaf 3")
       (S.insert (tree "intermediate"
                  (S.insert (leaf "leaf 1")
                   (S.insert (leaf "leaf 2") S.empty))) S.empty))
  

