module Test.Simulator (tests) where

import Concelo.Publisher (Publisher(), Update(Add, NewRoot))
import qualified Concelo.Publisher as Pub
import Concelo.Subscriber (Subscriber())
import qualified Concelo.Subscriber as Sub
import Concelo.Tree (Tree())
import qualified Concelo.Tree as T
import Prelude (($), (+), (-), (++), unit, (==), (<=), bind, return, Unit(),
                mod, otherwise, (<<<), show)
import Data.Foldable (foldr)
import Data.List (List(Cons, Nil), (:))
import qualified Data.List as L
import Data.Sequence (Seq())
import qualified Data.Sequence as Q
import Data.Set (Set())
import qualified Data.Set as S
import Data.Tuple (Tuple(Tuple))
import Data.Maybe (Maybe(Just, Nothing))
import Test.Unit (Assertion(), test, assert)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary )
import Test.QuickCheck.LCG (Seed(), mkSeed, runSeed, lcgNext)
import Test.QuickCheck.Gen (chooseInt, listOf)
import Test.QuickCheck (Testable, quickCheckPure)
import qualified Test.QuickCheck as QC

foreign import trace :: forall a. String -> (Unit -> a) -> a

arbitraryTree maxChildCount = do
  value :: String <- arbitrary
  childCount <- chooseInt 0 maxChildCount
  children <- listOf childCount $ arbitraryTree $ maxChildCount - 1
  return $ T.make value $ S.fromList children

data Task
  = Send
  | Reconnect
  | Flush
  | Publish
  | QueueUpdate
  | QueueNack

data Result
  = Iterate State
  | Error String
  | Done

data Simulation = Simulation
  { trees :: List (Tree String String)
  , tasks :: Tasks
  , seed :: Int }

instance arbitrarySimulation :: Arbitrary Simulation where
  arbitrary = do
    -- todo: instead of a list of N unrelated arbitrary trees, we
    -- should create a list where some consecutive trees are closely
    -- related and some aren't
    trees <- listOf 10 $ arbitraryTree 3
    tasks <- arbitrary
    seed <- arbitrary
    return $ Simulation { trees: trees
                        , tasks: tasks
                        , seed: seed }

data State = State
  { trees :: List (Tree String String)
  , tasks :: Tasks
  , messages :: Seq (State -> State)
  , publisher :: Publisher String String
  , subscriber :: Subscriber String String
  , generator :: Seed }

data Tasks = Tasks Task (Unit -> Tasks)

instance arbitraryTasks :: Arbitrary Tasks where
  arbitrary = do
    seed <- arbitrary
    return $ tasks seed

succeed = assert "" true

fail message = assert message false

check :: forall e p. (Testable p) =>
         Seed ->
         Int ->
         p ->
         Assertion e

check seed count property =
  assertSuccess $ quickCheckPure seed count property

  where assertSuccess Nil = succeed
        assertSuccess (Cons (QC.Failed message) _) = fail message
        assertSuccess (Cons _ results) = assertSuccess results

pickNumber min max generator =
  { value: (mod (runSeed generator) (max - min + 1)) + min
  , generator: lcgNext generator }

pickMessage messages generator =
  case Q.uncons messages of
    Just (Tuple first afterFirst) ->
      case Q.uncons afterFirst of
        Just (Tuple second afterSecond) ->
          let reorder = pickNumber 0 9 generator in
          { message: Just if reorder.value == 0 then second else first
          , messages: if reorder.value == 0 then
                        Q.cons first afterSecond else
                        afterFirst
          , generator: reorder.generator }

        Nothing ->
          { message: Just first
          , messages: afterFirst
          , generator: generator }

    Nothing ->
      { message: Nothing
      , messages: messages
      , generator: generator }

maybeDrop picked =
  picked { message = if drop.value == 0 then Nothing else picked.message
         , generator = drop.generator }

  where drop = pickNumber 0 9 picked.generator

apply Send (State state) =
  Iterate case result.message of
    Just message -> message state'
    Nothing -> state'

  where result = maybeDrop $ pickMessage state.messages state.generator
        state' = State state { generator = result.generator
                             , messages = result.messages }

apply Flush state =
  iterate state 1000

  where iterate state@(State s) try
          | Sub.root s.subscriber == Pub.root s.publisher =
            if Pub.consistent s.publisher then
               if L.null s.trees then
                 Done else
                 Iterate state else
            Error "inconsistent publisher"
            
          | try <= 0 =
            Error "flush failed"

          | otherwise =
            let task = head s.tasks
                state' = State s { tasks = tail s.tasks } in
            
            case task of
              Publish -> iterate state' try
              Reconnect -> iterate state' try
              Flush -> iterate state' try              
              _ ->
                let result = apply task state' in
                case result of
                  Iterate state'' ->
                    iterate state'' (try - 1)
                      
                  _ -> result

apply Reconnect state@(State s) =
  iterate (State s { generator = delay.generator }) delay.value

  where delay = pickNumber 10 1000 s.generator
        
        iterate state@(State s) delay
          | delay <= 0 =
            Iterate $ State s
              { publisher = Pub.publish (Pub.make (Sub.root s.subscriber))
                            $ Pub.root s.publisher
              , subscriber = Sub.make $ Sub.root s.subscriber
              , messages = Q.empty }

          | otherwise =
            let task = head s.tasks
                state' = State s { tasks = tail s.tasks } in
            
            case task of
              Send -> iterate state' delay
              Flush -> iterate state' delay
              Reconnect -> iterate state' delay              
              _ ->
                let result = apply task state' in
                case result of
                  Iterate state'' ->
                    iterate state'' (delay - 1)
                      
                  _ -> result
  
apply Publish state@(State s) =
  Iterate case s.trees of
    Cons tree trees ->
      State s { publisher = Pub.publish s.publisher tree
              , trees = trees }

    Nil -> state

apply QueueUpdate state@(State s) =
  Iterate $ send s.publisher

  where receive update (State s) =
          State s { subscriber = Sub.apply update s.subscriber }
                    
        send publisher =
          case Pub.next publisher of
            Pub.Next update publisher ->
              State s { publisher = publisher
                      , messages = Q.snoc s.messages (receive update) }

            Pub.End -> state

apply QueueNack state@(State s) =
  Iterate $ send s.subscriber

  where receive nack (State s) =
          State s { publisher = Pub.nack s.publisher nack }

        send subscriber =
          case Sub.next subscriber of
            Sub.Next nack subscriber ->
              State s { subscriber = subscriber
                      , messages = Q.snoc s.messages (receive nack) }

            Sub.End -> state

taskList
  = Tuple 40 Send
  : Tuple 20 QueueUpdate
  : Tuple 20 QueueNack
  : Tuple 10 Flush
  : Tuple 10 Publish
  : Tuple  1 Reconnect
  : Nil

taskTotalWeight = foldr (\(Tuple weight _) sum -> sum + weight) 0 taskList

pickTask generator =
  { value: pick which.value taskList
  , generator: which.generator }
  
  where which = pickNumber 0 taskTotalWeight generator
        pick n Nil = Send
        pick n (Cons (Tuple weight task) tail) =
          if n <= weight then task else pick (n - weight) tail

tasks seed =
  Tasks which.value next

  where generator = mkSeed seed
        which = pickTask generator
        next _ = tasks $ runSeed which.generator

head (Tasks task _) = task

tail (Tasks _ next) = next unit
        
run (Simulation s) =
  iterate $ State { trees: s.trees
                  , tasks: s.tasks
                  , messages: Q.empty
                  , publisher: Pub.make T.empty
                  , subscriber: Sub.make T.empty
                  , generator: mkSeed s.seed }

  where iterate state@(State s) =
          case apply (head s.tasks) $ State s { tasks = tail s.tasks } of
            Iterate state -> iterate state
            Error message -> QC.Failed message
            Done -> QC.Success
                    
tests seed = do
  test ("sync random trees with packet loss and reconnects (seed "
        ++ show seed ++ ")") do
    check seed 10 run
  
