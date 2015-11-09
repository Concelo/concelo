module Simulator where

data Result a
  = Iterate a
  | Error String
  | Done

check :: forall e p. (Testable p) =>
         Seed ->
         Int ->
         p ->
         Assertion e

check seed count property =
  assertSuccess $ quickCheckPure seed count property

  where assertSuccess Nil = success
        assertSuccess (Cons (Failed message) _) = failure message
        assertSuccess (Cons _ results) = assertSuccess results

pickNumber min max generator =
  { value: (mod (runSeed generator) (max - min + 1)) + min
  , generator: lcgNext generator }

pickMessage messages generator
  | Q.length messages == 0 =
    { message: Nothing
    , messages: messages
    , generator: generator }

  | otherwise =
      { message: if send.value == 0 then
                   Just (Q.index index.value messages) else
                   Nothing
      , messages: remove index.value messages
      , generator: send.generator }

  where index = pickNumber 0 (Q.length messages) generator
        send = pickNumber 0 9 index.generator

send state =
  Iterate case result.message of
    Just message -> message next
    Nothing -> next

  where result = pickMessage state.messages state.generator
        next = state { generator = result.generator
                     , messages = result.messages }

flush state =
  iterate state 1000

  where iterate state try
          | Sub.root state.subscriber == Pub.root state.publisher =
            case state.trees of
              Cons _ _ -> Iterate state
              Nil -> Done
            
          | try <= 0 =
            Error "flush failed"

          | otherwise =
            let task = head state.tasks
                state = state { tasks: tail state.tasks } in
            
            case task of
              Publish -> iterate state try
              Reconnect -> iterate state try              
              _ ->
                let result = execute task state in
                case result of
                  Iterate state ->
                    iterate state (try - 1)
                      
                  _ -> result in

reconnect state =
  case pickNumber 10 1000 state.generator of
    Tuple delay generator -> iterate state { generator = generator } delay

  where iterate state delay
          | delay <= 0 =
            Iterate state { publisher = publisher
                                         $ publish (Pub.root state.publisher)
                                         $ Sub.root state.subscriber
                          , subscriber = subscriber $ Sub.root state.subscriber
                          , messages = Q.empty }

          | otherwise =
            let task = head state.tasks
                state = state { tasks: tail state.tasks } in
            
            case task of
              Send -> iterate state delay
              Flush -> iterate state delay              
              _ ->
                let result = execute task state in
                case result of
                  Iterate state ->
                    iterate state (delay - 1)
                      
                  _ -> result in
  
publish state =
  Iterate case state.trees of
    Cons tree trees ->
      Iterate state { publisher = update state.publisher tree
                    , trees = trees }

      Nil -> Iterate state

queueUpdate state =
  Iterate $ send state.publisher

  where receive update state =
          state { subscriber = apply update state.subscriber }
                    
        send publisher =
          case Pub.next publisher of
            Pub.Next update publisher ->
              state { publisher = publisher
                    , messages = Q.cons (receive update) state.messages }

            Pub.End -> state in

queueNack state =
  Iterate $ send state.publisher

  where receive nack state =
          state { publisher = Pub.nack nack state.publisher }

        send subscriber =
          case Sub.next subscriber of
            Sub.Next nack subscriber ->
              state { subscriber = subscriber
                    , messages = Q.cons (receive nack) state.messages }

            Sub.End -> state in

type State =
  { trees :: List (Tree String String)
  , tasks :: Tasks
  , messages :: Sequence
  , publisher :: Publisher String String
  , subscriber :: Subscriber String String }

instance arbitraryTree :: Arbitrary (Tree String String) where
  arbitrary = do
    value <- arbitrary
    childCount <- chooseInt 0 3
    children <- listOf childCount arbitrary
    return $ tree value $ S.fromList children

data Tasks = Tasks (State -> Result) (Unit -> Tasks)

instance arbitraryTasks :: Arbitrary Tasks where
  arbitrary = do
    seed <- arbitrary
    return tasks seed

taskList
  = Tuple 40 send
  : Tuple 20 queueUpdate
  : Tuple 20 queueNack
  : Tuple 10 flush
  : Tuple 10 publish
  : Tuple  1 reconnect
  : Nil

taskTotalWeight = foldr (\(Tuple weight _) sum -> sum + weight) 0 taskList

pickTask generator =
  { value: pick which.value taskList
  , generator: which.generator }
  
  where which = pickNumber 0 total generator
        pick n Nil = send
        pick n (Cons (Tuple weight task) tail) =
          if n <= weight then task else pick (n - weight) tail

tasks seed =
  Tasks which.value next

  where generator = mkSeed seed
        which = pickTask generator
        next _ = tasks which.generator

data Simulation = Simulation
  { trees :: List (Tree String String)
  , tasks :: Tasks }

instance arbitrarySimulation :: Arbitrary Simulation where
  arbitrary = do
    trees <- listOf 1000 arbitrary
    tasks <- arbitrary
    return { trees: trees
           , tasks: tasks }

head (Tasks task _) = task

tail (Tasks _ next) = next unit
        
run (Simulation s) =
  iterate { trees: s.trees
          , tasks: s.tasks
          , messages: Q.empty
          , publisher: publisher empty
          , subscriber: subscriber empty }

  where iterate state =
          case (head state.tasks) state { tasks: tail state.tasks } of
            Iterate state -> iterate state
            Error message -> Failed message
            Done -> Success
                    
main = do
  seed <- randomSeed

  log "using seed " ++ show seed

  test "sync random trees with packet loss and reconnects" do
    check seed 100 run
  
