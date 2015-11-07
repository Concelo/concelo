module Simulator where

data Task
  = Send
  | Flush
  | Reconnect
  | Publish
  | QueueUpdate
  | QueueNack

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
        
execute task state =
  case task of
    Send -> Iterate
      case pickMessage state.messages state.generator of
        Tuple (Just message) generator ->
          message state { generator = generator }

        Tuple Nothing generator ->
          state { generator = generator }

    Flush ->
      let iterate state try
          | Rec.root state.receiver == Sub.root state.subscriber =
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
      
      iterate state 1000

    Reconnect ->
      let iterate state delay
          | delay <= 0 =
            Iterate state { subscriber = subscribe
                                         $ publish (Sub.root state.subscriber)
                                         $ Rec.root state.receiver
                          , receiver = receive $ Rec.root state.receiver
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
            
      case pickNumber 100 1000 state.generator of
        Tuple delay generator ->
          iterate state { generator = generator } delay
  
    Publish -> Iterate
      case state.trees of
        Cons tree trees ->
          Iterate state { subscriber = update state.subscriber tree
                        , trees = trees }

          Nil -> Iterate state

    QueueUpdate -> Iterate
      let receive update state =
            state { receiver = apply update state.receiver }
                    
          send subscriber =
            case Sub.next subscriber of
              Next update subscriber ->
                state { subscriber = subscriber
                      , messages = Q.cons (receive update) state.messages }

              End -> state in
              
         send state.subscriber

    QueueNack -> Iterate
      let receive nack state =
            state { subscriber = Sub.nack nack state.subscriber }

          send receiver =
            case S.toList $ Rec.nacks receiver of
              Cons nack _ ->
                state { messages = Q.cons (receive nack) state.messages }

              Nil -> state in

        send state.receiver
        
simulate tasks trees =
  iterate { trees: trees
          , tasks: tasks
          , messages: Q.empty
          , receiver: receive empty
          , subscriber: subscribe empty }

  where iterate state =
          case execute (head state.tasks) state { tasks: tail state.tasks } of
            Iterate state -> iterate state
            Error message -> Failed message
            Done -> Success
                    
main = do
  seed <- randomSeed

  log "using seed " ++ show seed

  test "sync random trees with packet loss and reconnects" do
    check seed 100 simulate
  
