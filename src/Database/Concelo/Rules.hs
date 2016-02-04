module Database.Concelo.Rules
  ( parse ) where

parseExpression expression env =
  error "todo"

parseRule value env =
  case value of
    J.String expression -> do
      parseExpression expression env >>= return . evaluate
    _ -> Left "unexpected type in rule"

parseIndexOn value =
  case value of
    J.Array names -> Right $ foldr T.insert T.empty names
    J.String name -> T.singleton name
    _ -> Left "unexpected type in index"

parseTrie value env =
  (case value of
      J.Object map -> foldrWithKey fold (Right T.empty) map
      _ -> Left "unexpected type in rules") where

    fold key value eitherTrie = do
      trie <- eitherTrie
      let rule = fromMaybe empty $ T.getValue trie
          update lens value =
            Right $ L.set T.value (L.set ruleRead value rule) trie
      
      case key of
        ".read" -> parseRule value env >>= update ruleRead
        ".write" -> parseRule value env >>= update ruleWrite
        ".validate" -> parseRule value env >>= update ruleValidate
        ".indexOn" -> parseIndexOn value >>= update ruleIndexOn
        
        name@('$' : _) ->
          parseTrie value (T.insert name env) >>= update ruleWildCard
          
          _ -> parseTrie value env >>= Right $ T.super key

parse json =
  parseTrie (decode json) T.empty where

