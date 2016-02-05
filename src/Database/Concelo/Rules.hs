module Database.Concelo.Rules
  ( parse ) where

data Fields = Fields { getFieldEnv :: Trie Key ()
                     , getFieldString :: String }

type Parser a = StateT Fields (ErrorT () Identity) a

runParser grammar fields =
  runIdentity $ runErrorT $ evalStateT (Parser grammar) fields

eos expression = do
  s <- get fieldString >>= skipSpace
  if null s then return expression else throwError ()

skipSpace all@(c:cs)
  | isSpace c = cs
  | otherwise = all
skipSpace [] = []

subtractPrefix p:ps c:cs
  | p == c = subtractPrefix ps cs
  | otherwise = Nothing
subtractPrefix [] cs = Just cs
subtractPrefix _ _ = Nothing

terminal t = do
  s <- get fieldString >>= skipSpace
  case subtractPrefix t s of
    Just s' -> do
      set fieldString s'
      return ()
    Nothing -> throwError ()

ternary parser expression = do
  a <- boolean
  terminal "?"
  b <- parser
  terminal ":"
  parser >>= return . expression a b

binary parser operator expression = do
  a <- parser
  terminal operator
  parser >>= return . expression a

unary parser operator expression = do
  terminal operator
  parser >>= return . expression

call1 object method argument expression = do
  a <- object
  terminal "."
  terminal method
  group argument >>= return . expression a

call0 object method argument expression = do
  a <- object
  terminal "."
  terminal method
  group void
  return $ expression a

booleanOperation
  =   binary boolean "&&" And
  >>| binary boolean "||" Or
  >>| unary boolean "!" Not

booleanCall
  =   call1 snapshot "hasChild" string HasChild
  >>| call1 snapshot "hasChildren" (optional stringArray) HasChildren
  >>| call0 snapshot "exists" Exists
  >>| call0 snapshot "isNumber" IsNumber
  >>| call0 snapshot "isString" IsString
  >>| call0 snapshot "isBoolean" IsBoolean
  >>| call1 string "contains" string Contains
  >>| call1 string "beginsWith" string BeginsWith
  >>| call1 string "endsWith" string EndsWith
  >>| call1 string "matches" string Matches

comparison
  =   binary number "===" NumberEqual
  >>| binary number "!==" \a b -> Not $ NumberEqual a b
  >>| binary number "<" NumberLessThan
  >>| binary number ">" NumberGreaterThan
  >>| binary number "<=" \a b -> Not $ NumberGreaterThan
  >>| binary string "===" StringEqual
  >>| binary string "!==" \a b -> Not $ StringEqual a b
  >>| binary string "<" StringLessThan
  >>| binary string ">" StringGreaterThan
  >>| binary string "<=" \a b -> Not $ StringGreaterThan
  >>| binary string ">=" \a b -> Not $ StringLessThan
  >>| binary boolean "===" BooleanEqual
  >>| binary boolean "!==" \a b -> Not $ BooleanEqual a b
  >>| binary boolean "<" BooleanLessThan
  >>| binary boolean ">" BooleanGreaterThan
  >>| binary boolean "<=" \a b -> Not $ BooleanGreaterThan
  >>| binary boolean ">=" \a b -> Not $ BooleanLessThan

booleanTernary = ternary boolean BooleanIfElse

void = return ()

group parser = do
  terminal "("
  a <- parser
  terminal ")"
  return a

numberLiteralPrefix c:cs
  | isDigit c = digitsOrDot [c] cs where
    digitsOrDot acc all@(c:cs)
      | isDigit c = digitsOrDot (c : acc) cs
      | c == '.' = digits (c : acc) cs
      | otherwise = Just (reverse acc, all)
                    
    digitsOrDot acc [] = Just (reverse acc, [])

    digits acc c:cs
      | isDigit c = digits (c : acc) cs
                    
      | otherwise = Just (reverse acc, all)

  | otherwise = Nothing

numberLiteralPrefix [] = Nothing

numberLiteral = do
  s <- get fieldString >>= skipSpace
  case numberLiteralPrefix s of
    Just (n, s') -> do
      set fieldString s'
      return $ NumberLiteral n
    Nothing -> throwError ()  

numberReference = terminal "now" >> return Now

numberOperation
  =   binary number "+" Add
  >>| binary number "-" Subtract
  >>| binary number "*" Multiply
  >>| binary number "/" Divide
  >>| binary number "%" Modulo
  >>| unary number "-" Negate

numberCall
  =   call0 snapshot "val" NumberVal
  >>| call0 snapshot "getPriority" Priority

field object field expression = do
  a <- object
  terminal field
  return $ expression a

numberField = field string "length" Length

numberTernary = ternary number NumberIfElse

stringLiteralPrefix c:cs
  | c == '"' = unquoted [c] cs where
    unquoted acc c:cs
      | c == '"' = Just (reverse (c : acc), cs)
      | c == '\\' = quoted acc cs
      | otherwise = unquoted (c : acc) cs
    unquoted _ [] = Nothing

    quoted acc c:cs
      | c == '"' = unquoted (c : acc) cs
      | otherwise = unquoted ('\\' : c : acc) cs

    quoted _ [] = Nothing

  | otherwise = Nothing
                
stringLiteralPrefix [] = Nothing

stringLiteral = do
  s <- get fieldString >>= skipSpace
  case stringLiteralPrefix s of
    (p, s') -> do
      set fieldString s'
      return $ StringLiteral p
    Nothing -> throwError ()  

stringOperation = binary string "+" Concatenate

stringCall
  =   call2 string "replace" string string Replace
  >>| call0 string "toLowerCase" ToLowerCase
  >>| call0 string "toUpperCase" ToUpperCase

stringField = do
  a <- field auth "uid" Uid
  set fieldUsingUid True
  return a

stringTernary = ternary string StringIfElse

auth = terminal "auth" >> return Auth

snapshotReference
  =   terminal "root" >> return Root
  >>| terminal "data" >> return Data
  >>| terminal "newData" >> return NewData

snapshotCall
  =   call1 snapshot "child" string Child
  >>| call0 snapshot "parent" Parent

snapshot
  =   snapshotReference
  >>| snapshotCall

number
  =   numberLiteral
  >>| numberReference
  >>| numberOperation
  >>| numberCall
  >>| numberField  
  >>| numberTernary
  >>| group number

string
  =   stringLiteral
  >>| stringOperation
  >>| stringCall
  >>| stringField  
  >>| stringTernary
  >>| group string

boolean
  =   booleanLiteral
  >>| booleanOperation
  >>| booleanCall
  >>| comparison
  >>| booleanTernary
  >>| group boolean

annotate expression = do
  usingUid <- get fieldUsingUid
  return if usingUid then UsingUid expression else expression

evalACL expression context acl =
  error "todo"

evalBoolean expression context acl =
  error "todo"

parseRule evaluate value env =
  case value of
    J.String s -> do
      runParser (boolean >>= eos >>= annotate) (Fields env xs False)
        >>= return . evaluate
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
        ".read" -> parseRule evalACL value env >>= update ruleRead
        ".write" -> parseRule evalACL value env >>= update ruleWrite
        ".validate" -> parseRule evalBoolean value env >>= update ruleValidate
        ".indexOn" -> parseIndexOn value >>= update ruleIndexOn
        
        name@('$' : _) ->
          parseTrie value (T.insert name env) >>= update ruleWildCard
          
          _ -> parseTrie value env >>= Right $ T.super key

parse json =
  parseTrie (decode json) T.empty where

