module Database.Concelo.Rules
  ( parse ) where

data Fields = Fields { getFieldEnv :: Trie Key ()
                     , getFieldString :: String }

-- type Parser a = StateT Fields (ErrorT () Identity) a

-- todo: enforce operator precedence and associativity

-- todo: use a custom error type which distinguishes between recoverable (i.e. try the next alternative) and unrecoverable (i.e. return an error even if there's an alternative) errors

-- todo: perhaps the parser should just create an Error/State monad instead of an explicit AST

runParser parser fields =
  runIdentity $ runErrorT $ evalStateT parser fields

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
  update fieldString skipSpace
  prefix t

prefix t =
  fmap (subtractPrefix t) (get fieldString) >>= \case
    Just s' -> do
      set fieldString s'
      return t
    Nothing -> throwError ()

optional parser
  =   fmap Just parser
  >>| return Nothing

ternary parser expression = do
  a <- boolean
  terminal "?"
  b <- parser
  terminal ":"
  fmap (expression a b) parser

binary parser operator expression = do
  a <- parser
  terminal operator
  fmap (expression a) parser

unary parser operator expression = do
  terminal operator
  fmap expression parser

call1 object method argument expression = do
  a <- object
  terminal "."
  terminal method
  fmap (expression a) (group argument)

call0 object method argument expression = do
  a <- object
  terminal "."
  terminal method
  group void
  return $ expression a

intersperse delimiter parser =
  optional parser >>= \case
    Just a ->
      delimiter >> fmap (a:) (intersperse delimiter parser)
      >>| [a]
    Nothing -> return []

stringArray = do
  terminal "["
  a <- intersperse (terminal ",") string
  terminal "]"
  return a

element code expression =
  fmap expression $ prefix code

alternative = do
  a <- patternElement
  prefix "|"
  fmap (Alternative a) patternElement

escaped = prefix "\\" >> fmap Character character

atom
  =   element "\s" Space
  >>| element "\w" WordCharacter
  >>| element "\d" Digit
  >>| element "\S" (Neg . Space)
  >>| element "\w" (Neg . WordCharacter)
  >>| element "\d" (Neg . Digit)
  >>| escaped
  >>| unescaped

unescaped =
  character >>= \case
    ']' -> throwError ()
    '$' -> throwError ()    
    _ -> return $ Character c

interval = do
  a <- unescaped
  prefix "-"
  fmap (Interval a) unescaped

zeroOrMore parser =
  optional parser >>= \case
    Just a -> fmap (a:) parser >>| [a]
    Nothing -> return []

characterSet = do
  prefix "["
  negate <- (optional $ prefix "^") >>= isJust
  atoms <- zeroOrMore (atom >>| interval)
  prefix "]"
  return $ if negate then Neg else id $ foldr Alternative Fail atoms

suffix s = do
  a <- patternElement
  prefix s
  return a

patternElement
  =   group pattern
  >>| suffix "*" ZeroOrMore
  >>| suffix "+" OneOrMore
  >>| suffix "?" ZeroOrOne
  >>| alternative
  >>| characterSet
  >>| element "." Wildcard
  >>| atom
  >>| element "]" (Character ']')

pattern ignoreCase = do
  anchorStart <- fmap isJust $ optional $ prefix "^"
  elements <- zeroOrMore patternElement
  anchorEnd <- fmap isJust $ optional $ prefix "$"
  return $ Pattern ignoreCase anchorStart anchorEnd
    $ foldr Sequence Success elements
  
regex = do
  p <- stringLiteral' '/'
  ignoreCase <- fmap isJust $ optional $ terminal 'i'
  case runParser (pattern >>= eos) (RegexState p) of
    Right result -> return result
    Left error -> throwError error

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
  >>| call1 string "matches" regex Matches

comparison
  =   binary number "===" Equal
  >>| binary number "!==" \a b -> Not $ Equal a b
  >>| binary number "<" LessThan
  >>| binary number ">" GreaterThan
  >>| binary number "<=" \a b -> Not $ GreaterThan
  >>| binary string "===" Equal
  >>| binary string "!==" \a b -> Not $ Equal a b
  >>| binary string "<" LessThan
  >>| binary string ">" GreaterThan
  >>| binary string "<=" \a b -> Not $ GreaterThan
  >>| binary string ">=" \a b -> Not $ LessThan
  >>| binary boolean "===" Equal
  >>| binary boolean "!==" \a b -> Not $ Equal a b
  >>| binary boolean "<" LessThan
  >>| binary boolean ">" GreaterThan
  >>| binary boolean "<=" \a b -> Not $ GreaterThan
  >>| binary boolean ">=" \a b -> Not $ LessThan

booleanTernary = ternary boolean IfElse

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

numberLiteral =
  get fieldString >>= fmap numberLiteralPrefix skipSpace >>= \case
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

numberTernary = ternary number IfElse

stringLiteralBody delimiter =
  character >>= unescaped where
    unescaped c
      | c == delimiter = return []
      | c == '\\' = character >>= escaped
      | otherwise = character >>= fmap (c:) unescaped

    escaped c
      | c == delimiter = character >>= fmap (c:) unescaped
      | otherwise = character >>= fmap ('\\':c:) unescaped

stringLiteral' delimiter = do
  terminal [delimiter]
  s <- stringLiteralBody delimiter
  return s

stringLiteral = fmap StringLiteral $ stringLiteral' '"'

stringReference =
  get fieldEnv >>= foldr fold (throwError ()) where
    fold key alternative =
      fmap StringReference (terminal key)
      >>| alternative

stringOperation = binary string "+" Concatenate

stringCall
  =   call2 string "replace" string string Replace
  >>| call0 string "toLowerCase" ToLowerCase
  >>| call0 string "toUpperCase" ToUpperCase

stringField = do
  a <- field auth "uid" Uid
  set fieldUsingUid True
  return a

stringTernary = ternary string IfElse

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
  >>| stringReference
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

evalACL lens (UsingUid expression) context acl =
  foldr fold acl $ acListsBlackList $ L.get lens acl where
    fold uid acl
      | evalRule expression (L.set contextMe uid) = whiteList lens uid acl
      | otherwise = acl

evalACL lens expression context acl
  | evalRule expression context = whiteListAll lens acl
  | otherwise = blackListAll lens acl

evalRule expression context =
  case runErrorT $ evalBoolean expression context of
    Right v -> v
    Left _ -> False

hasValueOfType trie type' =
  fromMaybe false
  (fmap valueType (T.value trie) >>= \case
      Number -> return true
      _ -> return false)

test parser p = parser >>= \x -> if p x then return x else throwError ()

evalMatch = \case
  Character c -> prefix [c]
  Sequence a b -> evalMatch a >> evalMatch b
  Alternative a b -> evalMatch a >>| const evalMatch b
  Space -> test character isSpace
  WordCharacter -> test character \c -> (isAlphaNum c) || c == '_'
  Digit -> test character isDigit
  Neg a -> do
    result <- evalMatch a >> return True >>| return False
    if result then throwError() else return ()
  Interval a b -> test character \c -> ord c >= ord a && ord c <= ord b
  ZeroOrMore a -> zeroOrMore $ evalMatch a
  OneOrMore a -> evalMatch a >>= zeroOrMore (evalMatch a)
  ZeroOrOne a -> optional $ evalMatch a
  WildCard -> character
  Fail -> throwError ()
  Success -> return ()
     
tryMatch =
  get matchStateMatchers >>= \case
    [] -> return ()
    m:ms -> do
      set matchStateMatchers ms
      evalMatch m

match anchorStart anchorEnd =
  again where
    again
      =   tryMatch >> if anchorEnd then eos else return ()
      >>| const if anchorStart then
                  throwError () else
                  update position (drop 1) >> again

matches string (Pattern ignoreCase anchorStart anchorEnd elements) =
  case runParser (match anchorStart anchorEnd)
       (MatchState string elements ignoreCase) of
    Right _ -> True
    Left _ -> False

lift3 f a b c = liftM3 f (eval a) (eval b) (eval c)
lift2 f a b = liftM2 f (eval a) (eval b)
lift1 f a = fmap f $ eval a

eval = \case
  And a b -> lift2 (&&) a b
  Or a b -> lift2 (||) a b
  Not a -> lift1 not a
    
  HasChild v key -> lift2 hasChild v key where
    hasChild v key = not $ null $ T.sub key $ getVisitorTrie v
    
  HasChildren v (Just keys) -> lift2 hasChildren v keys where
    hasChildren v keys = foldr fold True keys
    fold key result = (not $ null $ T.sub key $ getVisitorTrie v) && result
      
  HasChildren v Nothing -> lift1 hasChildren v where
    hasChildren v = not $ null $ T.keys $ getVisitorTrie v
    
  Exists v -> lift1 exists v where
    exists v = not $ null $ getVisitorTrie v

  IsNumber v -> lift1 isNumber v where
    isNumber v = maybe False (const True)
                   $ (T.value $ getVisitorTrie v) >>= valueNumber

  IsString v -> lift1 isString v where
    isString v = maybe False (const True)
                 $ (T.value $ getVisitorTrie v) >>= valueString

  IsBoolean v -> lift1 isBoolean v where
    isBoolean v = maybe False (const True)
                  $ (T.value $ getVisitorTrie v) >>= valueBoolean
    
  Contains string substring -> lift2 isInfixOf substring string
  BeginsWith string substring -> lift2 isPrefixOf substring string
  EndsWith string substring -> lift2 isSuffixOf substring string
  Matches string pattern -> lift2 matches string pattern
    
  Equal a b -> lift2 (==) a b
  LessThan a b -> lift2 (<) a b
  GreaterThan a b -> lift2 (>) a b
  IfElse a b c -> lift3 (\a b c -> if a then b else c) a b c
    
  Add a b -> lift2 (+) a b
  Subtract a b -> lift2 (-) a b
  Multiply a b -> lift2 (*) a b
  Divide a b -> lift2 (/) a b
  Modulo a b -> lift2 mod a b
  Negate a b -> lift1 (0.0-) a
    
  NumberVal v -> maybeToEither () (T.value (getVisitorTrie $ eval v)
                                   >>= valueNumber)

  StringVal v -> maybeToEither () (T.value (getVisitorTrie $ eval v)
                                   >>= valueString)

  BooleanVal v -> maybeToEither () (T.value (getVisitorTrie $ eval v)
                                    >>= valueBoolean)

  Priority v -> maybeToEither () (T.value (getVisitorTrie $ eval v)
                                  >>= valuePriority)
                     
  Length a -> lift1 length a
  StringLiteral a -> return a
  StringReference a -> maybeToEither () T.find a $ getContextEnv context
  Concatenate a b -> lift2 (++) a b
    
  Replace string substring replacement ->
    lift3 substring replacement string
      
  ToLowerCase a -> lift1 toLower a
  ToUpperCase a -> lift1 toUpper a
  
  Uid _ -> return $ getContextMe context
  Auth -> return ()
  Root -> return $ getContextRoot context
  Data -> return $ getContextData context
  NewData -> return $ getContextNewData context
    
  Child v key -> lift2 child v key where
    child v key = T.sub key $ getVisitorTrie v

  Parent v -> eval v >>= maybeToEither () . getVisitorParent

parseRule evaluate env = \case
  J.String s -> do
    fmap evaluate
      $ runParser (boolean >>= eos >>= annotate) (ParseState env xs False)
  _ -> Left "unexpected type in rule"

parseIndexOn = \case
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
        ".read" -> parseACLRule aclReadLists env value >>= update ruleRead
        ".write" -> parseACLRule aclWriteLists env value >>= update ruleWrite
        ".validate" -> parseBooleanRule env value >>= update ruleValidate
        ".indexOn" -> parseIndexOn value >>= update ruleIndexOn
        
        name@('$' : _) ->
          parseTrie value (T.insert name env) >>= update ruleWildCard
          
          _ -> parseTrie value env >>= Right $ T.super key

parse json =
  parseTrie (decode json) T.empty where

