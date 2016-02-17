module Database.Concelo.Rules
  ( parse ) where

data Fields = Fields { getFieldEnv :: Trie Key ()
                     , getFieldString :: String }

data Exception = NoParse | Error String

-- type Parser a = StateT Fields (ErrorT Exception Identity) a

-- todo: enforce operator precedence and associativity

runParser parser fields =
  runIdentity $ runErrorT $ evalStateT parser fields

noParse = throwError NoParse

eos expression = do
  s <- get fieldString >>= skipSpace
  if null s then return expression else noParse

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
  subtractPrefix t <$> get fieldString >>= \case
    Just s' -> do
      set fieldString s'
      return t
    Nothing -> noParse

optional parser
  =   Just <$> parser
  >>| return Nothing

ternary parser = do
  a <- boolean
  terminal "?"
  b <- parser
  terminal ":"
  c <- parser
  return $ liftM3 (\a b c -> if a then b else c) a b c

binary parser operator function = do
  a <- parser
  terminal operator
  b <- parser
  return $ liftM2 function a b

unary parser operator function = do
  terminal operator
  a <- parser
  return $ fmap function a

call1 object method argument function =
  callM1 object method argument (liftM2 function)

callM1 object method argument action = do
  a <- object
  terminal "."
  terminal method
  b <- argument
  return $ action a b

call0 object method argument function = do
  callM0 object method argument (fmap function)

callM0 object method argument action = do
  a <- object
  terminal "."
  terminal method
  group void
  return $ action a

intersperse delimiter parser =
  optional parser >>= \case
    Just a ->
      delimiter >> (a:) <$> intersperse delimiter parser
      >>| [a]
    Nothing -> return []

stringArray = do
  terminal "["
  a <- intersperse (terminal ",") string
  terminal "]"
  return a

element code expression =
  expression <$> prefix code

alternative = do
  a <- patternElement
  prefix "|"
  b <- patternElement
  return (a <|> b)

escaped = prefix "\\" >> (prefix . (:[])) <$> character

isWordCharacter c = isAlphaNum c || c == '_'

atom
  =   element "\s" (test character isSpace)
  >>| element "\w" (test character isWordCharacter)
  >>| element "\d" (test character isDigit)
  >>| element "\S" (test character (not . isSpace))
  >>| element "\w" (test character (not . isWordCharacter))
  >>| element "\d" (test character (not . isDigit))
  >>| escaped
  >>| unescaped

neg parser = do
  result <- parser >> return True >>| return False
  if result then noParse else return ()

unescaped =
  character >>= \case
    ']' -> noParse
    '$' -> noParse
    _ -> return $ prefix [c]

interval = do
  a <- unescaped
  prefix "-"
  b <- unescaped
  return (test character \c -> ord c >= ord a && ord c <= ord b)

zeroOrMore parser =
  optional parser >>= \case
    Just a -> (a:) <$> parser >>| [a]
    Nothing -> return []

characterSet = do
  prefix "["
  negate <- (optional $ prefix "^") >>= isJust
  atoms <- zeroOrMore (atom >>| interval)
  prefix "]"
  return $ if negate then neg else id $ foldr (<|>) noParse atoms

suffix s = do
  a <- patternElement
  prefix s
  return a

patternElement
  =   group pattern
  >>| suffix "*" (zeroOrMore character)
  >>| suffix "+" (character >>= zeroOrMore character)
  >>| suffix "?" (optional character)
  >>| alternative
  >>| characterSet
  >>| element "." character
  >>| atom
  >>| element "]" (prefix "]")

pattern ignoreCase = do
  anchorStart <- isJust <$> optional $ prefix "^"
  elements <- zeroOrMore patternElement
  anchorEnd <- isJust <$> optional $ prefix "$"
  return $ Pattern ignoreCase anchorStart anchorEnd
    $ foldr (>>) void elements

regex = do
  p <- stringLiteral' '/'
  ignoreCase <- isJust <$> optional $ terminal 'i'
  case runParser (pattern >>= eos) (RegexState p) of
    Right result -> return result
    Left error -> throwError error

booleanOperation
  =   binary boolean "&&" (&&)
  >>| binary boolean "||" (||)
  >>| unary boolean "!" not

hasChild visitor key = do
  v <- visitor
  k <- key
  update contextDependencies $ T.union $ T.make (k : getVisitorPath v) ()

  return $ not $ null $ T.sub k $ getVisitorTrie v

queryVisitor visitor = do
  v <- visitor
  update contextDependencies (T.union $ T.make $ getVisitorPath v)
  return v

queryValue visitor = (T.value . getVisitorTrie) <$> queryVisitor visitor

queryMaybeType accessor visitor =
  (maybe False (const True)) <$> queryMaybeField accessor visitor

queryMaybeField accessor visitor = (accessor =<<) <$> queryValue visitor

queryRequiredField accessor visitor =
  queryMaybeField accessor >>= maybeToEither (Error "field not found")

queryField accessor visitor = fmap accessor <$> queryValue visitor

booleanCall
  =   callM1 snapshot "hasChild" string hasChild

  >>| callM1 snapshot "hasChildren" (optional stringArray)
      (\visitor -> \case
          Just keys -> mapM_ (hasChild visitor) keys
          Nothing -> do
            v <- visitor
            mapM_ (hasChild visitor) $ T.keys $ getVisitorTrie v)

  >>| callM0 snapshot "exists"
      (\visitor -> (maybe False (const True)) <$> queryValue visitor)

  >>| callM0 snapshot "isNumber" (queryMaybeType valueNumber)
  >>| callM0 snapshot "isString" (queryMaybeType valueString)
  >>| callM0 snapshot "isBoolean" (queryMaybeType valueBoolean)
  >>| call1 string "contains" string isInfixOf
  >>| call1 string "beginsWith" string isPrefixOf
  >>| call1 string "endsWith" string isSuffixOf
  >>| call1 string "matches" regex matches

comparison
  =   binary number "===" (==)
  >>| binary number "!==" (/=)
  >>| binary number "<" (<)
  >>| binary number ">" (>)
  >>| binary number "<=" (<=)
  >>| binary number ">=" (>=)

  >>| binary string "===" (==)
  >>| binary string "!==" (/=)
  >>| binary string "<" (<)
  >>| binary string ">" (>)
  >>| binary string "<=" (<=)
  >>| binary string ">=" (>=)

  >>| binary boolean "===" (==)
  >>| binary boolean "!==" (/=)
  >>| binary boolean "<" (<)
  >>| binary boolean ">" (>)
  >>| binary boolean "<=" (<=)
  >>| binary boolean ">=" (>=)

booleanTernary = ternary boolean

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
  get fieldString >>= numberLiteralPrefix <$> skipSpace >>= \case
    Just (n, s') -> do
      set fieldString s'
      return $ return n
    Nothing -> noParse

numberReference = terminal "now" >> return $ get contextNow

numberOperation
  =   binary number "+" (+)
  >>| binary number "-" (-)
  >>| binary number "*" (*)
  >>| binary number "/" (/)
  >>| binary number "%" (%)
  >>| unary number "-" (0.0-)

numberCall
  =   callM0 snapshot "val" (queryRequiredField valueNumber)
  >>| call0 snapshot "getPriority" (queryField valuePriority)

field object field expression = do
  a <- object
  terminal field
  return $ expression a

numberField = field string "length" length

numberTernary = ternary number

stringLiteralBody delimiter =
  character >>= unescaped where
    unescaped c
      | c == delimiter = return []
      | c == '\\' = character >>= escaped
      | otherwise = character >>= (c:) <$> unescaped

    escaped c
      | c == delimiter = character >>= (c:) <$> unescaped
      | otherwise = character >>= ('\\':c:) <$> unescaped

stringLiteral' delimiter = do
  terminal [delimiter]
  s <- stringLiteralBody delimiter
  return s

stringLiteral = return <$> stringLiteral' '"'

stringReference =
  get fieldEnv >>= foldr fold noParse where
    sr name = do
      env <- get contextEnv
      n <- name
      maybeToEither (Error "bad reference") (T.find n env)

    fold key alternative =
      sr <$> terminal key
      >>| alternative

stringOperation = binary string "+" (++)

stringCall
  =   call2 string "replace" string string (\x y z -> replace y z x)
  >>| call0 string "toLowerCase" toLower
  >>| call0 string "toUpperCase" toUpper

stringField = do
  a <- field auth "uid" Uid
  set fieldUsingUid True
  return a

stringTernary = ternary string

auth = terminal "auth" >> return (return ())

snapshotReference
  =   terminal "root" >> return (get contextRootVisitor)
  >>| terminal "data" >> return (get contextVisitor)
  >>| terminal "newData" >> return (get contextVisitor)

snapshotCall
  =   callM1 snapshot "child" string
      (\visitor key -> do
          v <- visitor
          k <- key
          return $ Visitor v (key : getVisitorPath v)
            (T.sub key $ getVisitorTrie v))

  >>| callM0 snapshot "parent"
      (maybeToEither (Error "visitor has no parent")
       . getVisitorParent . (=<<))

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
  return if usingUid then UsingUid expression else NotUsingUid expression

evalACL lens (UsingUid expression) context acl =
  foldr fold acl $ acListsBlackList $ L.get lens acl where
    fold uid acl
      | evalRule expression (L.set contextMe uid) = whiteList lens uid acl
      | otherwise = acl

evalACL lens (NotUsingUid expression) context acl
  | evalRule expression context = whiteListAll lens acl
  | otherwise = acl

evalRule expression context =
  case runErrorT $ expression context of
    Right v -> v
    Left _ -> False

hasValueOfType trie type' =
  fromMaybe false
  (valueType <$> T.value trie >>= \case
      Number -> return true
      _ -> return false)

test parser p = parser >>= \x -> if p x then return x else noParse

tryMatch =
  get matchStateMatchers >>= \case
    [] -> return ()
    m:ms -> do
      set matchStateMatchers ms
      m

match anchorStart anchorEnd =
  again where
    again
      =   tryMatch >> if anchorEnd then eos else return ()
      >>| const if anchorStart then
                  noParse else
                  update position (drop 1) >> again

matches string (Pattern ignoreCase anchorStart anchorEnd elements) =
  case runParser (match anchorStart anchorEnd)
       (MatchState string elements ignoreCase) of
    Right _ -> True
    Left _ -> False

parseRule evaluate env = \case
  J.String s -> do
    evaluate <$>
      runParser (boolean >>= eos >>= annotate) (ParseState env xs False)
  _ -> Left "unexpected type in rule"

parseACLRule lens env value =
  parseRule (evalACL lens) env value

parseBooleanRule lens env value =
  parseRule (evalBoolean lens) env value

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
