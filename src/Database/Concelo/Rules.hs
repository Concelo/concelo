module Database.Concelo.Rules
  ( parse
  , Context(Context)
  , contextDirty
  , contextNow
  , contextEnv
  , contextRoot
  , contextVisitor
  , contextDependencies ) where

import Database.Concelo.Control (noParse, ParseState(parseString), endOfStream,
                                 prefix, maybeToEither, stringLiteral,
                                 Exception(Error))

import qualified Data.ByteString as BS
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Set as S
import qualified Database.Concelo.Path as Path
import qualified Database.Concelo.Protocol as P
import qualified Database.Concelo.ACL as ACL
import qualified Control.Lens as L

data Context a =
  Context { getContextDirty :: T.Trie BS.ByteString a
          , getContextNow :: Integer
          , getContextEnv :: M.Map BS.ByteString BS.ByteString
          , getContextRoot :: Visitor
          , getContextVisitor :: Visitor
          , getContextDependencies :: T.Trie BS.ByteString () }

contextDirty =
  L.lens getContextDirty (\x v -> x { getContextDirty = v })

contextNow =
  L.lens getContextNow (\x v -> x { getContextNow = v })

contextRoot =
  L.lens getContextRoot (\x v -> x { getContextRoot = v })

contextVisitor =
  L.lens getContextVisitor (\x v -> x { getContextVisitor = v })

contextDependencies =
  L.lens getContextDependencies (\x v -> x { getContextDependencies = v })

data RuleState = RuleState { getRuleStateString :: ByteString
                           , getRuleStateEnv :: S.Set BS.ByteString
                           , ruleStateUsingUid :: Bool }

ruleStateString =
  L.lens getRuleStateString (\x v -> x { getRuleStateString = v })

ruleStateEnv =
  L.lens getRuleStateEnv (\x v -> x { getRuleStateEnv = v })

ruleStateUsingUid =
  L.lens getRuleStateUsingUid (\x v -> x { getRuleStateUsingUid = v })

instance ParseState RuleState where
  parseString = ruleStateString

-- todo: enforce operator precedence and associativity

endOfInput expression = do
  update fieldString skipSpace
  endOfStream expression

skipSpace = BS.dropWhile isSpace

terminal t = do
  update fieldString skipSpace
  prefix t

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
  return (function <$> a)

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
      delimiter >> ((a:) <$> intersperse delimiter parser)
      >>| [a]
    Nothing -> return []

stringArray = do
  terminal "["
  a <- intersperse (terminal ",") string
  terminal "]"
  return a

booleanOperation
  =   binary boolean "&&" (&&)
  >>| binary boolean "||" (||)
  >>| unary boolean "!" not

data Visitor = Visitor { getVisitorParent :: Maybe Visitor
                       , getVisitorPath :: [BS.ByteString]
                       , getVisitorTrie :: T.Trie ByteString P.Value ]

hasChild visitor key = do
  v <- visitor
  k <- key

  update contextDependencies $ T.union $ Path.toPath (k : getVisitorPath v) ()

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
  queryMaybeField accessor >>= maybeToAction (Error "field not found")

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

  >>| callM0 snapshot "isNumber" (queryMaybeType P.valueNumber)
  >>| callM0 snapshot "isString" (queryMaybeType P.valueString)
  >>| callM0 snapshot "isBoolean" (queryMaybeType P.valueBoolean)
  >>| call1 string "contains" string isInfixOf
  >>| call1 string "beginsWith" string isPrefixOf
  >>| call1 string "endsWith" string isSuffixOf
  >>| call1 string "matches" Regex.regex Regex.matches

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

numberLiteralPrefix s = case BS.uncons s of
  Nothing -> Nothing
  Just (c, cs)
    | isDigit c -> digitsOrDot [c] cs where
      digitsOrDot acc s = case BS.uncons s of
        Nothing -> Just (reverse acc, [])
        Just (c, cs)
          | isDigit c -> digitsOrDot (c : acc) cs
          | c == '.' -> digits (c : acc) cs
          | otherwise -> Just (reverse acc, s)

      digits acc s = case BS.uncons s of
        Nothing -> Just (reverse acc, [])
        Just (c, cs)
          | isDigit c -> digits (c : acc) cs
          | otherwise -> Just (reverse acc, s)

    | otherwise -> Nothing

numberLiteral =
  get parseString >>= numberLiteralPrefix <$> skipSpace >>= \case
    Just (n, s') -> do
      set parseString s'
      return $ return $ read n :: Double
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
  =   callM0 snapshot "val" (queryRequiredField P.valueNumber)
  >>| call0 snapshot "getPriority" (queryField P.valuePriority)

field object field expression = do
  a <- object
  terminal field
  return $ expression a

numberField = field string "length" length

numberTernary = ternary number

stringReference =
  get ruleStateEnv >>= foldr visit noParse where
    sr name = do
      env <- get contextEnv
      n <- name
      maybeToAction (Error "bad reference") (M.findValue n env)

    visit key alternative =
      sr <$> terminal key
      >>| alternative

stringOperation = binary string "+" (++)

stringCall
  =   call2 string "replace" string string (\x y z -> BS.replace y z x)
  >>| call0 string "toLowerCase" BS.toLower
  >>| call0 string "toUpperCase" BS.toUpper

stringField = do
  a <- field auth "uid" Uid
  set ruleStateUsingUid True
  return a

stringTernary = ternary string

auth = terminal "auth" >> return (return ())

snapshotReference
  =   terminal "root" >> return (get contextRoot)
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
  usingUid <- get ruleStateUsingUid
  return if usingUid then UsingUid expression else NotUsingUid expression

evalACL lens (UsingUid expression) context acl =
  foldr visit acl $ ACL.listsBlackList $ L.get lens acl where
    visit uid acl
      | evalBoolean expression (L.set contextMe uid) =
        ACL.whiteList lens uid acl

      | otherwise = acl

evalACL lens (NotUsingUid expression) context acl
  | evalBoolean expression context = ACL.whiteListAll lens acl
  | otherwise = acl

evalBoolean expression context =
  case eval expression context of
    Right v -> v
    Left _ -> False

parseRule evaluate env s =
  evaluate <$>
  eval (boolean >>= endOfInput >>= annotate) (RuleState s env False)

parseACLRule lens env value =
  parseRule (evalACL lens) env value

parseBooleanRule lens env value =
  parseRule (evalBoolean lens) env value

parseIndexOn = [] -- todo

data Rule a =
  Rule { getRuleRead :: Context a -> ACL -> ACL
       , getRuleWrite :: Context a -> ACL -> ACL
       , getRuleValidate :: Context a -> Bool
       , getRuleIndexOn :: [BS.ByteString]
       , getRuleWildCard :: Maybe (Rule a)
       , getRuleMap :: M.Map BS.ByteString (Rule a) }

ruleRead =
  L.lens getRuleRead (\x v -> x { getRuleRead = v })

ruleWrite =
  L.lens getRuleWrite (\x v -> x { getRuleWrite = v })

ruleValidate =
  L.lens getRuleValidate (\x v -> x { getRuleValidate = v })

ruleIndexOn =
  L.lens getRuleIndexOn (\x v -> x { getRuleIndexOn = v })

ruleWildCard =
  L.lens getRuleWildCard (\x v -> x { getRuleWildCard = v })

ruleMap =
  L.lens getRuleMap (\x v -> x { getRuleMap = v })

emptyRule = Rule (const id) (const id) (const True) [] Nothing M.empty

parseTrie env =
  foldM visit emptyRule . T.triples where
    visit (k, v, sub) rule = case k of
      ".read" ->
        flip L.set ruleRead rule <$> parseACLRule aclReadLists env v

      ".write" ->
        flip L.set ruleWrite rule <$> parseACLRule aclWriteLists env v

      ".validate" ->
        flip L.set ruleValidate rule <$> parseBooleanRule env v

      ".indexOn" ->
        flip L.set ruleIndexOn rule <$> parseIndexOn v

      _ -> case BS.uncons k of
        Just ('$', _) ->
          flip L.set ruleWildCard rule . Just
          <$> parseTrie (S.insert k env) sub

        _ -> parseTrie env sub >>= \r ->
          return $ L.over ruleMap (M.insert k r) rule

parse = parseTrie S.empty
