{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Database.Concelo.Rules
  ( identity
  , parse
  , Context()
  , contextNow
  , contextEnv
  , contextRoot
  , contextVisitor
  , contextDependencies
  , context
  , rootVisitor
  , visitorParent
  , visitorPath
  , visitorTrie
  , visitorChild
  , Rules()
  , subRules
  , getRulesRead
  , getRulesWrite
  , getRulesValidate
  , rulesRead
  , rulesWrite
  , rulesValidate
  , rulesWildCard
  , rulesMap ) where

import Prelude hiding (foldr, null)
import Database.Concelo.Control (noParse, maybeToAction, stringLiteral,
                                 update, skipSpace, terminal, zeroOrOne,
                                 (>>|), void, group, get, set, run, eval,
                                 endOfInput, eitherToAction)
import Database.Concelo.Misc (null)
import Control.Monad (liftM2, liftM3, foldM)
import Data.Fixed (mod')
import Data.Functor ((<$>))
import Data.Foldable (foldr)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Search as BSS
import qualified Database.Concelo.Trie as T
import qualified Database.Concelo.Map as M
import qualified Database.Concelo.Set as S
import qualified Database.Concelo.Path as Pa
import qualified Database.Concelo.Protocol as Pr
import qualified Database.Concelo.ACL as ACL
import qualified Database.Concelo.Control as Co
import qualified Database.Concelo.Regex as R
import qualified Control.Lens as L
import qualified Data.Char as Ch

type Dependencies = T.Trie BS.ByteString ()

data Context =
  Context { getContextNow :: Integer
          , getContextMe :: BS.ByteString
          , getContextEnv :: M.Map BS.ByteString BS.ByteString
          , getContextRoot :: Visitor
          , getContextVisitor :: Visitor
          , getContextDependencies :: Dependencies }

contextNow =
  L.lens getContextNow (\x v -> x { getContextNow = v })

contextMe :: L.Lens' Context BS.ByteString
contextMe =
  L.lens getContextMe (\x v -> x { getContextMe = v })

contextEnv =
  L.lens getContextEnv (\x v -> x { getContextEnv = v })

contextRoot =
  L.lens getContextRoot (\x v -> x { getContextRoot = v })

contextVisitor =
  L.lens getContextVisitor (\x v -> x { getContextVisitor = v })

contextDependencies =
  L.lens getContextDependencies (\x v -> x { getContextDependencies = v })

context now env root visitor = Context now BS.empty env root visitor T.empty

data RuleState = RuleState { getRuleStateString :: BS.ByteString
                           , getRuleStateEnv :: S.Set BS.ByteString
                           , getRuleStateUsingUid :: Bool }

ruleStateString :: L.Lens' RuleState BS.ByteString
ruleStateString =
  L.lens getRuleStateString (\x v -> x { getRuleStateString = v })

ruleStateEnv =
  L.lens getRuleStateEnv (\x v -> x { getRuleStateEnv = v })

ruleStateUsingUid =
  L.lens getRuleStateUsingUid (\x v -> x { getRuleStateUsingUid = v })

instance Co.ParseState RuleState where
  parseString = ruleStateString

-- todo: enforce operator precedence and associativity

ternary :: Co.Action RuleState (Co.Action Context a) ->
           Co.Action RuleState (Co.Action Context a)
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

call2 object method argument0 argument1 function =
  callM2 object method argument0 argument1 (liftM3 function)

callM2 object method argument0 argument1 action = do
  a <- object
  terminal "."
  terminal method
  b <- argument0
  c <- argument1
  return $ action a b c

call1 object method argument function =
  callM1 object method argument (liftM2 function)

callM1 object method argument action = do
  a <- object
  terminal "."
  terminal method
  b <- argument
  return $ action a b

call0 object method function = do
  callM0 object method (fmap function)

callM0 object method action = do
  a <- object
  terminal "."
  terminal method
  group void
  return $ action a

intersperse delimiter parser =
  zeroOrOne parser >>= \case
    Just a ->
      delimiter >> ((a:) <$> intersperse delimiter parser)
      >>| return [a]
    Nothing -> return []

stringArray = do
  terminal "["
  a <- intersperse (terminal ",") string
  terminal "]"
  return a

booleanLiteral
  =   (terminal "true" >> return (return True))
  >>| (terminal "false" >> return (return False))

booleanOperation
  =   binary boolean "&&" (&&)
  >>| binary boolean "||" (||)
  >>| unary boolean "!" not

data Visitor = Visitor { getVisitorParent :: Maybe Visitor
                       , getVisitorPath :: [BS.ByteString]
                       , getVisitorTrie :: T.Trie BS.ByteString Pr.Value }

visitorParent =
  L.lens getVisitorParent (\x v -> x { getVisitorParent = v })

visitorPath =
  L.lens getVisitorPath (\x v -> x { getVisitorPath = v })

visitorTrie =
  L.lens getVisitorTrie (\x v -> x { getVisitorTrie = v })

rootVisitor trie = Visitor Nothing [] trie

visitorChild key v@(Visitor _ path trie) =
  Visitor (Just v) (key : path) (T.sub key trie)

hasChild visitor key = do
  v <- visitor
  k <- key

  update contextDependencies $ T.union $ Pa.toPath (k : getVisitorPath v) ()

  return $ not $ null $ T.sub k $ getVisitorTrie v

queryVisitor visitor = do
  v <- visitor
  update contextDependencies (T.union $ Pa.toPath (getVisitorPath v) ())
  return v

queryValue visitor = (T.value . getVisitorTrie) <$> queryVisitor visitor

queryMaybeType accessor visitor =
  (maybe False (const True)) <$> queryMaybeField accessor visitor

queryMaybeField accessor visitor = (accessor =<<) <$> queryValue visitor

queryRequiredField accessor visitor =
  queryMaybeField accessor visitor
  >>= maybeToAction (Co.Exception "field not found")

queryField accessor visitor =
  ((fmap accessor) <$> queryValue visitor)
  >>= maybeToAction (Co.Exception "field not found")

booleanCall
  =   callM1 snapshot "hasChild" string hasChild

  >>| callM1 snapshot "hasChildren" (zeroOrOne stringArray)
      (\visitor -> let visit r k = (&& r) <$> hasChild visitor k in \case
          Just keys -> foldM visit True keys
          Nothing -> do
            v <- visitor
            foldM visit True (return <$> T.keys (getVisitorTrie v)))

  >>| callM0 snapshot "exists"
      (\visitor -> (maybe False (const True)) <$> queryValue visitor)

  >>| callM0 snapshot "isNumber" (queryMaybeType Pr.valueNumber)
  >>| callM0 snapshot "isString" (queryMaybeType Pr.valueString)
  >>| callM0 snapshot "isBoolean" (queryMaybeType Pr.valueBoolean)
  >>| call1 string "contains" string BS.isInfixOf
  >>| call1 string "beginsWith" string BS.isPrefixOf
  >>| call1 string "endsWith" string BS.isSuffixOf
  >>| call1 string "matches" (return <$> R.regex) R.matches

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

numberLiteralPrefix s = case BS.uncons s of
  Nothing -> Nothing
  Just (c, cs)
    | Ch.isDigit c -> digitsOrDot [c] cs
    | otherwise -> Nothing
  where
    digitsOrDot acc s = case BS.uncons s of
      Nothing -> Just (reverse acc, BS.empty)
      Just (c, cs)
        | Ch.isDigit c -> digitsOrDot (c : acc) cs
        | c == '.' -> digits (c : acc) cs
        | otherwise -> Just (reverse acc, s)

    digits acc s = case BS.uncons s of
      Nothing -> Just (reverse acc, BS.empty)
      Just (c, cs)
        | Ch.isDigit c -> digits (c : acc) cs
        | otherwise -> Just (reverse acc, s)

numberLiteral = do
  update Co.parseString skipSpace
  (numberLiteralPrefix <$> get Co.parseString) >>= \case
    Just (n, s') -> do
      set Co.parseString s'
      return $ return $ read n
    Nothing -> noParse

numberReference = terminal "now" >> return (fromIntegral <$> get contextNow)

numberOperation
  =   binary number "+" (+)
  >>| binary number "-" (-)
  >>| binary number "*" (*)
  >>| binary number "/" (/)
  >>| binary number "%" mod'
  >>| unary number "-" (0.0-)

numberCall
  =   callM0 snapshot "val" (queryRequiredField Pr.valueNumber)
  >>| callM0 snapshot "getPriority"
      (queryField (fromIntegral . Pr.getValuePriority))

field object field expression = do
  a <- object
  terminal field
  return $ expression a

numberField = field string "length" (fmap (fromIntegral . BS.length))

numberTernary = ternary number

stringReference =
  get ruleStateEnv >>= foldr visit noParse where
    sr name = do
      env <- get contextEnv
      maybeToAction (Co.Exception "bad reference") (M.lookup name env)

    visit key alternative = (sr <$> terminal key) >>| alternative

stringOperation = binary string "+" BS.append

stringCall
  =   call2 string "replace" string string
      (\x y z -> BS.concat $ BSL.toChunks $ BSS.replace y z x)
  >>| call0 string "toLowerCase" (BS.map Ch.toLower)
  >>| call0 string "toUpperCase" (BS.map Ch.toUpper)

stringField = do
  a <- field auth "uid" (>> get contextMe)
  set ruleStateUsingUid True
  return a

stringTernary = ternary string

auth = terminal "auth" >> return (return ())

snapshotReference
  =   (terminal "root" >> return (get contextRoot))
  >>| (terminal "data" >> return (get contextVisitor))
  >>| (terminal "newData" >> return (get contextVisitor))

snapshotCall
  =   callM1 snapshot "child" string
      (\visitor key -> do
          v <- visitor
          k <- key
          return $ visitorChild k v)

  >>| callM0 snapshot "parent"
      (>>= maybeToAction (Co.Exception "visitor has no parent")
       . getVisitorParent)

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

data Annotation a = UsingUid a | NotUsingUid a

getAnnotationExpression = \case
  UsingUid e -> e
  NotUsingUid e -> e

annotate expression = do
  usingUid <- get ruleStateUsingUid
  return $ if usingUid then UsingUid expression else NotUsingUid expression

evalACL :: L.Lens' ACL.ACL ACL.Lists ->
           Annotation (Co.Action Context Bool) ->
           Context ->
           ACL.ACL ->
           (ACL.ACL, Dependencies)
evalACL lens (UsingUid expression) context acl =
  foldr visit (acl, T.empty) $ ACL.getListsBlackList $ L.view lens acl where
    visit :: BS.ByteString -> (ACL.ACL, Dependencies) -> (ACL.ACL, Dependencies)
    visit uid (acl, deps) =
      let (valid, newDeps) =
            evalBoolean expression $ L.set contextMe uid context in
      (if valid then ACL.whiteList lens uid acl else acl,
       T.union newDeps deps)

evalACL lens (NotUsingUid expression) context acl =
  case evalBoolean expression context of
    (True, deps) -> (ACL.whiteListAll lens acl, deps)
    (False, deps) -> (acl, deps)

evalBoolean expression context =
  case run expression context of
    Right (valid, context') -> (valid, getContextDependencies context')
    Left _ -> (False, T.empty)

parseRule evaluate env s =
  evaluate <$>
  eval (boolean >>= endOfInput >>= annotate) (RuleState s env False)

parseACLRule :: L.Lens' ACL.ACL ACL.Lists ->
                S.Set BS.ByteString ->
                BS.ByteString ->
                Either Co.Exception (Context ->
                                     ACL.ACL ->
                                     (ACL.ACL, Dependencies))
parseACLRule lens env value =
  parseRule (evalACL lens) env value

parseBooleanRule env value =
  parseRule (evalBoolean . getAnnotationExpression) env value

parseIndexOn = const $ Right [] -- todo

data Rules =
  Rules { getRulesRead :: Context -> ACL.ACL -> (ACL.ACL, Dependencies)
        , getRulesWrite :: Context -> ACL.ACL -> (ACL.ACL, Dependencies)
        , getRulesValidate :: Context -> (Bool, Dependencies)
        , getRulesIndexOn :: [BS.ByteString]
        , getRulesWildCard :: Maybe (Rules, BS.ByteString)
        , getRulesMap :: M.Map BS.ByteString Rules }

rulesRead =
  L.lens getRulesRead (\x v -> x { getRulesRead = v })

rulesWrite =
  L.lens getRulesWrite (\x v -> x { getRulesWrite = v })

rulesValidate =
  L.lens getRulesValidate (\x v -> x { getRulesValidate = v })

rulesIndexOn =
  L.lens getRulesIndexOn (\x v -> x { getRulesIndexOn = v })

rulesWildCard =
  L.lens getRulesWildCard (\x v -> x { getRulesWildCard = v })

rulesMap =
  L.lens getRulesMap (\x v -> x { getRulesMap = v })

idACLRule = const $ \a -> (a, T.empty)

trueRule = const (True, T.empty)

identity = Rules idACLRule idACLRule trueRule [] Nothing M.empty

subRules key rules = case M.lookup key $ getRulesMap rules of
  Nothing -> case getRulesWildCard rules of
    Nothing -> (identity, BS.empty)
    Just r -> r
  Just r -> (r, BS.empty)

parseTrie env =
  foldM visit identity . T.pairs where
    visit rules (k, sub) =
      let maybeParseRule = flip (maybe $ Right rules) (T.value sub) in
      case k of
        ".read" ->
          maybeParseRule (\v -> flip (L.set rulesRead) rules
                                <$> parseACLRule ACL.aclReadLists env v)

        ".write" ->
          maybeParseRule (\v -> flip (L.set rulesWrite) rules
                                <$> parseACLRule ACL.aclWriteLists env v)

        ".validate" ->
          maybeParseRule (\v -> flip (L.set rulesValidate) rules
                                <$> parseBooleanRule env v)

        ".indexOn" ->
          maybeParseRule (\v -> flip (L.set rulesIndexOn) rules
                                <$> parseIndexOn v)

        _ -> case BS.uncons k of
          Just ('$', _) ->
            flip (L.set rulesWildCard) rules . Just . (, k)
            <$> parseTrie (S.insert k env) sub

          _ -> parseTrie env sub >>= \r ->
            return $ L.over rulesMap (M.insert k r) rules

parse = eitherToAction . parseTrie S.empty
