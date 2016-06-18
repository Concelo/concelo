{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Regex
  ( regex
  , matches ) where

import Database.Concelo.Control (noParse, ParseState(parseString), endOfStream,
                                 prefix, character, (>>|), Action(),
                                 stringLiteralDelimited, zeroOrMore, eval,
                                 zeroOrOne, void, group, update,
                                 endOfInput, oneOrMore, get)

import Data.Maybe (isJust)

import qualified Control.Monad.Except as E
import qualified Data.ByteString.Char8 as BS
import qualified Control.Lens as L
import qualified Data.Char as C

data Pattern = Pattern { _getPatternIgnoreCase :: Bool
                       , _getPatternAnchorStart :: Bool
                       , _getPatternAnchorEnd :: Bool
                       , _getPatternParser :: Action MatchState () }

newtype PatternState = PatternState { getPatternStateString :: BS.ByteString }

patternStateString :: L.Lens' PatternState BS.ByteString
patternStateString =
  L.lens getPatternStateString $ \x v -> x { getPatternStateString = v }

instance ParseState PatternState where
  parseString = patternStateString

data MatchState = MatchState { getMatchStateString :: BS.ByteString
                             , getMatchStateIgnoreCase :: Bool }

matchStateString :: L.Lens' MatchState BS.ByteString
matchStateString =
  L.lens getMatchStateString $ \x v -> x { getMatchStateString = v }

matchStateIgnoreCase :: L.Lens' MatchState Bool
matchStateIgnoreCase =
  L.lens getMatchStateIgnoreCase $ \x v -> x { getMatchStateIgnoreCase = v }

instance ParseState MatchState where
  parseString = matchStateString

element code expression =
  prefix code >> return (expression >> void)

alternative = do
  a <- patternElement
  prefix "|"
  b <- patternElement
  return (a >>| b)

matchCharacter c = do
  ignoreCase <- get matchStateIgnoreCase
  test character (\x -> x == c || (ignoreCase && C.toLower x == C.toLower c))

escaped = prefix "\\" >> (matchCharacter <$> character)

isWordCharacter c = C.isAlphaNum c || c == '_'

atom
  =   element "\\s" (test character C.isSpace)
  >>| element "\\w" (test character isWordCharacter)
  >>| element "\\d" (test character C.isDigit)
  >>| element "\\S" (test character (not . C.isSpace))
  >>| element "\\w" (test character (not . isWordCharacter))
  >>| element "\\d" (test character (not . C.isDigit))
  >>| escaped
  >>| unescaped

neg parser = do
  result <- (parser >> return True) >>| return False
  if result then noParse else return ()

getUnescaped =
  character >>= \case
    ']' -> noParse
    '$' -> noParse
    c -> return c

unescaped = matchCharacter <$> getUnescaped

interval = do
  a <- getUnescaped
  prefix "-"
  b <- getUnescaped
  return (test character (\c -> c >= a && c <= b))

characterSet = do
  prefix "["
  negate <- isJust <$> (zeroOrOne $ prefix "^")
  atoms <- zeroOrMore (atom >>| interval)
  prefix "]"
  return $ (if negate then neg else id) (foldr (>>|) noParse atoms)

suffix s f = do
  a <- patternElement
  prefix s
  return $ f a

patternElement
  =   group sequence'
  >>| suffix "*" ((>> void) . zeroOrMore)
  >>| suffix "+" ((>> void) . oneOrMore)
  >>| suffix "?" ((>> void) . zeroOrOne)
  >>| alternative
  >>| characterSet
  >>| element "." character
  >>| atom
  >>| element "]" (prefix "]")

sequence' = foldr (>>) void <$> zeroOrMore patternElement

pattern ignoreCase = do
  anchorStart <- isJust <$> zeroOrOne (prefix "^")
  p <- sequence'
  anchorEnd <- isJust <$> zeroOrOne (prefix "$")
  return $ Pattern ignoreCase anchorStart anchorEnd p

regex = do
  p <- stringLiteralDelimited '/'
  ignoreCase <- isJust <$> zeroOrOne (prefix "i")
  case eval (pattern ignoreCase >>= endOfStream) (PatternState p) of
    Right result -> return result
    Left error -> E.throwError error

test parser p = parser >>= \x -> if p x then return () else noParse

match parser anchorStart anchorEnd =
  again where
    next = if anchorStart then
             noParse
           else
             update parseString (BS.drop 1) >> again

    again
      =   parser >> if anchorEnd then endOfInput () else next
      >>| next

matches string (Pattern ignoreCase anchorStart anchorEnd parser) =
  case eval (match parser anchorStart anchorEnd)
       (MatchState string ignoreCase) of
    Right _ -> True
    Left _ -> False
