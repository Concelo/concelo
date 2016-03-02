module Database.Concelo.Regex
  ( regex
  , matches ) where

import Database.Concelo.Control (noParse, ParseState(parseString), endOfStream,
                                 prefix, character, (>>|), Action(),
                                 stringLiteralDelimited, zeroOrMore, eval)

import qualified Data.ByteString as BS

data Pattern = Pattern { getPatternIgnoreCase :: Bool
                       , getPatternAnchorStart :: Bool
                       , getPatternAnchorEnd :: Bool
                       , getPatternParser :: Action MatchState () }

newtype PatternState = PatternState { getPatternStateString :: BS.ByteString }

patternStateString =
  L.lens getPatternStateString (\x v -> x { getPatternStateString = v })

instance ParseState PatternState where
  parseString = patternStateString

data MatchState = MatchState { getMatchStateString :: BS.ByteString
                             , getMatchStateIgnoreCase :: Bool }

matchStateString =
  L.lens getMatchStateString (\x v -> x { getMatchStateString = v })

matchStateIgnoreCase =
  L.lens getMatchStateIgnoreCase (\x v -> x { getMatchStateIgnoreCase = v })

instance ParseState MatchState where
  parseString = matchStateString

element code expression =
  expression <$> prefix code

alternative = do
  a <- patternElement
  prefix "|"
  b <- patternElement
  return (a >>| b)

escaped = prefix "\\" >> ((prefix . (:[])) <$> character)

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
  result <- (parser >> return True) >>| return False
  if result then noParse else return ()

unescaped =
  character >>= \case
    ']' -> noParse
    '$' -> noParse
    c -> return $ prefix [c]

interval = do
  a <- unescaped
  prefix "-"
  b <- unescaped
  return (test character \c -> ord c >= ord a && ord c <= ord b)

characterSet = do
  prefix "["
  negate <- (optional $ prefix "^") >>= isJust
  atoms <- zeroOrMore (atom >>| interval)
  prefix "]"
  return $ if negate then neg else id $ foldr (>>|) noParse atoms

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
  anchorStart <- isJust <$> optional (prefix "^")
  elements <- zeroOrMore patternElement
  anchorEnd <- isJust <$> optional (prefix "$")
  return $ Pattern ignoreCase anchorStart anchorEnd
    $ foldr (>>) void elements

regex = do
  p <- stringLiteralDelimited '/'
  ignoreCase <- isJust <$> optional (prefix 'i')
  case run (pattern ignoreCase >>= endOfStream) (PatternState p) of
    Right result -> return result
    Left error -> throwError error

test parser p = parser >>= \x -> if p x then return x else noParse

match parser anchorStart anchorEnd =
  again where
    next = if anchorStart then
             noParse
           else
             update parseString (drop 1) >> again

    again
      =   parser >> if anchorEnd then endOfInput else next
      >>| next

matches string (Pattern ignoreCase anchorStart anchorEnd parser) =
  case eval (match parser anchorStart anchorEnd)
       (MatchState string ignoreCase) of
    Right _ -> True
    Left _ -> False
