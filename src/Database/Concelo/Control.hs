{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- todo: this file has become home to a lot of little utilities
-- unrelated to control flow; split them out
module Database.Concelo.Control
  ( Action
  , get
  , set
  , setThenGet
  , update
  , updateM
  , overM
  , updateThenGet
  , getThenUpdate
  , getThenSet
  , with
  , lend
  , run
  , exec
  , eval
  , try
  , Exception (Exception, PatternFailure, BadForest, MissingChunks, NoParse,
               Success)
  , exception
  , patternFailure
  , badForest
  , missingChunks
  , noParse
  , ParseState (parseString)
  , void
  , group
  , character
  , (>>|)
  , prefix
  , terminal
  , skipSpace
  , zeroOrOne
  , zeroOrMore
  , oneOrMore
  , endOfStream
  , endOfInput
  , stringLiteral
  , stringLiteralDelimited
  , mapPair
  , eitherToMaybe
  , maybeToAction
  , eitherToAction
  , bsShow
  , bsRead ) where

-- import Debug.Trace

import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as C
import qualified Control.Lens as L
import qualified Control.Monad.State as S
import qualified Control.Monad.Except as E
import qualified Control.Monad.Identity as I

type Action s a = S.StateT s (E.ExceptT Exception I.Identity) a

data Exception = Exception String
               | PatternFailure
               | BadForest
               | MissingChunks
               | NoParse
               | Success
               deriving (Show)

throw :: Exception ->
         Action s a
throw = error . show --E.throwError

exception = throw . Exception

patternFailure = throw PatternFailure

badForest = throw BadForest

missingChunks = E.throwError MissingChunks

noParse = E.throwError NoParse

run action = I.runIdentity . E.runExceptT . S.runStateT action

exec action = I.runIdentity . E.runExceptT . S.execStateT action

eval action = I.runIdentity . E.runExceptT . S.evalStateT action

get lens = L.view lens <$> S.get

set lens value = S.state $ \s -> ((), L.set lens value s)

setThenGet lens value = S.state $ \s -> (value, L.set lens value s)

update lens update = S.state $ \s -> ((), L.over lens update s)

updateM :: S.MonadState s m => L.Lens' s a -> (a -> m a) -> m ()
updateM lens action = get lens >>= action >>= set lens

overM :: Monad m => L.Lens' s a -> (a -> m a) -> s -> m s
overM lens action object =
  flip (L.set lens) object <$> action (L.view lens object)

updateThenGet :: S.MonadState s m => L.Lens' s a -> (a -> a) -> m a
updateThenGet lens update =
  S.state $ \s -> let v = update $ L.view lens s in (v, L.set lens v s)

getThenUpdate :: S.MonadState s m => L.Lens' s a -> (a -> a) -> m a
getThenUpdate lens update =
  S.state $ \s -> (L.view lens s, L.over lens update s)

getThenSet :: S.MonadState s m => L.Lens' s a -> a -> m a
getThenSet lens value = S.state $ \s -> (L.view lens s, L.set lens value s)

try either =
  either >>= \case
    Left error -> throw error
    Right result -> return result

with :: L.Lens' s a -> Action a b -> Action s b
with lens action =
  try (run action <$> get lens) >>= \(result, state) -> do
    set lens state
    return result

lend :: L.Lens' s c -> L.Lens' a c -> L.Lens' s a -> Action a b -> Action s b
lend src dst lens action = do
  original <- get (lens . dst)
  get src >>= set (lens . dst)
  result <- with lens action
  get (lens . dst) >>= set src
  set (lens . dst) original
  return result

mapPair f (x, y) = (f x, f y)

eitherToMaybe = \case
  Left _ -> Nothing
  Right v -> Just v

maybeToAction error = \case
  Nothing -> throw error
  Just v -> return v

eitherToAction = \case
  Left error -> E.throwError error
  Right v -> return v

class ParseState s where
  parseString :: L.Lens' s BS.ByteString

void = return ()

group parser = do
  terminal "("
  a <- parser
  terminal ")"
  return a

character = do
  s <- get parseString
  case BS.uncons s of
    Nothing -> noParse
    Just (c, cs) -> do
      set parseString cs
      return c

subtractPrefix p s =
  let (a, b) = BS.splitAt (BS.length p) s in
  if p == a then Just b else Nothing

prefix t = do
  subtractPrefix t <$> get parseString >>= \case
    Just s -> do
      set parseString s
      return t
    Nothing -> noParse

skipSpace = BS.dropWhile C.isSpace

terminal t = do
  update parseString skipSpace
  prefix t

a >>| b = do
  s <- S.get
  case run a s of
    Right (x, s') -> S.put s' >> return x
    Left NoParse -> b
    Left error -> throw error

zeroOrMore parser =
  zeroOrOne parser >>= \case
    Just a -> ((a:) <$> zeroOrMore parser) >>| return [a]
    Nothing -> return []

oneOrMore parser =
  parser >>= \a -> ((a:) <$> zeroOrMore parser) >>| return [a]

zeroOrOne parser = (Just <$> parser) >>| return Nothing

endOfStream result = do
  s <- get parseString
  if BS.null s then return result else noParse

endOfInput result = do
  update parseString skipSpace
  endOfStream result

stringLiteralBody delimiter =
  character >>= unescaped where
    unescaped c
      | c == delimiter = return []
      | c == '\\' = character >>= escaped
      | otherwise = (c:) <$> (character >>= unescaped)

    escaped c
      | c == delimiter = (c:) <$> (character >>= unescaped)
      | otherwise = ('\\':) . (c:) <$> (character >>= unescaped)

stringLiteralDelimited delimiter = do
  terminal $ BS.singleton delimiter
  s <- stringLiteralBody delimiter
  return $ BS.pack s

stringLiteral = return <$> stringLiteralDelimited '"'

bsShow :: Show s => s -> BS.ByteString
bsShow = BS.pack . show

bsRead :: Read r => BS.ByteString -> r
bsRead = read . BS.unpack
