{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Database.Concelo.Control
  ( get
  , set
  , setThenGet
  , update
  , updateM
  , overM
  , updateThenGet
  , getThenUpdate
  , getThenSet
  , with
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
  , character
  , (>>|)
  , zeroOrMore
  , endOfStream
  , stringLiteral
  , stringLiteralDelimited
  , mapPair
  , maybeM
  , maybeM2
  , eitherToMaybe
  , maybeToAction
  , bindMaybe
  , bindMaybe2 ) where

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

exception :: E.MonadError Exception m => String -> m a
exception = E.throwError . Exception

patternFailure :: E.MonadError Exception m => m a
patternFailure = E.throwError PatternFailure

badForest :: E.MonadError Exception m => m a
badForest = E.throwError BadForest

missingChunks :: E.MonadError Exception m => m a
missingChunks = E.throwError MissingChunks

noParse :: E.MonadError Exception m => m a
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

overM :: S.MonadState s m => L.Lens' s a -> (a -> m a) -> s -> m s
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
    Left error -> E.throwError error
    Right result -> return result

with :: L.Lens' s a -> Action a b -> Action s b
with lens action =
  try (run action <$> get lens) >>= \(result, state) -> do
    set lens state
    return result

mapPair f (x, y) = (f x, f y)

bindMaybe f x =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> f x'

bindMaybe2 f x y =
  x >>= \case
    Nothing -> return Nothing
    Just x' -> y >>= \case
      Nothing -> return Nothing
      Just y' -> f x' y'

maybeM f x = case f x of
  Nothing -> patternFailure
  Just v -> return v

maybeM2 f x y = case f x y of
  Nothing -> patternFailure
  Just v -> return v

eitherToMaybe = \case
  Left _ -> Nothing
  Right v -> Just v

maybeToAction error = \case
  Nothing -> E.throwError error
  Just v -> return v

class ParseState s where
  parseString :: L.Lens' s a

character :: (ParseState s, S.MonadState s m, E.MonadError Exception m) =>
             m Char
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

prefix t =
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
    Left error -> E.throwError error

zeroOrMore parser =
  optional parser >>= \case
    Just a -> ((a:) <$> zeroOrMore parser) >>| return [a]
    Nothing -> return []

optional parser = (Just <$> parser) >>| return Nothing

endOfStream expression = do
  s <- get parseString
  if BS.null s then return expression else noParse

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
  _ <- terminal $ BS.singleton delimiter
  s <- stringLiteralBody delimiter
  return $ BS.pack s

stringLiteral :: (ParseState s,
                  S.MonadState s m,
                  E.MonadError Exception m,
                  Monad n) =>
                 m (n BS.ByteString)
stringLiteral = return <$> stringLiteralDelimited '"'
