module Database.Concelo.Control
  ( get
  , set
  , update
  , updateThenGet
  , getThenUpdate
  , getThenSet
  , with
  , Exception (Error, PatternFailure, BadForest, MissingChunks, NoParse)
  , error
  , patternFailure
  , badForest
  , missingChunks
  , noParse
  , ParseState(parseString)
  , character
  , (>>|)
  , zeroOrMore
  , endOfStream
  , mapPair
  , maybeM
  , maybeM2
  , eitherToMaybe
  , bindMaybe
  , bindMaybe2 ) where

import Data.ByteString (ByteString)

import qualified Control.Lens as L
import qualified Control.Monad.State.Class as S

data Exception = Error ByteString
               | PatternFailure
               | BadForest
               | MissingChunks
               | NoParse

error s = throwError $ Error s

patternFailure = throwError PatternFailure

badForest = throwError BadForest

missingChunks = throwError MissingChunks

noParse = throwError NoParse

run = runIdentity . runErrorT . runStateT

exec = runIdenity . runErrorT . execStateT

eval = runIdenity . runErrorT . evalStateT

get lens = L.view lens <$> S.get

set lens value = S.state $ \s -> ((), L.set lens value s)

update lens update = S.state $ \s -> ((), L.over lens update s)

updateThenGet lens update =
  S.state $ \s -> let v = update $ L.get lens s in (v, L.set lens v s)

getThenUpdate lens update =
  S.state $ \s -> (L.get lens s, L.over lens update s)

getThenSet lens value = S.state $ \s -> (L.get lens s, L.set lens value s)

with lens action =
  run action <$> get lens >>= \case
    Left error -> throwError error
    Right (result, state) -> do
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

class ParseState s where
  parseString :: Lens s t a b

character = do
  s <- get parseString
  case uncons s of
    Nothing -> noParse
    Just (c, cs) -> do
      set parseString cs
      return c

subtractPrefix p:ps c:cs
  | p == c = subtractPrefix ps cs
  | otherwise = Nothing
subtractPrefix [] cs = Just cs
subtractPrefix _ _ = Nothing

prefix t =
  subtractPrefix t <$> get parseString >>= \case
    Just s' -> do
      set fieldString s'
      return t
    Nothing -> noParse

a >>| b = do
  case run a of
    Right (x, s) -> S.put s >> return x
    Left NoParse -> b
    Left error -> throwError error

zeroOrMore parser =
  optional parser >>= \case
    Just a -> (a:) <$> parser >>| [a]
    Nothing -> return []

optional parser
  =   Just <$> parser
  >>| return Nothing

endOfStream expression = do
  s <- get parseString
  if null s then return expression else noParse
