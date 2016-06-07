{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
module Database.Concelo.Path
  ( Path(Path)
  , leaf
  , singleton
  , toPath
  , keys
  , value
  , valueHere
  , sub
  , super
  , append
  , Database.Concelo.Path.init
  , initLast ) where

import Prelude hiding (foldr)
import Data.Foldable (Foldable(foldr))

import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Database.Concelo.Bytes as B

data Path k v = Path { getPathKeys :: [k]
                     , getPathValue :: v } deriving (Eq)

instance (Show v, Show k) => Show (Path k v) where
  show (Path ks v) = (L.intercalate "/" $ map show ks) ++ ":" ++ show v

instance
#if __GLASGOW_HASKELL__ >= 710
    {-# OVERLAPPING #-}
#endif
  Show v => Show (Path BS.ByteString v) where
  show (Path ks v) = (L.intercalate "/" $ map B.prefix ks) ++ ":" ++ show v

instance Functor (Path k) where
  fmap f (Path ks v) = Path ks (f v)

instance Foldable (Path k) where
  foldr visit seed (Path _ v) = visit v seed

leaf v = Path [] v

singleton k v = Path [k] v

keys = getPathKeys

value = getPathValue

valueHere = \case
  Path [] v -> Just v
  _ -> Nothing

toPath = Path

sub = \case
  Path (k:ks) v -> Just (k, Path ks v)
  _ -> Nothing

super k (Path ks v) = Path (k:ks) v

append (Path ks v) k = Path (ks ++ [k]) v

initLast' = \case
  [] -> error "empty list"
  [k] -> ([], k)
  k:ks -> let (init, last) = initLast' ks in (k:init, last)

initLast (Path ks v) = (Path init v, last) where
  (init, last) = initLast' ks

init = fst . initLast
