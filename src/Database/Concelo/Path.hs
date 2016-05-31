{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
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

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.List as L

data Path k v = Path { getPathKeys :: [k]
                     , getPathValue :: v } deriving (Eq)

instance {-# OVERLAPPABLE #-} (Show v, Show k) => Show (Path k v) where
  show (Path ks v) = (L.intercalate "/" $ map show ks) ++ ":" ++ show v

instance {-# OVERLAPPING #-} Show v => Show (Path BS.ByteString v) where
  show (Path ks v) = (L.intercalate "/" $ map visit ks) ++ ":" ++ show v where
    visit bs = if BS.length bs > 1 then
                 show $ B16.encode $ BS.take 4 bs
               else
                 show bs

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
