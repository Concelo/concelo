{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Database.Concelo.Bytes
  ( Database.Concelo.Bytes.toInteger
  , Database.Concelo.Bytes.fromInteger
  , toWord64
  , fromWord64
  , toWord32
  , fromWord32
  , prefix ) where

import Database.Concelo.Prelude

import Data.Bits ((.&.))

import qualified Database.Concelo.Control as Co
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Bits as B
import qualified Data.Char as Ch
import qualified Data.Word as W

toInteger = toInteger'

toInteger' = do
  (Ch.ord <$> Co.character) >>= \case
    0 -> (Ch.ord <$> Co.character) >>= \case
      0 -> return 0
      _ -> (`B.shiftL` 8) <$> toInteger'
    n -> ((+ fromIntegral n) . (`B.shiftL` 8)) <$> toInteger'

fromInteger n = BS.pack $ writeInteger n []

writeInteger = \case
  0 -> (toEnum 0 :) . (toEnum 0 :)
  n -> (case n .&. 255 of
           0 -> (toEnum 0 :) . (toEnum 1 :)
           b -> (toEnum (fromIntegral b) :))
       . writeInteger (n `B.shiftR` 8)

toWord64 :: BS.ByteString -> W.Word64
toWord64 = BS.foldl' (\x y -> x * 256 + fromIntegral y) 0

fromWord64 :: W.Word64 -> BS.ByteString
fromWord64 w =
  BS.pack $ fmap (fromIntegral . (.&. 255) . B.shiftR w) [56, 48..0]

toWord32 :: BS.ByteString -> W.Word32
toWord32 = BS.foldl' (\x y -> x * 256 + fromIntegral y) 0

fromWord32 :: W.Word32 -> BS.ByteString
fromWord32 w =
  BS.pack $ fmap (fromIntegral . (.&. 255) . B.shiftR w) [24, 16..0]

prefix bs = if BS.length bs > 1 then
              show $ B16.encode $ BS.take 4 bs
            else
              show bs
