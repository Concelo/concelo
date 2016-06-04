{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Bytes
  ( Database.Concelo.Bytes.toInteger
  , Database.Concelo.Bytes.fromInteger
  , toWord64
  , fromWord64 ) where

import Data.Bits ((.&.))

import qualified Database.Concelo.Control as Co
import qualified Data.ByteString as BS
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
  BS.pack $ map (fromIntegral . (.&. 255) . B.shiftR w) [56, 48..0]
