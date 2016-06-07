{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Database.Concelo.Crypto
  ( derivePrivate
  , derivePublic
  , fromPublic
  , toPublic
  , fromPrivate
  , toPrivate
  , dummyKey
  , fromSymmetric
  , toSymmetric
  , newSymmetric
  , decryptPrivate
  , decryptAsymmetric
  , encryptAsymmetric
  , decryptSymmetric
  , encryptSymmetric
  , PublicKey()
  , PrivateKey()
  , SymmetricKey()
  , PRNG()
  , makePRNG
  , randomBytes
  , seedSize
  , sign
  , verify
  , hash ) where

import Database.Concelo.Prelude

import qualified Crypto.Error as E
import qualified Crypto.Random as R
import qualified Crypto.Cipher.Types as CT
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Hash as H
import qualified Crypto.PubKey.RSA as RSA
import qualified Crypto.PubKey.RSA.Types as RSAT
import qualified Crypto.PubKey.RSA.PKCS15 as PKCS15
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Data.Foldable as F
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import qualified Control.Lens as L
import qualified Control.Monad as M
import qualified Control.Monad.State as S
import qualified Database.Concelo.Bytes as B
import qualified Database.Concelo.Control as C

newtype PRNG = PRNG R.ChaChaDRG

newtype PrivateKey = PrivateKey { getPrivateKey :: RSAT.PrivateKey }
                     deriving Show

newtype PublicKey = PublicKey { _getPublicKey :: RSAT.PublicKey }

instance Show PublicKey where
  show = show . fromPublic

newtype SymmetricKey = SymmetricKey { fromSymmetric :: BS.ByteString }

newtype ParseState = ParseState { getParseStateString :: BS.ByteString }

parseStateString :: L.Lens' ParseState BS.ByteString
parseStateString =
    L.lens getParseStateString (\x v -> x { getParseStateString = v })

instance C.ParseState ParseState where
  parseString = parseStateString

type Cipher = AES.AES256

hashAlgorithm = H.SHA256

-- todo: does WebCrypto support Scrypt or Bcrypt?  If so, use one of them.
prf = PBKDF2.prfHMAC hashAlgorithm

iterations = 4096

-- This only determines the size of email/password derived keys; keys
-- generated from different sources may be of any size.  Ideally, we'd
-- use e.g. 1920 byte (15360 bit) keys here so as to achieve a 256-bit
-- equivalent strength, but those are prohibitively slow to generate.
-- TODO: Switch to elliptic curves and use ECDH to agree on a key and
-- use that to encrypt stuff like the keys used to encrypt each tree.
asymmetricKeySize = 512

symmetricKeySize = 32

seedSize = 40

defaultExponent = 65537

makePRNG = PRNG . R.drgNewTest . toSeed

toSymmetric = SymmetricKey

newSymmetric = toSymmetric <$> randomBytes symmetricKeySize

-- todo: use standard serialization formats for public and private keys

decryptPrivate password privateKey =
  decryptSymmetric
  (SymmetricKey $ deriveKey symmetricKeySize password privateKey) privateKey
  >>= C.eitherToAction . toPrivate

fromPrivate (PrivateKey key) =
  BS.concat [fromPublic $ PublicKey $ RSAT.private_pub key,
             B.fromInteger $ RSAT.private_d key]

parsePrivate = do
  public <- parsePublic
  d <- B.toInteger
  return $ PrivateKey $ RSAT.PrivateKey public d 0 0 0 0 0

toPrivate s = C.eval parsePrivate $ ParseState s

fromPublic (PublicKey key) =
  BS.concat [B.fromInteger $ RSAT.public_size key,
             B.fromInteger $ RSAT.public_n key,
             B.fromInteger $ RSAT.public_e key]

parsePublic =
  M.liftM3 RSAT.PublicKey
  B.toInteger
  B.toInteger
  B.toInteger

toPublic s = PublicKey <$> C.eval parsePublic (ParseState s)

decryptAsymmetric (PrivateKey key) ciphertext = do
  PRNG drg <- S.get

  let (result, drg') = R.withDRG drg $ PKCS15.decryptSafer key ciphertext

  S.put $ PRNG drg'

  case result of
    Left error -> C.exception $ show error
    Right plaintext -> return plaintext

encryptAsymmetric (PublicKey key) plaintext = do
   PRNG drg <- S.get

   let (result, drg') = R.withDRG drg $ PKCS15.encrypt key plaintext

   S.put $ PRNG drg'

   case result of
     Left error -> C.exception $ show error
     Right ciphertext -> return ciphertext

decryptSymmetric (SymmetricKey key) ciphertext = do
  let (nonce, tail) = BS.splitAt 16 ciphertext
  case CT.cipherInit key :: E.CryptoFailable Cipher of
    E.CryptoPassed cipher -> case CT.makeIV nonce of
      Nothing -> C.exception "unable to make initialization vector"

      Just iv -> return $ CT.ctrCombine cipher iv tail

    E.CryptoFailed message -> C.exception $ show message

encryptSymmetric (SymmetricKey key) plaintext = do
  nonce <- randomBytes 16
  case CT.cipherInit key :: E.CryptoFailable Cipher of
    E.CryptoPassed cipher -> case CT.makeIV nonce of
      Nothing -> C.exception "unable to make initialization vector"

      Just iv -> return $ nonce `BS.append` CT.ctrCombine cipher iv plaintext

    E.CryptoFailed error -> C.exception $ show error

withRandomBytes :: R.DRG g => g -> Int -> (BS.ByteString, g)
withRandomBytes drg count = R.withRandomBytes drg count id

randomBytes count = do
  PRNG drg <- S.get

  let (result, drg') = withRandomBytes drg count

  S.put $ PRNG drg'

  return result

toSeed s = (B.toWord64 a,
            B.toWord64 b,
            B.toWord64 c,
            B.toWord64 d,
            B.toWord64 e)
  where (a, as) = BS.splitAt 8 s
        (b, bs) = BS.splitAt 8 as
        (c, cs) = BS.splitAt 8 bs
        (d, ds) = BS.splitAt 8 cs
        (e,  _) = BS.splitAt 8 ds

deriveSeed password salt = toSeed $ deriveKey seedSize password salt

deriveKey size = PBKDF2.generate prf $ PBKDF2.Parameters iterations size

derivePrivate password salt =
  PrivateKey $ snd $ fst $ R.withDRG (R.drgNewTest $ deriveSeed password salt)
  $ RSA.generate asymmetricKeySize defaultExponent

dummyKey = derivePrivate
           ("secret" :: BS.ByteString)
           ("trust@every.one" :: BS.ByteString)

derivePublic = PublicKey . RSAT.private_pub . getPrivateKey

sign (PrivateKey key) text = do
  PRNG drg <- S.get

  let (result, drg') =
        R.withDRG drg $ PKCS15.signSafer (Just hashAlgorithm) key text

  S.put $ PRNG drg'

  case result of
    Left error -> C.exception $ show error
    Right s -> return s

verify (PublicKey key) signature text =
  PKCS15.verify (Just hashAlgorithm) key text signature

hash :: Foldable t => t BS.ByteString -> BS.ByteString
hash = BA.convert . H.hashFinalize
       . F.foldr (flip H.hashUpdate) (H.hashInitWith hashAlgorithm)
