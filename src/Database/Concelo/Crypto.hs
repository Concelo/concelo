{-# LANGUAGE LambdaCase #-}
module Database.Concelo.Crypto
  ( derivePrivate
  , derivePublic
  , decryptPrivate
  , decryptSymmetric
  , encryptSymmetric
  , randomBytes
  , PRNG()
  , makePRNG
  , sign
  , verify
  , hash ) where

import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types (getCurveByName, CurveName(SEC_p521r1),
                                Point(Point, PointO))
import Crypto.PubKey.ECC.ECDSA (KeyPair(KeyPair), toPrivateKey,
                                Signature(Signature))
import Crypto.Hash (SHA256(SHA256), hashInitWith, hashUpdate, hashFinalize)
import Crypto.KDF.PBKDF2 (prfHMAC, generate, Parameters(Parameters))
import Crypto.Number.Serialize (i2osp, os2ip)
import Crypto.Cipher.AES (AES256())
import Crypto.Cipher.Types (makeIV, cipherInit, ctrCombine)
import Crypto.Random (ChaChaDRG, withRandomBytes, drgNewTest, withDRG)
import Crypto.Error (CryptoFailable(CryptoPassed, CryptoFailed))

import Database.Concelo.Control (exception, Action)

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import qualified Control.Monad.State.Class as S

newtype PRNG = PRNG ChaChaDRG

newtype PrivateKey = PrivateKey BS.ByteString

newtype PublicKey = PublicKey BS.ByteString

newtype SymmetricKey = SymmetricKey BS.ByteString

type Cipher = AES256

hashAlgorithm = SHA256

-- todo: does WebCrypto support Scrypt or Bcrypt?  If so, use one of them.
prf = prfHMAC hashAlgorithm

iterations = 4096

asymmetricKeySize = 64

makePRNG = PRNG . drgNewTest

decryptPrivate :: BS.ByteString -> BS.ByteString -> Action () PrivateKey
decryptPrivate password privateKey =
  PrivateKey <$> decryptSymmetric
  (SymmetricKey $ deriveKey symmetricKeySize password privateKey) privateKey

decryptSymmetric :: SymmetricKey -> BS.ByteString -> Action () BS.ByteString
decryptSymmetric (SymmetricKey key) ciphertext = do
  let (nonce, tail) = BS.splitAt 16 ciphertext
  case cipherInit key :: CryptoFailable Cipher of
    CryptoPassed cipher -> case makeIV nonce of
      Nothing -> exception "unable to make initialization vector"

      Just iv -> return $ ctrCombine cipher iv tail

    CryptoFailed message -> exception $ show message

encryptSymmetric :: SymmetricKey -> BS.ByteString -> Action PRNG BS.ByteString
encryptSymmetric (SymmetricKey key) plaintext = do
  nonce <- randomBytes 16
  case cipherInit key :: CryptoFailable Cipher of
    CryptoPassed cipher -> case makeIV nonce of
      Nothing -> exception "unable to make initialization vector"

      Just iv -> return $ nonce `BS.append` ctrCombine cipher iv plaintext

    CryptoFailed error -> exception $ show error

symmetricKeySize = 32

randomBytes :: Int -> Action PRNG BS.ByteString
randomBytes count = do
  PRNG drg <- S.get

  let (result, drg') = withRandomBytes drg count id

  S.put $ PRNG drg'

  return result

deriveKey size = generate prf $ Parameters iterations size

derivePrivate :: BS.ByteString -> BS.ByteString -> PrivateKey
derivePrivate password salt =
  PrivateKey $ deriveKey asymmetricKeySize password salt

-- todo: some experts are suspicious of the NIST curves, so find one
-- that's more generally trusted
curve = getCurveByName SEC_p521r1

derivePublic = PublicKey . pointToString . generateQ curve . stringToInteger

stringToInteger :: BS.ByteString -> Integer
stringToInteger = os2ip

integerToString :: Integer -> BS.ByteString
integerToString = i2osp

stringToIntegerPair s = (stringToInteger x, stringToInteger y) where
  (c, cs) = case BS.uncons s of
    Nothing -> error "empty point"
    Just pair -> pair

  (x, y) = BS.splitAt (fromIntegral c) cs

integerPairToString (ai, bi) =
  BS.concat [ BS.singleton $ fromIntegral $ count a, a, b ] where
    a = integerToString ai
    b = integerToString bi
    count a = let c = BS.length a in
      if c > 255 then error "string too large" else c

pointToString = \case
  Point x y -> integerPairToString (x, y)
  PointO -> error "infinity probably isn't what we want"

stringToPoint s = Point x y where (x, y) = stringToIntegerPair s

stringToPrivate s = toPrivateKey $ KeyPair curve (generateQ curve i) i where
  i = stringToInteger s

stringToSignature s = Signature x y where (x, y) = stringToIntegerPair s

signatureToString (Signature x y) = integerPairToString (x, y)

sign :: PrivateKey -> BS.ByteString -> Action PRNG BS.ByteString
sign (PrivateKey privateKey) text = do
  PRNG drg <- S.get

  let (result, drg') =
        withDRG drg
        (ECDSA.sign (stringToPrivate privateKey) hashAlgorithm text)

  S.put $ PRNG drg'

  return $ signatureToString result

stringToPublic s = ECDSA.PublicKey curve $ stringToPoint s

verify publicKey signature text =
  ECDSA.verify hashAlgorithm (stringToPublic publicKey)
  (stringToSignature signature) text

hash :: Foldable t => t BS.ByteString -> BS.ByteString
hash = BA.convert . hashFinalize
       . foldr (flip hashUpdate) (hashInitWith hashAlgorithm)
