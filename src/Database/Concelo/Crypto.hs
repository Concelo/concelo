module Database.Concelo.Crypto
  ( deriveKey
  , derivePublic
  , decryptPrivate
  , decryptSymmetric
  , encryptSymmetric
  , randomBytes
  , PRNG()
  , makePRNG
  , sign
  , hash ) where

import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types (getCurveByName, SEC_p521r1, Point(Point))
import Crypto.PubKey.ECC.ECDSA (KeyPair(KeyPair))
import Crypto.Hash (SHA256(SHA256), hashInitWith, hashUpdate, hashFinalize)
import Crypto.KDF.PBKDF2 (prfHMAC, generate, Parameters(Parameters))
import Crypto.Number.Serialize (i2osp, os2ip)
import Crypto.Cipher.AES (AES256())
import Crypto.Cipher.Types (CryptoFailable(CryptoPassed, CryptoFailed), makeIV)
import Crypto.Random.Types (ChaChaDRG)
import Cryoto.Random (withRandomBytes)

import Database.Concelo.Control (exception)

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import qualified Control.Monad.State.Class as S

newtype PRNG = PRNG ChaChaDRG

type Cipher = AES256

hashAlgorithm = SHA256

-- todo: does WebCrypto support Scrypt or Bcrypt?  If so, use one of them.
prf = prfHMAC hashAlgorithm

iterations = 4096

makePRNG = PRNG . drgNewTest

decryptPrivate password privateKey =
  decryptSymmetric (deriveKey password privateKey) privateKey

decryptSymmetric key ciphertext = do
  let (head, tail) = BS.splitAt 16 ciphertext
  iv <- makeIV head
  case cipherInit key :: CryptoFailable Cipher of
    CryptoPassed cipher -> Just $ ctrCombine cipher iv tail
    CryptoFailed _ -> Nothing

encryptSymmetric key plaintext = do
  nonce <- randomBytes 16
  case cipherInit key :: CryptoFailable Cipher of
    CryptoPassed cipher ->
      return $ nonce `BS.append` ctrCombine cipher iv plaintext
    CryptoFailed error ->
      exception $ show error

symmetricKeySize = 32

randomBytes count =
  PRNG drg <- S.get

  let (result, drg') = withRandomBytes drg count BA.convert

  S.put $ PRNG drg

  return result

deriveKey = generate prf . Parameters iterations

-- todo: some experts are suspicious of the NIST curves, so find one
-- that's more generally trusted
curve = getCurveByName SEC_p521r1

derivePublic = pointToString . generateQ curve . stringToInteger

stringToInteger = os2ip

integerToString = i2osp

pointToString (Point x y) = combine (integerToString x) (integerToString y)

stringToPoint s = Point (stringToInteger x) (stringToInteger y) where
  (c, cs) = case BS.uncons s of
    Nothing -> error "empty point"
    Just pair -> pair

  (x, y) = splitAt c cs

combine a b = concat [ BS.singleton $ count a, a, b ] where
  count a = let c = length a in
    if c > 255 then error "string too large" else c

stringToPrivate s = toPrivateKey $ KeyPair curve (generateQ curve i) i where
  i = stringToInteger s

sign privateKey text = do
  PRNG drg <- S.get

  let (result, drg') =
        withDRG drg
        (ECDSA.sign (stringToPrivate privateKey) hashAlgorithm text)

  S.put $ PRNG drg

  return result

stringToPublic s = PublicKey curve $ stringToPoint s

verify publicKey signature text =
  ECDSA.verify hashAlgorithm (stringToPublic publicKey) signature text

hash = BA.convert . hashFinalize
       . foldr (flip hashUpdate) (hashInitWith hashAlgorithm)
