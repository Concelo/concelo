module Database.Concelo.Crypto
  ( deriveKey
  , derivePublic
  , decryptPrivate
  , decryptSymmetric
  , PRNG()
  , makePRNG
  , sign ) where

import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types (getCurveByName, SEC_p521r1, Point(Point))
import Crypto.PubKey.ECC.ECDSA (KeyPair(KeyPair))
import Crypto.Hash (SHA256(SHA256))
import Crypto.KDF.PBKDF2 (prfHMAC, generate, Parameters(Parameters))
import Crypto.Number.Serialize (i2osp, os2ip)
import Crypto.Cipher.AES (AES256())
import Crypto.Cipher.Types (CryptoFailable(CryptoPassed, CryptoFailed))
import Crypto.Random.Types (ChaChaDRG)

import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import qualified Data.ByteString as BS
import qualified Control.Monad.State.Class as S

newtype PRNG = PRNG ChaChaDRG

hash = SHA256

-- todo: does WebCrypto support Scrypt or Bcrypt?  If so, use one of them.
prf = prfHMAC hash

iterations = 4096

makePRNG = PRNG . drgNewTest

decryptPrivate password privateKey =
  decryptSymmetric (deriveKey password privateKey) privateKey

decryptSymmetric key ciphertext = do
  let (head, tail) = BS.splitAt 128 ciphertext
  iv <- makeIV nonce
  case cipherInit key :: CryptoFailable AES256 of
    CryptoPassed cipher -> Just $ ctrCombine cipher iv tail
    CryptoFailed _ -> Nothing

deriveKey = generate prf . Parameters iterations

-- todo: many are suspicious of the NIST curves, so find one that's
-- more generally trusted
curve = getCurveByName SEC_p521r1

derivePublic = pointToString . generateQ curve . stringToInteger

stringToInteger = os2ip

integerToString = i2osp

pointToString (Point x y) = combine (integerToString x) (integerToString y)

combine a b = concat [ BS.singleton $ count a, a, b ] where
  count a = let c = length a in
    if c > 255 then error "string too large" else c

stringToPrivate s = toPrivateKey $ KeyPair curve (generateQ curve $ i) i where
  i = stringToInteger s

sign privateKey text = do
  PRNG drg <- S.get

  let (result, drg') =
        withDRG drg (ECDSA.sign (stringToPrivate privateKey) hash text)

  S.put $ PRNG drg

  return result
