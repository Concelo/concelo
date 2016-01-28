module Database.Concelo.Crypto
  ( derivePrivate
  , derivePublic
  , byteStringToInteger
  , integerToByteString ) where

import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types (getCurveByName, SEC_p521r1)
import Crypto.Hash (SHA256(SHA256))
import Crypto.KDF.PBKDF2 (prfHMAC, generate)
import Crypto.Number.Serialize (i2osp, os2ip)

prf = prfHMAC SHA256

iterations = 4096

derivePrivate = generate prf . Parameters iterations

curve = getCurveByName SEC_p521r1

derivePublic = pointToByteString . generateQ curve . byteStringToInteger

byteStringToInteger = os2ip

integerToByteString = i2osp

pointToByteString (Point x y) =
  combine (integerToByteString x) (integerToByteString y)

combine a b = concat [ BS.singleton $ count a, a, b ] where
  count a = let c = length a in
    if c > 255 then error "bytestring too large" else c
