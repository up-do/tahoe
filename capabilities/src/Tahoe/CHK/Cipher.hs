{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tahoe.CHK.Cipher (
    Key (keyBytes, keyCipher),
) where

import Control.DeepSeq (NFData)
import Crypto.Cipher.Types (AEAD, BlockCipher (..), Cipher (..))
import Data.ByteArray (ScrubbedBytes)
import qualified Data.ByteArray as BA
import Data.Coerce (coerce)
import GHC.Generics (Generic)

{- | A block cipher key which can be deserialized from or serialized to a
 ByteArray.

 This is a wrapper around Crypto.Cipher.Types.Cipher which does not provide a
 way to recover the original bytes of the key.  We provide this by keeping the
 original bytes around.
-}
data Key cipher = Key {keyBytes :: ScrubbedBytes, keyCipher :: cipher}

deriving instance Generic (Key cipher)
deriving instance NFData cipher => NFData (Key cipher)

instance forall cipher. Cipher cipher => Cipher (Key cipher) where
    cipherInit bs = Key (BA.convert bs) <$> cipherInit bs
    cipherName _ = cipherName @cipher undefined
    cipherKeySize _ = cipherKeySize @cipher undefined

instance forall cipher. BlockCipher cipher => BlockCipher (Key cipher) where
    blockSize _ = blockSize @cipher undefined
    ecbEncrypt = ecbEncrypt . keyCipher
    ecbDecrypt = ecbDecrypt . keyCipher
    cbcEncrypt (Key _ cipher) iv = cbcEncrypt cipher (coerce iv)
    cbcDecrypt (Key _ cipher) iv = cbcDecrypt cipher (coerce iv)

    cfbEncrypt (Key _ cipher) iv = cfbEncrypt cipher (coerce iv)
    cfbDecrypt (Key _ cipher) iv = cfbDecrypt cipher (coerce iv)
    ctrCombine (Key _ cipher) iv = ctrCombine cipher (coerce iv)

    aeadInit mode (Key _ cipher) iv = wrap <$> aeadInit mode cipher iv
      where
        wrap = coerce @(AEAD cipher) @(AEAD (Key cipher))

instance BA.ByteArrayAccess (Key cipher) where
    length (Key ba _) = BA.length ba
    withByteArray (Key ba _) = BA.withByteArray ba
