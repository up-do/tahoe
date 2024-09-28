-- | Implement the encryption scheme used by SDMF.
module Tahoe.SDMF.Internal.Encrypting where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (BlockCipher (blockSize), ctrCombine, makeIV, nullIV)
import Crypto.Random (MonadRandom (getRandomBytes))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Tahoe.SDMF.Internal.Keys as Keys

-- | Randomly generate a new IV suitable for use with the block cipher used by SDMF.
randomIV :: MonadRandom m => m (Maybe Keys.SDMF_IV)
randomIV = (fmap Keys.SDMF_IV . makeIV :: B.ByteString -> Maybe Keys.SDMF_IV) <$> getRandomBytes (blockSize (undefined :: AES128))

{- | Encrypt plaintext bytes according to the scheme used for SDMF share
 construction.
-}
encrypt :: Keys.KeyPair -> Keys.SDMF_IV -> LB.ByteString -> LB.ByteString
encrypt keypair iv = encryptWithDataKey dataKey
  where
    signatureKey = Keys.toSignatureKey keypair
    (Just writeKey) = Keys.deriveWriteKey signatureKey
    (Just readKey) = Keys.deriveReadKey writeKey
    (Just dataKey) = Keys.deriveDataKey iv readKey

{- | Decrypt ciphertext bytes according to the scheme used for SDMF share
 construction.
-}
decrypt :: Keys.Read -> Keys.SDMF_IV -> LB.ByteString -> LB.ByteString
decrypt readKey iv = decryptWithDataKey dataKey
  where
    (Just dataKey) = Keys.deriveDataKey iv readKey

{- | Encrypt plaintext bytes according to the scheme used for SDMF share
 construction using a pre-computed data encryption key.
-}
encryptWithDataKey :: Keys.Data -> LB.ByteString -> LB.ByteString
encryptWithDataKey Keys.Data{unData} = LB.fromStrict . ctrCombine unData nullIV . LB.toStrict

{- | Decrypt ciphertext bytes according to the scheme used for SDMF share
 construction using a pre-computed data encryption key.
-}
decryptWithDataKey :: Keys.Data -> LB.ByteString -> LB.ByteString
decryptWithDataKey = encryptWithDataKey
