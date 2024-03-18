{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Support the encryption requirements of CHK.
module Tahoe.CHK.Encrypt (encrypt, encryptLazy, decrypt, decryptLazy) where

import Crypto.Cipher.Types (BlockCipher (blockSize, ctrCombine), ivAdd, nullIV)
import Data.ByteArray (ByteArray)
import qualified Data.ByteString.Lazy as LBS
import Data.List (unfoldr)

{- | CTR-mode encrypt a byte string using some block cipher.

 When used for CHKv1 or CHKv2 the block cipher should be AES128.

 This replaces allmydata.immutable.upload.EncryptAnUploadable

 The only noteworthy piece here is that encryption starts with the zero IV.
-}
encrypt :: (BlockCipher cipher, ByteArray ba) => cipher -> ba -> ba
encrypt key = ctrCombine key nullIV

-- | Like encrypt but operate on lazy bytestrings.
encryptLazy :: forall cipher. BlockCipher cipher => cipher -> LBS.ByteString -> LBS.ByteString
encryptLazy cipher lbs = LBS.concat . (LBS.fromStrict <$>) $ zipWith (ctrCombine cipher) ivs blocks
  where
    -- The underlying encryption function works on strict bytes.  Here's the
    -- number of *blocks* to feed to it (that is, to make strict) at a time.
    -- This value here is a magic number that is meant to represent a good
    -- compromise between performance and number of bytes forced at one time.
    workingBlocks = 1024 * 16

    -- The size of a block is determined by the cipher.
    workingBytes = workingBlocks * blockSize @cipher undefined

    ivs = iterate (`ivAdd` workingBlocks) nullIV
    blocks = LBS.toStrict <$> unfoldr takeChunk lbs

    takeChunk "" = Nothing
    takeChunk xs = Just . LBS.splitAt (fromIntegral workingBytes) $ xs

-- | AES128-CTR decrypt a byte string in the manner used by CHK.
decrypt :: (BlockCipher cipher, ByteArray ba) => cipher -> ba -> ba
decrypt = encrypt

-- | Like decrypt but operate on lazy bytestrings.
decryptLazy :: BlockCipher cipher => cipher -> LBS.ByteString -> LBS.ByteString
decryptLazy = encryptLazy
