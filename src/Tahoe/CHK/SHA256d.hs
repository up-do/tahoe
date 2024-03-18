{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Tahoe.CHK.SHA256d where

import Crypto.Hash (Context, Digest, HashAlgorithm, SHA256, digestFromByteString, hash)
import Crypto.Hash.IO (HashAlgorithm (..))
import qualified Data.ByteArray as BA
import Data.ByteString (packCStringLen, useAsCString)
import qualified Data.ByteString as B
import Data.ByteString.Base32 (decodeBase32Unpadded, encodeBase32Unpadded')
import qualified Data.ByteString.Char8 as C8
import Data.Char (toLower)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Maybe (fromJust, fromMaybe)
import Data.Primitive (Ptr)
import Data.Primitive.Ptr (copyPtr)
import Data.String (IsString (..))
import Data.TreeDiff.Class (ToExpr (..))
import Foreign.C (CString)

{- | A newtype wrapper around Digest which comes with the string interpretation
 Tahoe-LAFS is accustomed to (lowercase base32 rather than lowercase base16),
 as well as a ToExpr instance for participation in nice diff computation.
-}
newtype Digest' a = Digest' (Digest a) deriving newtype (Eq, Ord)

instance HashAlgorithm hash => Show (Digest' hash) where
    show (Digest' digest) = fmap toLower . C8.unpack . encodeBase32Unpadded' . toBytes $ digest

instance ToExpr (Digest' a) where
    toExpr (Digest' d) = toExpr (toBytes d)

deriving instance BA.ByteArrayAccess (Digest' hash)

instance HashAlgorithm hash => IsString (Digest' hash) where
    fromString =
        Digest'
            . fromMaybe (error "invalid base32-encoded digest")
            . either (error "invalid base32-encoded digest") digestFromByteString
            . decodeBase32Unpadded
            . C8.pack

-- | The all-zero digest value at a specific hash algorithm.
zero :: forall hash. HashAlgorithm hash => Digest' hash
zero = Digest' . fromJust . digestFromByteString @hash . B.replicate (hashDigestSize (undefined :: hash)) $ 0

{- | A hash algorithm which computes its digest using the parameterized hash
 algorithm and then computes a digest of _that_ digest with the same hash
 algorithm.
-}
data DoubleHash hash = DoubleHash

-- | The double SHA256 hash algorithm.
type SHA256d = DoubleHash SHA256

deriving instance Show hash => Show (DoubleHash hash)
deriving instance Data hash => Data (DoubleHash hash)

instance HashAlgorithm hash => HashAlgorithm (DoubleHash hash) where
    type HashBlockSize (DoubleHash hash) = HashBlockSize hash
    type HashDigestSize (DoubleHash hash) = HashDigestSize hash
    type HashInternalContextSize (DoubleHash hash) = HashInternalContextSize hash

    -- cryptonite doesn't force the argument and neither will we, allowing the
    -- pattern of passing `undefined` around as the value.
    hashBlockSize _ = hashBlockSize @hash undefined
    hashDigestSize _ = hashDigestSize @hash undefined
    hashInternalContextSize _ = hashInternalContextSize @hash undefined

    -- We'll re-use a Context for the wrapped hash type.
    hashInternalInit ctxPtr = hashInternalInit (coerce ctxPtr :: Ptr (Context hash))
    hashInternalUpdate ctxPtr = hashInternalUpdate (coerce ctxPtr :: Ptr (Context hash))
    hashInternalFinalize ctxPtr digestPtr = do
        -- Do the first pass
        hashInternalFinalize
            (coerce ctxPtr :: Ptr (Context hash))
            (coerce digestPtr :: Ptr (Digest hash))
        -- And then a second pass over the result
        firstHash <- digestPtrToByteString digestPtr
        let secondHash = hash firstHash :: Digest hash
        -- And shove the second result into the output
        useAsCString (toBytes secondHash) $ \new -> copyPtr (coerce digestPtr :: CString) new (hashDigestSize @hash undefined)

-- | Extract the bytes from a value like a `Digest' hash`.
toBytes :: BA.ByteArrayAccess a => a -> B.ByteString
toBytes = B.pack . BA.unpack

{- | Read the digest bytes out of a pointer to a Digest.  This uses some
 coerce trickery.  I hope it's not too broken.
-}
digestPtrToByteString :: forall hash. HashAlgorithm hash => Ptr (Digest hash) -> IO B.ByteString
digestPtrToByteString = packCStringLen . (,hashDigestSize @hash undefined) . coerce @(Ptr (Digest hash)) @CString
