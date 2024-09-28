{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Tahoe.CHK.Crypto (
    sha1,
    sha256,
    sha256d,
    storageIndexLength,
    toBytes,
    taggedHash,
    taggedHash',
    taggedPairHash,
    taggedPairHash',
    blockHash,
    blockHash',
    storageIndexHash,
    ciphertextTag,
    ciphertextSegmentHash,
    ciphertextSegmentHash',
    uriExtensionHash,
    convergenceEncryptionTag,
    convergenceEncryptionHashLazy,
    convergenceSecretLength,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Crypto.Hash (
    Digest,
    HashAlgorithm,
    hash,
    hashDigestSize,
    hashlazy,
 )
import Data.ByteArray (convert)

import Crypto.Cipher.AES (AES128)
import Crypto.Hash.Algorithms (
    SHA1,
    SHA256 (SHA256),
 )

import Tahoe.CHK.Cipher (Key)
import Tahoe.CHK.SHA256d (Digest' (..), SHA256d, toBytes)
import Tahoe.CHK.Types (Parameters (Parameters), StorageIndex)
import Tahoe.CHK.URIExtension (
    URIExtension,
    showBytes,
    uriExtensionToBytes,
 )
import Tahoe.Netstring (
    netstring,
 )

sha1 :: B.ByteString -> B.ByteString
sha1 xs = toBytes (hash xs :: Digest SHA1)

sha256 :: B.ByteString -> B.ByteString
sha256 xs = toBytes (hash xs :: Digest SHA256)

sha256d :: B.ByteString -> B.ByteString
sha256d = toBytes . (hash :: B.ByteString -> Digest SHA256d)

taggedHash :: Int -> B.ByteString -> B.ByteString -> B.ByteString
taggedHash size tag bytes = B.take size . toBytes $ taggedHash' @SHA256d tag bytes

{- | Compute the "tagged hash" of a byte string: the hash of the concatenation
 of the netstring encoding of a tag and the given bytes.
-}
taggedHash' :: HashAlgorithm hash => B.ByteString -> B.ByteString -> Digest' hash
taggedHash' tag bytes = Digest' . hash . B.concat $ [netstring tag, bytes]

taggedPairHash :: Int -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
taggedPairHash size tag left right = B.take size . toBytes $ taggedPairHash' @SHA256d tag left right

{- | Compute the "tagged pair hash" of two byte strings: the hash of the
 concatenation of the netstring encoding of a tag and each of two other byte
 strings.
-}
taggedPairHash' :: HashAlgorithm hash => B.ByteString -> B.ByteString -> B.ByteString -> Digest' hash
taggedPairHash' tag left right = Digest' . hash . B.concat $ [netstring tag, netstring left, netstring right]

blockTag :: B.ByteString
blockTag = "allmydata_encoded_subshare_v1"

-- allmydata.util.hashutil.block_hash
blockHash :: B.ByteString -> B.ByteString
blockHash = taggedHash (hashDigestSize SHA256) blockTag

{- | Compute the hash of a share block.  This is the same function as
 allmydata.util.hashutil.block_hash.
-}
blockHash' :: HashAlgorithm hash => B.ByteString -> Digest' hash
blockHash' = taggedHash' blockTag

storageIndexTag :: B.ByteString
storageIndexTag = "allmydata_immutable_key_to_storage_index_v1"

-- Compute the storage index for a given encryption key
-- allmydata.util.hashutil.storage_index_hash
storageIndexHash :: Key AES128 -> StorageIndex
storageIndexHash = taggedHash storageIndexLength storageIndexTag . convert

ciphertextTag :: B.ByteString
ciphertextTag = "allmydata_crypttext_v1"

ciphertextSegmentTag :: B.ByteString
ciphertextSegmentTag = "allmydata_crypttext_segment_v1"

ciphertextSegmentHash :: B.ByteString -> B.ByteString
ciphertextSegmentHash = toBytes . ciphertextSegmentHash' @SHA256d

-- | Compute the hash of a segment of ciphertext.
ciphertextSegmentHash' :: HashAlgorithm hash => B.ByteString -> Digest' hash
ciphertextSegmentHash' = taggedHash' ciphertextSegmentTag

uriExtensionTag :: B.ByteString
uriExtensionTag = "allmydata_uri_extension_v1"

uriExtensionHash :: URIExtension -> B.ByteString
uriExtensionHash = taggedHash (hashDigestSize SHA256) uriExtensionTag . uriExtensionToBytes

convergenceEncryptionTagPrefix :: B.ByteString
convergenceEncryptionTagPrefix = "allmydata_immutable_content_to_key_with_added_secret_v1+"

convergenceEncryptionTag :: B.ByteString -> Parameters -> B.ByteString
convergenceEncryptionTag secret (Parameters segmentSize total _ required) =
    tag
  where
    tag = B.concat [convergenceEncryptionTagPrefix, netstring secret, netstring paramTag]
    paramTag = B.intercalate "," . map showBytes $ [requiredI, totalI, segmentSizeI]
    requiredI = toInteger required
    totalI = toInteger total
    segmentSizeI = toInteger segmentSize

-- Compute the strict convergence encryption hash on a lazy data parameter.
convergenceEncryptionHashLazy :: B.ByteString -> Parameters -> BL.ByteString -> B.ByteString
convergenceEncryptionHashLazy secret params bytes =
    -- It was somewhat helpful during development/debugging to make this
    -- function return this instead:
    --
    --     BL.toStrict toHash
    --
    B.take convergenceSecretLength theSHA256d
  where
    theSHA256d = toBytes (hashlazy toHash :: Digest SHA256d)

    toHash :: BL.ByteString
    toHash = BL.concat [tag, bytes]

    tag = BL.fromStrict . netstring $ convergenceEncryptionTag secret params

type ByteLength = Int

convergenceSecretLength :: ByteLength
convergenceSecretLength = 16

storageIndexLength :: ByteLength
storageIndexLength = 16
