{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Generators where

import Control.Lens (over, view)
import Control.Lens.Tuple (_2)
import Control.Monad (zipWithM)
import Crypto.Hash (
    HashAlgorithm,
    digestFromByteString,
    hashDigestSize,
 )
import Crypto.Hash.Algorithms (
    SHA256 (SHA256),
 )
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Base32 (encodeBase32Unpadded)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Hedgehog (MonadGen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Tahoe.CHK.Capability (Reader, fingerprint, verifier)
import Tahoe.CHK.Crypto (storageIndexLength)
import Tahoe.CHK.Merkle (MerkleTree, leafHashes, makeTreePartial)
import Tahoe.CHK.SHA256d (Digest' (..), SHA256d, zero)
import Tahoe.CHK.Server (StorageServerAnnouncement (StorageServerAnnouncement))
import Tahoe.CHK.Share (Crypttext, Share (..), blocks, crypttextHashTree, neededHashes)
import Tahoe.CHK.Types (Parameters (..), Required, ShareNum, StorageIndex, Total)
import Tahoe.CHK.URIExtension (URIExtension (URIExtension))

-- | The maximum value an Int64 can represent.
maxInt64 :: Integer
maxInt64 = fromIntegral (maxBound :: Int64)

-- | Generate Parameters values for which all field invariants hold.
genParameters :: MonadGen m => m Parameters
genParameters = do
    -- The normal smallest amount of data for a CHK share is 56 bytes.  We can
    -- set the segment size smaller than this to break those 56 bytes into
    -- multiple segments but we end up with a much simplified share if the
    -- segment size equals the data set.  So, set the origin - the value to
    -- shrink towards - to 56.
    paramSegmentSize <- Gen.integral (Range.exponentialFrom 56 1 maxInt64)
    paramTotalShares <- Gen.integral (Range.linear 1 256)
    paramRequiredShares <- Gen.integral (Range.linear 1 paramTotalShares)
    -- XXX We're going to get rid of "Happy" from this type.  For now it's
    -- easier not to let this value vary and it doesn't hurt anything.
    let paramHappyShares = 1
    pure $ Parameters{paramSegmentSize, paramTotalShares, paramHappyShares, paramRequiredShares}

-- | Generate URIExtension values which are not necessarily well-formed.
genURIExtension :: MonadGen m => m URIExtension
genURIExtension =
    URIExtension
        <$> Gen.bytes (Range.linear 1 32)
        <*> genParameters
        <*> genParameters
        <*> Gen.integral (Range.exponential 1 maxInt64)
        <*> Gen.integral (Range.exponential 1 maxInt64)
        <*> Gen.integral (Range.exponential 1 (maxBound :: Int))
        <*> Gen.integral (Range.linear 1 256)
        <*> Gen.integral (Range.linear 1 256)
        <*> digests
        <*> digests
        <*> digests

-- | Generate Digest' values for some hash algorithm.  Shrinks toward "aaa..."
digests :: forall m hash. (MonadGen m, HashAlgorithm hash) => m (Digest' hash)
digests =
    Digest'
        . fromMaybe (error "Failed to interpret bytes as digest")
        . digestFromByteString
        <$> Gen.bytes (Range.singleton (hashDigestSize (undefined :: hash)))

-- | Generate ByteStrings which could be sha256d digests.
genHash :: MonadGen m => m BS.ByteString
genHash = Gen.bytes . Range.singleton . hashDigestSize $ SHA256

-- | Generate share-shaped data without trying to make the data itself coherent.
shares :: MonadGen m => m Share
shares = do
    -- XXX It would be nice to explore the full space but the tests operate in
    -- memory (and even if they didn't, they would be constrained by disk
    -- space and speed) and maxBound :: Int64 is a lot of bytes...
    let maxSize = 65536
    _blockSize <- Gen.integral (Range.exponential 1 maxSize)
    numBlocks <- Gen.integral (Range.exponential 1 32)

    -- We don't make shareDataSize agree with the rest of the share data
    -- because the field is supposedly unused so everyone should just ignore
    -- it and not mind if we put garbage there.
    --
    -- We can go all the way up to an unreasonable maximum here because this
    -- doesn't influence how many bytes are actually in the share.
    _dataSize <- fromIntegral <$> Gen.integral (Range.linear 1 maxInt64)

    _blocks <- Gen.list (Range.singleton numBlocks) (LBS.fromStrict <$> Gen.bytes (Range.singleton $ fromIntegral _blockSize))

    -- XXX These merkle trees and the "needed hashes" list all have a size
    -- that really needs to be dictated by the encoding parameters (k and n).
    _plaintextHashTree <- merkleTrees (Range.exponential 1 256)
    _crypttextHashTree <- merkleTrees (Range.exponential 1 256)
    _blockHashTree <- merkleTrees (Range.exponential 1 256)
    _neededHashes <- Gen.list (Range.exponential 1 100) ((,) <$> Gen.integral (Range.exponential 1 255) <*> digests)

    -- XXX A valid share will have a URI extension that agrees with some of
    -- the other fields we've just generated, which we're not even trying to
    -- do here.
    _uriExtension <- genURIExtension

    pure $ Share{..}

merkleTrees :: (HashAlgorithm hash, MonadGen m) => Range.Range Int -> m (MerkleTree value hash)
merkleTrees r = makeTreePartial <$> Gen.list r digests

storageIndexes :: MonadGen m => m StorageIndex
storageIndexes = Gen.bytes (Range.singleton storageIndexLength)

shareNumbers :: MonadGen m => m ShareNum
shareNumbers = Gen.integral Range.linearBounded

storageServerIdentifiers :: MonadGen m => m T.Text
storageServerIdentifiers =
    Gen.choice
        -- XXX Maybe more than alpha?
        [ Gen.text (Range.linear 1 64) Gen.alpha
        , encodeBase32Unpadded <$> Gen.bytes (Range.linear 1 64)
        ]

-- | Generate storage server anonymous storage service announcements.
storageServerAnnouncements :: MonadGen m => m StorageServerAnnouncement
storageServerAnnouncements =
    StorageServerAnnouncement
        <$> Gen.maybe storageServiceFURLs
        -- XXX Maybe more than alpha?
        <*> Gen.maybe (Gen.text (Range.linear 1 32) Gen.alpha)
        -- XXX 32 bytes?
        <*> Gen.maybe (Gen.bytes (Range.singleton 32))

{- | Generate text that could be a storage server fURL.  TODO: Represent fURLs
 _and NURLs_ in a structured way instead of with Text.
-}
storageServiceFURLs :: MonadGen m => m T.Text
storageServiceFURLs = do
    -- XXX 32 bytes?
    tubid <- encodeBase32Unpadded <$> Gen.bytes (Range.singleton 32)
    -- XXX 32 bytes?
    swissnum <- encodeBase32Unpadded <$> Gen.bytes (Range.singleton 32)
    let location = "@tcp:"
    pure $ "pb://" <> tubid <> location <> "/" <> swissnum

-- | Generate ByteStrings where at least one bit is non-zero.
nonZeroBytes :: MonadGen m => Range.Range Int -> m BS.ByteString
nonZeroBytes = Gen.filterT (BS.any (/= 0)) . Gen.bytes

-- | Represent ways we know to screw up a capability, share list pair.
data ShareBitFlips hash
    = -- | Flip some bits in the fingerprint in the capability.
      FingerprintBitFlips BS.ByteString
    | -- | Flip some bits in the "needed hashes" in the shares.
      ShareTreeLeafBitFlips [[BS.ByteString]]
    | -- | Flip some bits in some blocks of the shares.
      BlockBitFlips [[BS.ByteString]]
    | -- | Flip some bits in the crypttext hash tree.
      CrypttextTreeLeafBitFlips [MerkleTree Crypttext hash]
    deriving (Show)

{- | Generate instructions for flipping some bits in the fingerprint or a verify
 capability.
-}
fingerprintBitFlipper :: MonadGen m => Reader -> m (ShareBitFlips hash)
fingerprintBitFlipper reader = do
    FingerprintBitFlips <$> (Gen.bytes . Range.singleton . BS.length) (view (verifier . fingerprint) reader)

-- | Choose a function to run on a value based on a boolean.
conditionally :: (a -> b) -> (a -> b) -> Bool -> a -> b
conditionally f g p x = if p then f x else g x

-- | Generate instructions for flipping some bits in some blocks.
blockBitFlipper :: forall m hash. MonadGen m => Parameters -> [Share] -> m (ShareBitFlips hash)
blockBitFlipper Parameters{paramRequiredShares, paramTotalShares} shares' = do
    -- Pick the shares the blocks of which will be modified.
    whichShares <- enoughModifiedShares paramRequiredShares paramTotalShares

    -- Make up some masks to do the block modification.
    masks <- zipWithM (conditionally maskForShare (pure . zerosForShare)) whichShares shares'
    pure $ BlockBitFlips masks
  where
    -- Replace all the Word8s in the share blocks with 0s.
    zerosForShare :: Share -> [BS.ByteString]
    zerosForShare = (LBS.toStrict <$>) . (LBS.map (const 0) <$>) . view blocks

    maskForShare :: Share -> m [BS.ByteString]
    maskForShare = go . view blocks
      where
        go :: [LBS.ByteString] -> m [BS.ByteString]
        go = mapM (nonZeroBytes . Range.singleton . fromIntegral @Int64 @Int . LBS.length)

{- | Generate flags indicating which shares should be modified in order to
 make the whole set unusable.
-}
enoughModifiedShares :: MonadGen m => Required -> Total -> m [Bool]
enoughModifiedShares required total = do
    -- How many will we actually flip bits in?
    numSharesToModify <- Gen.integral $ Range.linear minSharesToModify maxSharesToModify

    -- Which shares will we flip bits in?  Each element corresponds to a share
    -- and tells us whether to modify it or not.
    Gen.shuffle $ (< numSharesToModify) <$> [0 .. total - 1]
  where
    -- What is the fewest number of shares we need to flip bits in?
    minSharesToModify = total - required + 1
    -- And the most?
    maxSharesToModify = total

-- | Execute the ShareTreeLeafBitFlips instruction on a list of shares.
applyShareBitFlips :: ShareBitFlips SHA256d -> ([Share], Reader) -> ([Share], Reader)
applyShareBitFlips (FingerprintBitFlips flips) = over (_2 . verifier . fingerprint) (BA.xor flips)
applyShareBitFlips (ShareTreeLeafBitFlips shareFlips) = first (zipWith flipLeaves shareFlips)
  where
    flipLeaves :: [BS.ByteString] -> Share -> Share
    flipLeaves leafFlips share = share{_neededHashes = zipWith flipBits (view neededHashes share) leafFlips}

    flipBits :: forall hash a. HashAlgorithm hash => (a, Digest' hash) -> BS.ByteString -> (a, Digest' hash)
    flipBits (a, x) y = (a, digestFromByteStringPartial $ BA.xor x y)
applyShareBitFlips (BlockBitFlips blockFlips) = first (zipWith flipBlocks blockFlips)
  where
    flipBlocks :: [BS.ByteString] -> Share -> Share
    flipBlocks masks s@Share{_blocks} = s{_blocks = LBS.fromStrict <$> zipWith BA.xor (LBS.toStrict <$> _blocks) masks}
applyShareBitFlips (CrypttextTreeLeafBitFlips hashFlips) = first (zipWith flipHashes hashFlips)
  where
    flipHashes :: MerkleTree Crypttext SHA256d -> Share -> Share
    flipHashes masks = over crypttextHashTree (makeTreePartial . zipWith flipLeafHashes (leafHashes masks) . leafHashes)

    flipLeafHashes :: forall a. HashAlgorithm a => Digest' a -> Digest' a -> Digest' a
    flipLeafHashes mask leaf =
        -- Should not fail since we're turning a Digest into bytes and then
        -- the same number of bytes back into a Digest, but hard to prove.
        digestFromByteStringPartial @a $ BA.xor mask leaf

{- | Generate instructions for making changes to the given list of shares so
     that some bits in the hashes needed to validate the merkle path to each
     share's "share root hash" are flipped.  The modified list will have the
     same length as the input list with fewer than paramRequiredShares
     elements unmodified so that they surely cannot be decoded.
-}
shareTreeLeafBitFlipper :: MonadGen m => Parameters -> [Share] -> m (ShareBitFlips hash)
shareTreeLeafBitFlipper Parameters{paramRequiredShares, paramTotalShares} shares' = do
    modifyShare <- enoughModifiedShares paramRequiredShares paramTotalShares

    -- Modify the shares to modify, leave the rest alone.
    ShareTreeLeafBitFlips <$> zipWithM modifiedShare modifyShare shares'
  where
    bytesInMask = fromIntegral $ hashDigestSize (undefined :: SHA256d)
    zeroMask = BS.replicate bytesInMask 0

    modifiedShare :: MonadGen m => Bool -> Share -> m [BS.ByteString]
    modifiedShare False Share{_neededHashes} = pure $ replicate (length _neededHashes) zeroMask
    modifiedShare True Share{_neededHashes} = do
        let -- We have to change *something*
            minHashesToModify = 1
            -- We might change everything
            maxHashesToModify = length _neededHashes

        -- Now choose how many we will change.
        numHashesToModify <- Gen.integral $ Range.linear minHashesToModify (maxHashesToModify - 1)
        -- And which ones
        modifyHash <- Gen.shuffle $ (< numHashesToModify) <$> [0 .. length _neededHashes - 1]

        mapM modifiedHash modifyHash

    modifiedHash :: MonadGen m => Bool -> m BS.ByteString
    modifiedHash False = pure zeroMask
    modifiedHash True = nonZeroFlips
      where
        -- Flip up to and including every bit of the input.
        flips = Gen.bytes $ Range.singleton bytesInMask
        -- Filter out the mask with no bits set, which would result in no bit flips.
        nonZeroFlips = Gen.filterT (/= zeroMask) flips

{- | Generate instructions for making changes to the given list of shares so
 that some bits in the "crypttext hash tree" leaves are flipped.  The
 modified list will have the same length as the input list with fewer than
 paramRequiredShares elements unmodified so that they surely cannot be
 decoded.
-}
crypttextTreeLeafBitFlipper :: forall m. MonadGen m => Parameters -> [Share] -> m (ShareBitFlips SHA256d)
crypttextTreeLeafBitFlipper Parameters{paramRequiredShares, paramTotalShares} shares' = do
    -- Pick the shares the crypttext hash trees of which will be modified.
    whichShares <- enoughModifiedShares paramRequiredShares paramTotalShares

    -- Make up some masks to do the block modification.
    masks <- zipWithM (conditionally maskForShare (pure . zerosForShare)) whichShares shares'
    pure $ CrypttextTreeLeafBitFlips masks
  where
    -- Replace all the Word8s in the hashes with 0s.
    zerosForShare :: Share -> MerkleTree a SHA256d
    zerosForShare share = makeTreePartial $ zero <$ leafHashes (view crypttextHashTree share)

    maskForShare :: Share -> m (MerkleTree Crypttext SHA256d)
    maskForShare = go . view crypttextHashTree
      where
        go :: MerkleTree a SHA256d -> m (MerkleTree a SHA256d)
        go = fmap makeTreePartial . mapM nonZeroDigest . leafHashes

        nonZeroDigest :: forall a. HashAlgorithm a => Digest' a -> m (Digest' a)
        nonZeroDigest _ = digestFromByteStringPartial <$> nonZeroBytes (Range.singleton (hashDigestSize @a undefined))

{- | Make a @Digest'@ out of a @BS.ByteString@ of the right length.  If the
 length is wrong, error.
-}
digestFromByteStringPartial :: HashAlgorithm hash => BS.ByteString -> Digest' hash
digestFromByteStringPartial =
    maybe
        (error "digestFromByteStringPartial could not construct Digest")
        Digest'
        . digestFromByteString
