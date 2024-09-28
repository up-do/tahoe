{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

--
-- Glossary
--
-- CHK - An encryption and encoding scheme for storing immutable data.
--
-- Data - The plaintext used to construct CHK.
--
-- Share - One complete unit of encrypted and FEC encoded data.
--
-- Segment - One piece of ciphertext used to construct CHK.  All segments
--     belonging to a CHK are the same size except the last one may be short.
--
-- k, required - The number of "primary" erasure-encoding outputs.  Equal to
--     the minimum number of erasure-encoding outputs needed to reconstruct
--     the erasure-encoding input.
--
-- n, total - The total number of erasure-encoding outputs.  Always greater
--     than or equal to required.
--
-- Block - One output resulting from erasure-encoding one segment using
--     required, total.  If necessary, the input segment is nul-padded so its
--     size is a multiple of required.
--
-- Plaintext Hash Tree - Not actually implemented by Tahoe so I'm not really
--     sure.  Probably something like a sha256d merkle tree where the leaves
--     are hashes of the plaintext corresponding to each ciphertext segment.
--     Since all shares for a CHK are derived from the same plaintext, every
--     share has the same plaintext hash tree.
--
-- Crypttext Hash Tree - A sha256d merkle tree where the leaves are hashes of
--     the ciphertext segments.  Since all shares for a CHK are derived from
--     the same ciphertext, every share has the same ciphertext hash tree.
--
-- Crypttext Root Hash - The hash at the root of Crypttext Hash Tree.
--
-- Block Hash Tree - A sha256d merkle tree where the leaves are hashes of the
--     blocks.  Since the erasure-encoding output is different for each share,
--     every share has a different block hash tree.
--
-- Share Hash Tree - A sha256d merkle tree where the leaves are the root
--     hashes of the block hash trees for all shares.
--
-- Share Hashes - A list of hashes from the Share Hash Tree which are required
--     to verify one block hash tree.  Each share includes the Share Hashes
--     required to verify the Block Hash Tree contained within that share.
--     Since every share contains a different Block Hash Tree, every share
--     contains a different list of Share Hashes.  Each Share Hash in this
--     list is accompanied by information about its position in the Share Hash
--     Tree though it may not be strictly required (since it could be inferred
--     from position in the list).
--
-- URI Extension - A collection of metadata describing the encryption and
--     encoding used to create the CHK and (largely) necessary for either
--     decoding or verifying the integrity of the contained data.

module Tahoe.CHK (
    zfec,
    zunfec,
    encode,
    decode,
    padCiphertext,
    segmentCiphertext,
    DecodeError (..),
) where

import qualified Codec.FEC as ZFEC
import Control.Applicative (Alternative (empty))
import Control.Lens (view)
import Crypto.Cipher.AES (AES128)
import Crypto.Hash (
    Context,
    HashAlgorithm,
    hashFinalize,
    hashInit,
    hashUpdate,
 )
import Data.Bifunctor (Bifunctor (bimap), first, second)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (foldlM)
import Data.Int (Int64)
import Data.List (partition, sort, transpose)
import Data.List.Extra (snoc)
import Data.Maybe (fromJust, mapMaybe)
import Data.Word (Word64)
import qualified Tahoe.CHK.Capability as Cap
import Tahoe.CHK.Cipher (Key)
import Tahoe.CHK.Crypto (
    blockHash',
    ciphertextSegmentHash',
    ciphertextTag,
    uriExtensionHash,
 )
import Tahoe.CHK.Merkle (
    MerkleTree,
    buildTreeOutOfAllTheNodes,
    leafHashes,
    leafNumberToNodeNumber,
    makeTreePartial,
    neededHashes,
    rootHash,
 )
import Tahoe.CHK.SHA256d (Digest' (Digest'), zero)
import Tahoe.CHK.Share (Share (..), crypttextHashTree, uriExtension)
import Tahoe.CHK.Types (
    BlockHash,
    CrypttextHash,
    Parameters (..),
    Required,
    requiredToInt,
    totalToInt,
 )
import Tahoe.CHK.URIExtension (
    URIExtension (..),
    codecParams,
 )
import Tahoe.CHK.Validate (
    matchingBlockHashRoot,
    matchingCrypttextHashRoot,
    shareValidBlocks,
    validFingerprint,
    validSegments,
    validShareRootHash,
 )
import Tahoe.Netstring (
    netstring,
 )
import Tahoe.Util (
    ceilDiv,
    chunkedBy,
    nextMultipleOf,
    nextPowerOf,
 )

-- | Erasure encode some bytes using ZFEC.
zfec ::
    -- | The number of outputs that will be required to reverse the encoding.
    -- Also known as `k`.
    Int ->
    -- | The total number of outputs to produce.  Also known as `n`.
    Int ->
    -- | Application data to divide into encoding inputs.
    B.ByteString ->
    -- | `n` encoding outputs.
    IO [B.ByteString]
zfec k n segment =
    pure $ chunks ++ ZFEC.encode (ZFEC.fec k n) chunks
  where
    chunks_ = chunkedBy (B.length segment `div` k) segment
    _msg =
        "zfec"
            <> " k="
            <> show k
            <> " n="
            <> show n
            <> ", segment len "
            <> show (B.length segment)
            <> ", chunk lengths "
            <> show (map B.length chunks_)
            <> ", segment "
            <> show segment
            <> "-> chunks "
            <> show chunks_
    chunks = {- trace _msg -} chunks_

-- | Version of `zfec` that operates on lazy ByteStrings.
zfecLazy :: Int -> Int -> LB.ByteString -> IO [LB.ByteString]
zfecLazy k n segment = (LB.fromStrict <$>) <$> zfec k n (LB.toStrict segment)

-- | Erasure decode some bytes using ZFEC.
zunfec ::
    -- | The `k` parameter used when encoding the data to decode.
    Int ->
    -- | The `n` parameter used when encoding the data to decode.
    Int ->
    -- | The encoding outputs annotated with their position (or "share number").
    [(Int, B.ByteString)] ->
    -- | The bytes which were originally encoded.
    IO B.ByteString
zunfec k n blocks = pure $ B.concat (ZFEC.decode (ZFEC.fec k n) blocks)

-- | Version of `zunfec` that operates on lazy ByteStrings.
zunfecLazy :: Int -> Int -> [(Int, LB.ByteString)] -> IO LB.ByteString
zunfecLazy k n blocks = do
    segment_ <- LB.fromStrict <$> zunfec k n (second LB.toStrict <$> blocks)
    let _msg =
            "zunfec"
                <> " k="
                <> show k
                <> " n="
                <> show n
                <> " blocks="
                <> show blocks
                <> " -> segment "
                <> show segment_

    -- pure (trace _msg) segment_
    pure segment_

{- | Represent progress encoding some ciphertext into a CHK share.  This
 carries along intermediate hash values used at the end to build extra
 self-authenticating fields into the share.
-}
data EncodingState hash = CPState
    { -- A single hash of all crypttext segments encoded so far.
      cpCrypttextHash :: Crypto.Hash.Context hash
    , -- A list of hashes of each ciphertext segment encoded so far
      cpCrypttextHashes :: [CrypttextHash hash]
    , -- Hashes of blocks encoded so far.
      cpBlockHashes :: [[BlockHash hash]]
    , -- Blocks encoded so far.
      cpBlocks :: [[LB.ByteString]]
    }

-- | The initial state for CHK encoding.
initEncodingState :: forall hash. (HashAlgorithm hash) => EncodingState hash
initEncodingState =
    CPState
        { cpCrypttextHash = hashUpdate (hashInit @hash) (netstring ciphertextTag)
        , cpCrypttextHashes = mempty
        , cpBlockHashes = mempty
        , cpBlocks = mempty
        }

{- | Split a full ciphertext string into the separate ciphertext segments
 required by most of CHK encoding.
-}
segmentCiphertext ::
    -- | The encoding parameters which determine how to split the ciphertext.
    Parameters ->
    -- | The ciphertext.
    LB.ByteString ->
    -- | The segments.
    [LB.ByteString]
segmentCiphertext Parameters{paramSegmentSize} ciphertext =
    result
  where
    result = {- trace ("segmentCiphertext: " <> show ciphertext) -} result_
    result_ = LB.fromStrict <$> chunkedBy (fromIntegral paramSegmentSize) (LB.toStrict ciphertext)

{- | Process ciphertext into blocks, carrying hashes computed along the way as
 state.
-}
processCiphertext :: forall hash. (HashAlgorithm hash) => Parameters -> [LB.ByteString] -> IO (EncodingState hash)
processCiphertext Parameters{paramRequiredShares, paramTotalShares} =
    foldlM processSegment (initEncodingState @hash)
  where
    processSegment CPState{..} segment = do
        -- Produce the FEC blocks for this piece of ciphertext.
        blocks <-
            zfecLazy
                (requiredToInt paramRequiredShares)
                (totalToInt paramTotalShares)
                (padCiphertext paramRequiredShares segment)
        pure $
            CPState
                { cpCrypttextHash = hashUpdate cpCrypttextHash (LB.toStrict segment)
                , cpCrypttextHashes = snoc cpCrypttextHashes (ciphertextSegmentHash' (LB.toStrict segment))
                , cpBlockHashes = snoc cpBlockHashes (blockHash' . LB.toStrict <$> blocks)
                , cpBlocks = snoc cpBlocks blocks
                }

-- Compute the correctly padded ciphertext.  The only ciphertext which is
-- expected to require padding is the final segment - in case the original
-- ciphertext did not have a length that was a multiple of the `required`
-- parameter.
--
-- allmydata.immutable.encode.Encoder._gather_data NUL pads up to num_chunks
-- times input_chunk_size.  num_chunks is our requiredShares.
-- input_chunk_size is taken from codec.get_block_size() which returns
-- codec.share_size.  share_size is div_ceil(data_size, required_shares).
-- data_size is our segmentSize and required_shares is our requiredShares.
padCiphertext :: Required -> LB.ByteString -> LB.ByteString
padCiphertext requiredShares bs
    | paddingLength > 0 = bs <> LB.replicate paddingLength 0x00
    | otherwise = bs
  where
    desiredLength = nextMultipleOf requiredShares (LB.length bs)
    paddingLength = desiredLength - LB.length bs

{- | Encode some application data (typically ciphertext, but this function only
 weakly assumes this is the case) into some CHK shares.

 This replaces much of allmydata.immutable.encode.
-}
encode ::
    -- | The encryption/decryption key.
    Key AES128 ->
    -- | The ZFEC parameters for this encoding.  This determines how many shares
    -- will come out of this function.
    Parameters ->
    -- | The data to encode.  This is typically ciphertext.
    LB.ByteString ->
    -- | An IO which can be evaluated to get the encoded share data and the
    -- read capability.  The number of Shares will equal the `total` value
    -- from the given Parameters.
    IO ([Share], Cap.Reader)
encode readKey initParams@(Parameters maximumSegmentSize total _ required) ciphertext =
    processCiphertext p (segmentCiphertext p ciphertext) >>= \CPState{..} ->
        let
            -- The number of segments encoded in the share.  There are the same number
            -- of plaintext and ciphertext segments and this is also the number of
            -- blocks in each share (though each share may have a different _value_
            -- for each block).
            --
            -- allmydata.immutable.encode.Encoder._got_all_encoding_parameters
            numSegments = length cpBlocks

            -- Our merkle trees need a number of leaves equal to a power of 2.
            -- Compute that here so we can pad as necessary.
            --
            -- allmydata.immutable.layout.WriteBucketProxy
            effectiveSegments = nextPowerOf 2 numSegments

            -- XXX Unused by Tahoe so we don't even try for a sensible value right
            -- now.  Just fill it with zeros.
            --
            -- As long as we calculate a valid number of nodes for a tree
            -- buildTreeOutOfAllTheNodes won't give us a Nothing back ... cross
            -- your fingers.
            Just plaintextHashTree =
                buildTreeOutOfAllTheNodes
                    -- We have to fill the *whole* tree with nul, not just the
                    -- leaves.  Compute the total number of nodes in a tree that
                    -- can hold our number of segments.
                    . replicate (2 * effectiveSegments - 1)
                    -- And make every node all nul.
                    $ zero

            -- shareTree is a MerkleTree of MerkleTree
            shareTree =
                -- trace ("shareTree: " <> show shareTree')
                shareTree'
              where
                shareTree' = makeShareTree . map makeTreePartial . transpose $ cpBlockHashes

            -- A bag of additional metadata about the share and encoded object.
            uriExt =
                URIExtension
                    { _codecName = "crs"
                    , _codecParams = p -- trace ("Params: " <> show p) p
                    , _size = fromIntegral $ LB.length ciphertext
                    , _segmentSize = segmentSize
                    , _neededShares = required
                    , _totalShares = total
                    , _numSegments = numSegments
                    , _tailCodecParams = tailParams p (LB.length ciphertext)
                    , _crypttextHash = makeCrypttextHash cpCrypttextHash
                    , _crypttextRootHash = makeCrypttextRootHash cpCrypttextHashes
                    , _shareRootHash = rootHash shareTree
                    }

            -- The read capability for the encoded object.
            cap =
                Cap.makeReader
                    readKey
                    (uriExtensionHash uriExt)
                    required
                    total
                    (fromIntegral $ LB.length ciphertext)

            toShare sharenum blocks blockHashes =
                Share
                    { _blockSize = shareBlockSize p
                    , _dataSize = fromIntegral $ LB.length ciphertext `ceilDiv` fromIntegral required
                    , _blocks = blocks
                    , _plaintextHashTree = plaintextHashTree
                    , _crypttextHashTree = makeTreePartial cpCrypttextHashes
                    , _blockHashTree = makeTreePartial blockHashes
                    , _neededHashes = sort . fmap (first fromIntegral) $ computeNeededHashes shareTree sharenum
                    , _uriExtension = uriExt
                    }

            -- The size in bytes of one erasure-encoded block of data.
            -- allmydata.immutable.encode.Encoder._got_all_encoding_parameters +
            -- allmydata.codec.CRSEncoder.set_params
            shareBlockSize :: Parameters -> Word64
            shareBlockSize Parameters{paramSegmentSize, paramRequiredShares} =
                fromIntegral paramSegmentSize `ceilDiv` fromIntegral paramRequiredShares
         in
            pure
                ( zipWith3 toShare [0 ..] (transpose cpBlocks) (transpose cpBlockHashes)
                , cap
                )
  where
    -- If we have little enough ciphertext, the maximum configured segment
    -- size may be greater than the length of the single segment we produce.
    -- Segment size is also required to be a multiple of the number of
    -- required shares so that segments can be evenly divided across the
    -- shares.
    p@(Parameters segmentSize _ _ required') =
        initParams
            { paramSegmentSize = nextMultipleOf required' $ min maximumSegmentSize (fromIntegral $ LB.length ciphertext)
            }

-- | A problem was encountered during decoding.
data DecodeError
    = -- | The size of the data is greater than the limits imposed by this implementation.
      SizeOverflow
    | -- | There weren't enough shares supplied to attempt erasure decoding.
      NotEnoughShares
    | -- | After discarding shares for which the fingerprint from the read
      -- | capability did not match the URI extension block, there weren't
      -- | enough shares left to attempt erasure decoding.
      IntegrityError
        { integrityErrorInvalidShares :: [(Int, Share, InvalidShare)]
        }
    | -- | The hash of one or more blocks did not match the expected value.
      BlockHashError
    | -- | The hash of one or more ciphertext segments did not match the expected value.
      CiphertextHashError
    deriving (Eq, Ord, Show)

{- | Decode some CHK shares to recover some application data.  This is roughly
 the inverse of ``encode``.
-}
decode ::
    -- | The read capability for the application data.
    Cap.Reader ->
    -- | At least as many shares as are required to erasure decode the
    -- ciphertext.
    [(Int, Share)] ->
    -- | An action that results in Right of the ciphertext contained by the
    -- shares if it is possible to recover it, or Left with information about
    -- why it is not.
    IO (Either DecodeError LB.ByteString)
decode reader shares
    | size reader > fromIntegral @Int64 @Integer maxBound = pure $ Left SizeOverflow
    | length shares < fromIntegral (required reader) = pure $ Left NotEnoughShares
    | length validShares < fromIntegral (required reader) = pure . Left . IntegrityError $ invalidShares
    | otherwise = do
        let
            -- The ZFEC decoder takes as input a list of (share number, block
            -- bytes) tuples (and the encoding parameters).  It wants the list
            -- to contain *exactly* `k` distinct blocks.  Our job is to give
            -- it these such a list, then.  If there were shares with metadata
            -- that disqualified them from use we have already discarded them.
            -- We have not yet verified the integrity of the actual blocks so
            -- we will do so now.
            --
            -- It could be that we initially appear to have some extra data
            -- available (more than `k` shares) but then discover that *some*
            -- blocks are invalid.  If we can disqualify *blocks* for being
            -- invalid rather than disqualifying entire shares then we will be
            -- able to recover data in more situations so we will try to do
            -- that.

            -- Start by annotating every block of every share with a boolean
            -- of whether its hash matches the good hash from the block hash
            -- tree.  The outer list gives a share number along with that
            -- share's data.  Each inner list gives a validated block or
            -- nothing if validation failed.
            blocksWithValidity :: [[(Int, Maybe LB.ByteString)]]
            blocksWithValidity = fixBlocks . second shareValidBlocks <$> validShares

            -- Change the container structure.  The outer list corresponds to
            -- erasure-encoded segments.  The order corresponds to the order
            -- of the segments from the original input.  Each inner list
            -- contains the blocks we were able to validate for that segment.
            explodedBlocks :: [[(Int, Maybe LB.ByteString)]]
            explodedBlocks = transpose blocksWithValidity

            -- Then filter down to only the validated blocks.
            validBlocks :: [[(Int, LB.ByteString)]]
            validBlocks = mapMaybe (\(num, mbs) -> (num,) <$> mbs) <$> explodedBlocks

            -- If we end up with fewer than `required` blocks for any
            -- particular segment, we cannot decode that segment.  Throw out
            -- the data we cannot use and structure what's left so we can
            -- easily skip over those segments if desired.
            enoughBlocks :: [Maybe [(Int, LB.ByteString)]]
            enoughBlocks = guarded ((fromIntegral (required reader) <=) . length) <$> validBlocks

            -- Figure out how many bytes are expected to be in each segment.
            -- Depending on the ZFEC encoding parameters, it is possible that
            -- we will end up with blocks that are not completely "filled"
            -- with real data.  When these are decoded, we will get _extra_
            -- bytes in the result.  By knowing how many bytes were originally
            -- in our segments, we can recognize and discard these extra
            -- bytes.
            segSize = paramSegmentSize . view (uriExtension . codecParams) $ anyValidShare

            -- The final segment might be short.  Find out.  Note we don't
            -- read the segment size from the tail codec params in the
            -- URIExtension because that *includes* padding and we're trying
            -- to *exclude* padding.  Instead we compute the result from the
            -- real application data size and the non-tail segment size.
            tailSegSize = case size reader `mod` segSize of
                0 -> segSize
                n -> n

            -- A helper that knows the correct parameters to do ZFEC decoding
            -- for us.
            --
            -- XXX Do we need this LB.take at the front?  Shouldn't each block
            -- be segSize bytes in length anyway (disregarding the tail
            -- segment, which we're not doing anything to handle here anyway)?
            -- We chunked the bytes up in to blocks, we know how big they are.
            -- But we chunked them based on `_blockSize` from the share, not
            -- `segSize` from the codec params.  Perhaps if we validated those
            -- are consistent then we could be confident of consistency here
            -- w/o the LB.take.
            zunfec' = (LB.take (fromIntegral segSize) <$>) . zunfecLazy (fromIntegral (required reader)) (fromIntegral (total reader))

            -- Get ready to decode the groups of blocks back to the original
            -- segments, where this is possible.  We might have even more than
            -- we need at this point so be sure to discard any extras so
            -- zunfec doesn't grow angry.
            getSegments :: [Maybe (IO LB.ByteString)]
            getSegments = fmap (zunfec' . take (fromIntegral (required reader)) <$>) enoughBlocks

        -- Actually do it
        maybeSegments <- traverse sequence getSegments :: IO [Maybe LB.ByteString]

        pure $ do
            -- This function produces a monolithic result - everything or nothing.
            -- So change the structure from "results and errors for individual
            -- blocks" to "a result or an error from somewhere".  A function with
            -- an incremental result interface could consider just completing with
            -- `segments` from above.  Or perhaps further transforming it to
            --
            --   (Traversable t, Functor f) => t (IO (f LB.ByteString))
            segments <- maybe (Left BlockHashError) Right (sequence maybeSegments)

            -- Now check the validity of the segments themselves against the
            -- crypttext hash tree.
            let maybeValidSegments =
                    validSegments
                        (leafHashes $ view crypttextHashTree anyValidShare)
                        -- Take care to validate the tail segment *without* padding.
                        (LB.toStrict <$> trimTailSegment (fromIntegral tailSegSize) segments)

            maybe
                -- Signal overall failure if any segments were excluded by the previous step.
                (Left CiphertextHashError)
                -- Combine the segments to produce the complete result if they all check out.
                (Right . LB.concat . (LB.fromStrict <$>))
                -- Get rid of any segments which do not agree with the hashes
                -- in the crypttext hash tree.
                (sequence maybeValidSegments)
  where
    -- Separate the shares into those we can use and those we cannot.
    --
    -- Make the list pattern match lazy (with `~`) in case there are *no*
    -- valid shares.  The guard above will check if there are any valid shares
    -- before we need to match that part of the pattern.  This lets us bind a
    -- name to some valid share which is helpful inside the body of the guard
    -- where we need to read some value that is shared across all shares.
    (validShares@(~((_, anyValidShare) : _)), invalidShares) = partitionShares (view Cap.verifier reader) shares

    -- Project the share number out across all of that share's blocks.  The
    -- result is something we can transpose into the correct form for ZFEC
    -- decoding.
    fixBlocks :: (Int, [a]) -> [(Int, a)]
    fixBlocks (sharenum, bs) = map (sharenum,) bs

    size = view (Cap.verifier . Cap.size)
    required = view (Cap.verifier . Cap.required)
    total = view (Cap.verifier . Cap.total)

    -- Return a list like the one given except that the last element is
    -- shortened to the given length.
    trimTailSegment :: Int64 -> [LB.ByteString] -> [LB.ByteString]
    trimTailSegment segSize = mapLast (LB.take segSize)

    -- Apply a function to the last element of a list, if there is one.
    mapLast _ [] = []
    mapLast f [x] = [f x]
    mapLast f (x : xs) = x : mapLast f xs

-- | Give a reason a share is considered invalid.
data InvalidShare
    = -- | The fingerprint in the capability does not match the fingerprint of the share.
      FingerprintMismatch
    | -- | The values in the share for the block hash tree root and the
      -- share's own entry in "needed shares" do not match.
      BlockHashRootMismatch
    | -- | The "share root hash" in the share's URIExtension doesn't agree
      -- with the root hash constructed from the "block hash tree" roots in
      -- the share's "needed shares" value.
      ShareRootHashInvalid
    | -- | The "crypttext root hash" in the share's URIExtension doesn't agree
      -- | with the root hash constructed from the "crypttext hash tree"
      -- | hashes in the share.
      CrypttextHashRootMismatch
    deriving (Ord, Eq, Show)

{- | Split a list of shares into those which pass all of the validation checks
 and those which do not.
-}
partitionShares :: Cap.Verifier -> [(Int, Share)] -> ([(Int, Share)], [(Int, Share, InvalidShare)])
partitionShares verifier shares =
    ( validShares
    , map (`err` FingerprintMismatch) haveInvalidFingerprint
        ++ map (`err` BlockHashRootMismatch) haveInvalidBlockHashRoot
        ++ map (`err` ShareRootHashInvalid) haveInvalidShareRootHash
        ++ map (`err` CrypttextHashRootMismatch) haveMismatchingCrypttextHashRoot
    )
  where
    -- Helper to build our error structure
    err = uncurry (,,)

    -- The hash of the UEB must equal the fingerprint in the capability.
    (haveValidFingerprint, haveInvalidFingerprint) = partition (validFingerprint verifier . snd) shares

    -- The root of the share block tree in the share body must equal the
    -- share's hash in the "needed hashes" merkle proof.
    (haveValidBlockHashRoot, haveInvalidBlockHashRoot) = partition (uncurry matchingBlockHashRoot) haveValidFingerprint

    (haveMatchingCrypttextHashRoot, haveMismatchingCrypttextHashRoot) = partition (matchingCrypttextHashRoot . snd) haveValidBlockHashRoot

    -- The "needed hashes" merkle proof must be valid with respect to the "share root hash" in the UEB.
    shareRootValidations = zip (validShareRootHash stillValid) stillValid
      where
        stillValid = haveMatchingCrypttextHashRoot
    (haveValidShareRootHash, haveInvalidShareRootHash) = bimap (snd <$>) (snd <$>) $ partition fst shareRootValidations

    validShares = haveValidShareRootHash

{- | Build a merkle tree where the leaves are the root hashes of the block
 hash tree of each share.
-}
makeShareTree :: (HashAlgorithm hash) => [MerkleTree B.ByteString hash] -> MerkleTree (MerkleTree B.ByteString hash) hash
makeShareTree = makeTreePartial . map rootHash

makeCrypttextHash :: (HashAlgorithm hash) => Context hash -> CrypttextHash hash
makeCrypttextHash = Digest' . hashFinalize

makeCrypttextRootHash :: (HashAlgorithm hash) => [CrypttextHash hash] -> CrypttextHash hash
makeCrypttextRootHash = rootHash . makeTreePartial

-- Construct the encoding parameters for the final segment which may be
-- smaller than the earlier segments (if the size of the data to be encoded is
-- not a multiple of the segment size).
-- allmydata.immutable.encode.Encoder._got_all_encoding_parameters
tailParams :: (Integral a) => Parameters -> a -> Parameters
tailParams p@Parameters{paramSegmentSize, paramRequiredShares} dataSize =
    p{paramSegmentSize = nextMultipleOf paramRequiredShares tailSize'}
  where
    tailSize' =
        if tailSize == 0
            then paramSegmentSize
            else tailSize
    tailSize = fromIntegral dataSize `mod` paramSegmentSize

{- | Determine the node numbers of the share tree which are required to verify
 the indicated share number.  The indicated share number is included in the
 result, as are the corresponding hashes from the given tree.
-}
computeNeededHashes :: MerkleTree (MerkleTree B.ByteString hash) hash -> Int -> [(Int, Digest' hash)]
computeNeededHashes shareTree sharenum =
    -- In addition to what neededHashes computes we also need to include this
    -- share's own block hash root in the result.  Shove it on the front of
    -- the result here.  This will place it out of order so we'll fix it up
    -- below when we construct the Share.  We also have to translate between
    -- zero-indexed share numbers and 1-indexed leaf numbers.
    --
    -- Is fromJust here safe?  neededHashes returns Nothing when it fails to
    -- compute a merkle proof.  Given the way we're using it, that can
    -- probably only happen if there's a bug inside neededHashes (as opposed
    -- to our passing in some value it doesn't want to provide a result for).
    (leafNumberToNodeNumber shareTree sharenum - 1, blockHashRoot shareTree sharenum) : fromJust (neededHashes shareTree sharenum)

{- | Find the block tree root hash for the nth share in the given share hash
 tree.
-}
blockHashRoot :: MerkleTree (MerkleTree B.ByteString hash) hash -> Int -> Digest' hash
blockHashRoot tree n
    | n < 0 = error "Cannot have a negative leaf number"
    | n >= length leafs = error "Leaf number goes past the end of the tree"
    | otherwise = leafs !! n
  where
    leafs = leafHashes tree

-- | Conditionally lift a value into a context.
guarded :: (Alternative f) => (a -> Bool) -> a -> f a
guarded predicate value
    | predicate value = pure value
    | otherwise = empty
