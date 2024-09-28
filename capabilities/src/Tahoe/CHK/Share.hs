{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

-- To read all the plaintext of a CHK share which you have enough shares for:

-- (-1). Find and download the shares
-- ( 0). Parse the share bytes into the various fields

-- 1. Check the UEB (URI Extension Block) hash
-- 2. Decode the UEB to find the share root hash
-- 3. Build the block hash tree for all shares you have
-- 4. Build the share hash tree out of those block hash tree roots combined with all of the "needed hashes" you pulled out of the shares you have
-- 5. Check the root of the share hash tree against the value in the UEB
-- 6. ZFEC decode the blocks into ciphertext **
-- 7. Check the "crypttext hash" against the hash of the ciphertext
--    (maybe helps catch a ZFEC implementation bug?)
-- 8. Decrypt the ciphertext **

-- 3 of 4
-- Have 4, 5, 6
-- neededHashes a == [ 5, 6, 7 ]

--                                     1
--                  2                                     3
--     4                    5                 6                     7
--     a                    b                 c                     d
--     ^
-- 5+"5s hash"+6+"6s hash"+7+"7s hash"

{- |
A share is a single data object comprising some erasure-encoded data and some
cryptographic hashes which allow certain determinations to be made about that
that data.  One or more shares can be interpreted together, typically to
recover a particular ciphertext object.

This modules exposes a structured representation of the share object along
with an encoder to and decoder from the canonical serialized representation.
-}
module Tahoe.CHK.Share where

import Control.Exception (Exception, throw)
import Control.Lens (makeLenses)
import Crypto.Hash (HashAlgorithm (hashDigestSize), digestFromByteString)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Binary (
    Binary (get, put),
    Word32,
    Word64,
    Word8,
    encode,
 )
import Data.Binary.Get (
    Get,
    bytesRead,
    getLazyByteString,
    isolate,
 )
import Data.Binary.Put (Put, putLazyByteString)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.List.Extra (dropEnd, sumOn')
import Data.Maybe (fromMaybe)
import Data.TreeDiff.Class (ToExpr)
import Debug.Trace ()
import GHC.Generics (Generic)
import Network.ByteOrder (bytestring32, bytestring64)
import Tahoe.CHK.Merkle (MerkleTree)
import Tahoe.CHK.SHA256d (Digest' (Digest'), SHA256d, toBytes)
import Tahoe.CHK.Types (ShareNum)
import Tahoe.CHK.URIExtension (
    URIExtension,
    pURIExtension,
    uriExtensionToBytes,
 )
import Tahoe.Util (chunkedBy, toStrictByteString)
import Text.Megaparsec (parse)

-- | A byte string of encrypted data.
type Crypttext = BS.ByteString

-- | Structured representation of a single CHK share.
data Share = Share
    { -- | The ZFEC block size.  Legacy value.  Unused.
      _blockSize :: Word64
    , -- | The share data length.  Legacy value.  Unused.
      _dataSize :: Word64
    , -- | The ZFEC encoded ciphertext blocks.
      _blocks :: [LBS.ByteString]
    , -- | A merkle tree of plaintext segment hashes.  Unimplemented.
      _plaintextHashTree :: MerkleTree BS.ByteString SHA256d
    , -- | A merkle tree of ciphertext segment hashes.
      _crypttextHashTree :: MerkleTree Crypttext SHA256d
    , -- | A merkle tree of hashes of `shareBlocks`.
      _blockHashTree :: MerkleTree BS.ByteString SHA256d
    , -- | The information needed to complete a merkle proof for this share.
      _neededHashes :: [(ShareNum, Digest' SHA256d)]
    , -- | Additional metadata about this share.
      _uriExtension :: URIExtension
    }
    deriving (Eq, Ord, Show, Generic, ToExpr)

$(makeLenses ''Share)

getWord32 :: Get Word64
getWord32 = do
    word32 <- get :: Get Word32
    pure $ fromIntegral word32

getWord64 :: Get Word64
getWord64 = get

word64To4Bytes :: Word64 -> Maybe BS.ByteString
word64To4Bytes = (bytestring32 <$>) . word64ToWord32

word64To4Bytes' :: Word64 -> Either String BS.ByteString
word64To4Bytes' w =
    case word64To4Bytes w of
        Nothing -> Left "Word64 out of bounds in conversion to Word32"
        Just bs -> pure bs

word64To8Bytes :: Word64 -> BS.ByteString
word64To8Bytes = bytestring64

instance Binary Share where
    -- Serialize a share to its canonical byte representation.  This replaces
    -- much of allmydata.immutable.layout.
    put
        Share{..} =
            let -- shareDataSize is supposedly unused.  Avoid making any
                -- calculations based on its value.  We'll serialize it into
                -- the output but otherwise we should ignore it.  Instead,
                -- we'll use this computed value that's consistent with the
                -- rest of our data.
                --
                -- CRSEncoder.set_params
                realSize = sumOn' LBS.length _blocks

                -- Pick a share format version based on the size of our data,
                -- along with helpers to encoding our fields for that format
                -- version.
                --
                -- Okay we won't completely ignore shareDataSize.  We can't
                -- encode sufficiently large values into a v1 format share so
                -- switch to v2 format if shareDataSize needs it.
                --
                -- Tahoe also checks blockSize < 2 ^ 32 but I don't see how it is
                -- possible for blockSize to be greater than dataSize.
                (version, encodeWord, putWord) = chooseVersion $ max _dataSize (int64ToWord64 realSize)

                -- This excludes the version but otherwise has all of the integer
                -- header fields we need to write.
                header =
                    [ _blockSize
                    , _dataSize
                    , (fromIntegral :: Int -> Word64) headerSize
                    ]
                        <> trailerFieldOffsets

                -- Compute the header size so we can include it in the offset
                -- calculation.  The header is the 4 byte version field and then some
                -- additional number of integer fields.  Each subsequent integer field
                -- is either 4 or 8 bytes depending on the share version.
                headerSize = 4 + fieldSizeForVersion version * length header

                -- Then compute the offset of each piece of the trailer.  They all
                -- follow the header and all of the share blocks so start there and
                -- advance by the size of each trailer piece.
                trailerOffset = (fromIntegral :: Int -> Word64) headerSize + int64ToWord64 realSize

                -- The scanl would calculate the offset of the field following the
                -- last field - which we don't need or want.  So drop the last size.
                trailerFieldOffsets = scanl (+) trailerOffset (dropEnd 1 trailerFieldSizes)

                -- We need to write offets to trailer fields into the header.  Compute
                -- the size of each trailer piece so we know how they'll be laid out.
                trailerFieldSizes = map (int64ToWord64 . LBS.length) trailerFields

                -- Construct all of the trailing metadata here so we know how
                -- big each piece of it is.  We need to put offsets pointing
                -- at this data into the header.  Keep in mind that nearby
                -- code assumes this list contains one element for each
                -- trailer field which has an offset recorded in the header.
                -- That code will produce an incorrect header if this
                -- assumption is violated.
                ueb = uriExtensionToBytes _uriExtension
                trailerFields =
                    [ encode _plaintextHashTree
                    , encode _crypttextHashTree
                    , encode _blockHashTree
                    , LBS.fromStrict $ serializeNeededShares _neededHashes
                    , LBS.fromStrict $ encodeWord (intToWord64 $ BS.length ueb) <> ueb
                    ]
             in do
                    put (fromIntegral version :: Word32)
                    mapM_ putWord header
                    mapM_ putLazyByteString _blocks
                    mapM_ putLazyByteString trailerFields

    get = do
        -- Read the version marker to determine the size of certain following
        -- fields.
        (_version, getWord) <- getVersion -- 0, 1
        _blockSize <- getWord -- 4, 1
        _dataSize <- getWord -- 8, 1

        -- These offsets are all relative to the beginning of the share.
        dataOffset <- getWord -- 12, 36
        plaintextHashTreeOffset <- getWord -- 16, 37
        crypttextHashTreeOffset <- getWord -- 20, 69
        blockHashesOffset <- getWord -- 24, 101
        shareHashesOffset <- getWord -- 28, 133
        uriExtensionLengthOffset <- getWord -- 32, 167

        -- Load the rest of the fields in the typical order.  The offsets
        -- might place these fields in a different order but they really
        -- shouldn't.  We'll fail with an explicit error in that case thanks
        -- to position checking done in getLazyByteStringInBoundsFrom.  Then
        -- we'll fail to load the share but at least we won't apply an invalid
        -- interpretation to any of the data.
        allShareBlocks <- getLazyByteStringInBoundsFrom "share blocks" dataOffset plaintextHashTreeOffset -- 36, <1 byte>
        _plaintextHashTree <- isolateBetween "plaintext hash tree" plaintextHashTreeOffset crypttextHashTreeOffset get -- 37, <69 - 37 == 32 bytes>
        _crypttextHashTree <- isolateBetween "crypttext hash tree" crypttextHashTreeOffset blockHashesOffset get -- 69, <101 - 69 == 32 bytes>
        _blockHashTree <- isolateBetween "block hash tree" blockHashesOffset shareHashesOffset get -- 101, <133 - 101 == 32 bytes>
        _neededHashes <- fromMaybe (fail "Could not parse `needed hashes`") . unserializeNeededShares . LBS.toStrict <$> getLazyByteStringInBoundsFrom "needed shares" shareHashesOffset uriExtensionLengthOffset -- 133, <167 - 133 == 34 bytes>
        uriExtensionLength <- getWord >>= getInt64FromWord64 "URI extension length" -- 167,
        uriExtensionBytes <- getLazyByteString uriExtensionLength
        _uriExtension <-
            either
                (fail . show)
                pure
                (parse pURIExtension "URI extension" $ LBS.toStrict uriExtensionBytes)

        let _blocks = segmentLazyBytes (fromIntegral _blockSize) allShareBlocks

        pure $ Share{..}

segmentLazyBytes :: Int64 -> LBS.ByteString -> [LBS.ByteString]
segmentLazyBytes _segmentSize "" = []
segmentLazyBytes segmentSize bs = nextSegment : segmentLazyBytes segmentSize theRest
  where
    (nextSegment, theRest) = LBS.splitAt segmentSize bs

isolateBetween :: String -> Word64 -> Word64 -> Get a -> Get a
isolateBetween name start end g = do
    pos <- bytesRead
    if (fromIntegral :: Int64 -> Word64) pos /= start
        then fail $ "expected to read from " <> show start <> " to get " <> name <> " but position is " <> show pos
        else isolate (fromIntegral (end - start)) g

getLazyByteStringInBoundsFrom :: String -> Word64 -> Word64 -> Get LBS.ByteString
getLazyByteStringInBoundsFrom name expectedPosition offset = do
    pos <- bytesRead
    if (fromIntegral :: Int64 -> Word64) pos /= expectedPosition
        then fail $ "expected to read from " <> show expectedPosition <> " to get " <> name <> " but position is " <> show pos
        else do
            offsetInt64 <- getInt64FromWord64 name offset
            getLazyByteString (offsetInt64 - pos)

getInt64FromWord64 :: String -> Word64 -> Get Int64
getInt64FromWord64 name = maybe (fail $ name <> " out of bounds") pure . word64ToInt64

word64ToInt64 :: Word64 -> Maybe Int64
word64ToInt64 w
    | w > maxInt64 = Nothing
    | otherwise = Just (fromIntegral w)
  where
    maxInt64 :: Word64
    maxInt64 = fromIntegral (maxBound :: Int64)

word64ToWord32 :: Word64 -> Maybe Word32
word64ToWord32 w
    | w > maxWord32 = Nothing
    | otherwise = Just (fromIntegral w)

maxWord32 :: Integral i => i
maxWord32 = fromIntegral (maxBound :: Word32)

{- | Serialize the list of (share number, block tree root hash) pairs for
 inclusion in the serialized form of a Share.  The inverse of
 unserializeNeededShares.
-}
serializeNeededShares :: HashAlgorithm hash => [(ShareNum, Digest' hash)] -> BS.ByteString
serializeNeededShares = BS.concat . pieces
  where
    pieces [] = []
    pieces ((sharenum, hash) : xs) = (toStrictByteString . BS.int16BE . fromIntegral $ sharenum) : toBytes hash : pieces xs

{- | Unserialize a a list of (share number, block tree root hash) pairs from
 their form in a serialized Share.  The inverse of serializeNeededShares.
-}
unserializeNeededShares :: forall hash. HashAlgorithm hash => BS.ByteString -> Maybe [(ShareNum, Digest' hash)]
unserializeNeededShares bs =
    traverse sequenceA result
  where
    chunks = chunkedBy (2 + hashDigestSize (undefined :: hash)) bs
    pairs = map (BS.splitAt 2) chunks
    result = bimap toShareNum (fmap Digest' . digestFromByteString) <$> pairs

    toShareNum :: BS.ByteString -> ShareNum
    toShareNum x = fromIntegral $ fromEnum msb `shiftL` 8 .|. fromEnum lsb
      where
        msb = BS.head x
        lsb = BS.last x

intToWord64 :: Int -> Word64
intToWord64 x
    | x < 0 = error "Negative Int cannot be converted to Word64"
    | otherwise = fromIntegral x

int64ToWord64 :: Int64 -> Word64
int64ToWord64 x
    | x < 0 = error "Negative Int64 cannot be converted to Word64"
    | otherwise = fromIntegral x

getVersion :: Get (Word8, Get Word64)
getVersion = do
    version <- getWord32
    pure
        ( fromIntegral version
        , case version of
            1 -> getWord32
            2 -> getWord64
            _ -> fail $ "unsupported version: " <> show version
        )

chooseVersion :: Word64 -> (Word8, Word64 -> BS.ByteString, Word64 -> Put)
chooseVersion shareDataSize =
    (version, encodeWord, putWord)
  where
    -- Version 1 can encode sizes up to 2^32 bytes.  Version 2 can encode
    -- sizes up to 2^64 bytes.  Choose a version based on the actual data
    -- size.  We only save a handful bytes of header this way so the extra
    -- complexity may not be worth it just for that but it's convenient to
    -- be able to emit either share version for testing.
    version = if shareDataSize <= maxWord32 then 1 else 2

    -- Here's where the version makes a difference to the header size.
    -- Choose an integer encoding that uses the right number of bytes.
    encodeWord
        -- word64To4Bytes can't always succeed but if we're picking version 1 then
        -- we believe it will succeed.  If it fails, we'll have to have a hard
        -- error :/ This is not ideal but ... I dunno what to do.
        | version == 1 = word64To4BytesPartial
        | version == 2 = word64To8Bytes
        | otherwise = error $ "unsupported version: " <> show version
    putWord = putLazyByteString . LBS.fromStrict . encodeWord

fieldSizeForVersion :: Word8 -> Int
fieldSizeForVersion 1 = 4
fieldSizeForVersion 2 = 8
fieldSizeForVersion n = error $ "Unsupported version number: " <> show n

{- | Serialize a Word64 to 4 bytes or throw an exception if the value can not
 fit.
-}
word64To4BytesPartial :: Word64 -> BS.ByteString
word64To4BytesPartial i = fromRight (throw $ Word64OutOfBounds i) (word64To4Bytes' i)

newtype EncodingError = Word64OutOfBounds Word64 deriving (Eq, Ord, Show)

instance Exception EncodingError
