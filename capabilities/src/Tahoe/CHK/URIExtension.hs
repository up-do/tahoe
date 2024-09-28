{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Tahoe.CHK.URIExtension (
    URIExtension (..),
    codecName,
    codecParams,
    tailCodecParams,
    size,
    segmentSize,
    numSegments,
    neededShares,
    totalShares,
    crypttextHash,
    crypttextRootHash,
    shareRootHash,
    uriExtensionToBytes,
    showBytes,
    pURIExtension,
) where

import Control.Applicative.Combinators (count)
import Control.Applicative.Permutations (runPermutation, toPermutation)
import Control.Lens (view)
import Control.Lens.TH (makeLenses)
import Control.Monad (join, void, (>=>))
import Crypto.Hash (HashAlgorithm, digestFromByteString)
import Data.TreeDiff.Class (ToExpr)
import Data.Void (Void)
import GHC.Generics (Generic)
import Tahoe.CHK.SHA256d (Digest' (Digest'), SHA256d, toBytes)

import Text.Megaparsec (
    MonadParsec (takeP),
    Parsec,
    anySingle,
 )
import Text.Megaparsec.Byte (string)
import Text.Megaparsec.Byte.Lexer (decimal)

import Data.ByteString.Base32 (
    encodeBase32Unpadded,
 )

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (
    decodeLatin1,
    encodeUtf8,
 )

import Data.List (
    sort,
 )

import qualified Tahoe.CHK.Parsing
import Tahoe.CHK.Types (
    CrypttextHash,
    Parameters (..),
    Required,
    SegmentNum,
    Size,
    Total,
 )
import Tahoe.Netstring (
    netstring,
 )

-- | Represent additional metadata that appears at the end of each share.
data URIExtension = URIExtension
    { -- | The name of the encoding function.  Only "zfec" is implemented.
      _codecName :: B.ByteString
    , -- | The parameters for the encoding function for all except the final
      -- segment.
      _codecParams :: Parameters
    , -- | The parameters for the encoding function for the final segment.
      _tailCodecParams :: Parameters
    , -- | The application data size in bytes.
      _size :: Size
    , -- | The individual segment size in bytes.
      _segmentSize :: Size
    , -- | The number of segments of application data.  Note the last segment
      -- may be short so it is not necessarily the case that uriExtSize ==
      -- uriExtSegmentSize * uriExtNumSegments.
      _numSegments :: SegmentNum
    , -- | The required (K) parameter to the encoding function.  This is a
      -- duplicate of the values in uriExtCodecParams and uriExtTailCodecParams.
      _neededShares :: Required
    , -- | The total (N) parameter to the encoding function.  This too is a
      -- duplicate.
      _totalShares :: Total
    , -- | A tagged sha256d hash of the complete ciphertext.
      _crypttextHash :: CrypttextHash SHA256d
    , -- | The root hash of a merkle tree where the leaf hashes are of segments of ciphertext.
      _crypttextRootHash :: CrypttextHash SHA256d
    , -- | The root hash of a merkle tree where leaf hashes are the root hashes of all of the block hash trees.
      _shareRootHash :: CrypttextHash SHA256d
    }
    deriving (Eq, Ord, Generic, ToExpr)

$(makeLenses ''URIExtension)

instance Show URIExtension where
    show (URIExtension name params tailParams sz segSize numSegs needed total hash1 hash2 hash3) =
        T.unpack . T.concat $
            [ "URIExtension { "
            , "codec = "
            , decodeLatin1 name
            , "; codec-params = "
            , showText params
            , "; tail-codec-params = "
            , showText tailParams
            , "; size = "
            , showText sz
            , "; segment-size = "
            , showText segSize
            , "; num-segments = "
            , showText numSegs
            , "; needed-shares = "
            , showText needed
            , "; total-shares = "
            , showText total
            , "; crypttext-hash = "
            , showText $ b32 hash1
            , "; crypttext-root-hash = "
            , showText $ b32 hash2
            , "; share-root-hash = "
            , showText $ b32 hash3
            , " }"
            ]
      where
        showText :: Show s => s -> T.Text
        showText = T.pack . show
        b32 = encodeBase32Unpadded . toBytes

-- Serialize a URIExtension to bytes in the format it appears in a CHK share.
uriExtensionToBytes :: URIExtension -> B.ByteString
uriExtensionToBytes =
    toWeirdString
        -- all of the below values are authenticated by the capability you get when you store data in Tahoe
        [ ("codec_name", view codecName)
        , ("codec_params", paramsToBytes . view codecParams)
        , ("tail_codec_params", paramsToBytes . view tailCodecParams)
        , ("size", showBytes . view size)
        , ("segment_size", showBytes . view segmentSize)
        , ("num_segments", showBytes . view numSegments)
        , ("needed_shares", showBytes . view neededShares)
        , ("total_shares", showBytes . view totalShares)
        , ("crypttext_hash", toBytes . view crypttextHash) -- hash of the *entire* cipher text
        , ("crypttext_root_hash", toBytes . view crypttextRootHash) -- root hash of the *cipher text* merkle tree
        , ("share_root_hash", toBytes . view shareRootHash) -- root hash of the *share* merkle tree
        ]

type Parser = Parsec Void B.ByteString

-- | A version of bounded specialized to parsing bytestrings.
bounded :: (Ord n, Integral n) => n -> n -> Parser n
bounded = Tahoe.CHK.Parsing.bounded decimal

{- | Parse the representation of a URIExtension which appears in CHK shares
 back into a URIExtension.
-}
pURIExtension :: Parser URIExtension
pURIExtension =
    runPermutation $
        URIExtension
            <$> toPermutation (B.pack <$> pField "codec_name" (`count` anySingle))
            <*> toPermutation (pField "codec_params" $ const pParameters)
            <*> toPermutation (pField "tail_codec_params" $ const pParameters)
            <*> toPermutation (pField "size" $ const decimal)
            <*> toPermutation (pField "segment_size" $ const decimal)
            <*> toPermutation (pField "num_segments" $ const (bounded 1 maxBound))
            <*> toPermutation (pField "needed_shares" $ const (bounded 1 256))
            <*> toPermutation (pField "total_shares" $ const (bounded 1 256))
            <*> toPermutation (pFieldM "crypttext_hash" pDigest)
            <*> toPermutation (pFieldM "crypttext_root_hash" pDigest)
            <*> toPermutation (pFieldM "share_root_hash" pDigest)

{- | Parse the raw bytes of a hash algorithm digest back into a Digest'.  The
 parser succeeds if exactly the size of the digest exactly matches the
 specified number of tokens to parse.
-}
pDigest :: HashAlgorithm hash => Int -> Parser (Maybe (Digest' hash))
pDigest = takeP Nothing >=> (pure . (Digest' <$>) . digestFromByteString)

-- | Parse one field of a serialized URIExtension.
pField ::
    -- | The serialized label for the field.
    B.ByteString ->
    -- | A function that takes the length of the field value and returns a parser for the field value.
    (Int -> Parser a) ->
    -- | A parser for the field.
    Parser a
pField label pInner = do
    void $ string (label <> ":")
    len <- decimal -- XXX Could overflow
    void $ string ":"
    result <- pInner len
    void $ string ","
    pure result

{- | Flatten a Parser for a value in Maybe to a Parser for just the value.  A
 Nothing result from the inner parser will trigger a Parser error.
-}
pFieldM :: B.ByteString -> (Int -> Parser (Maybe a)) -> Parser a
pFieldM label pInner = do
    result <- pField label pInner
    case result of
        Nothing -> fail $ "parsing " <> show label <> " failed to produce a value"
        Just r -> pure r

-- | Serialize some named URIExtension fields to bytes.
toWeirdString ::
    -- | A list of pairs of field names and functions to get serialized
    -- field values.
    [(B.ByteString, URIExtension -> B.ByteString)] ->
    -- | The URIExtension to get the field values from.
    URIExtension ->
    -- | The concatenation of all of the serialized fields.
    B.ByteString
toWeirdString fields ext =
    B.concat . join . sort $ map (encodedField ext) fields
  where
    encodedField ext' (name, extract) =
        [name, ":", netstring (extract ext')]

-- | Show a value as a UTF-8-encoded byte string.
showBytes :: (Show s) => s -> B.ByteString
showBytes = encodeUtf8 . T.pack . show

{- | Serialize Parameters to a byte string in the format it appears within the
 URI extension block in a CHK share.
-}
paramsToBytes :: Parameters -> B.ByteString
paramsToBytes Parameters{paramSegmentSize, paramTotalShares, paramRequiredShares} =
    B.concat [showBytes paramSegmentSize, "-", showBytes paramRequiredShares, "-", showBytes paramTotalShares]

{- | Parse a serialized Parameters value in the format produced by
 paramsToBytes.
-}
pParameters :: Parser Parameters
pParameters =
    (\segSize required total -> Parameters{paramSegmentSize = segSize, paramRequiredShares = required, paramHappyShares = 1, paramTotalShares = total})
        <$> decimal
        <* string "-"
        <*> bounded 1 maxShares
        <* string "-"
        <*> bounded 1 maxShares
  where
    maxShares = 256
