{-# LANGUAGE DataKinds #-}
-- https://artyom.me/aeson#records-and-json-generics
{-# LANGUAGE DeriveAnyClass #-}
-- https://artyom.me/aeson#records-and-json-generics
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
-- Supports derivations for ShareNumber
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TahoeLAFS.Storage.API (
    Version (..),
    Size,
    Offset,
    StorageIndex,
    ShareNumber (ShareNumber),
    shareNumber,
    toInteger,
    ShareData,
    ApplicationVersion,
    Version1Parameters (..),
    AllocateBuckets (..),
    AllocationResult (..),
    TestWriteVectors (..),
    WriteVector (..),
    ReadTestWriteVectors (..),
    ReadTestWriteResult (..),
    ReadVectors,
    ReadVector,
    QueryRange,
    TestVector (TestVector),
    ReadResult,
    CorruptionDetails (CorruptionDetails),
    SlotSecrets (..),
    TestOperator (..),
    StorageAPI,
    api,
    renewSecretLength,
    writeEnablerSecretLength,
    leaseRenewSecretLength,
    leaseCancelSecretLength,
    CBOR,
    CBORSet (..),
) where

import Debug.Trace

import Codec.Serialise (Serialise (decode, encode))
import Codec.Serialise.Class
import qualified Codec.Serialise.Decoding as CSD
import qualified Codec.Serialise.Encoding as CSE
import Control.Monad
import qualified Data.Set as Set
import Data.Text (
    pack,
    unpack,
 )
import Data.Text.Encoding (
    decodeUtf8,
 )
import Prelude hiding (
    toInteger,
 )

import Data.ByteString (
    ByteString,
 )

import Data.Map.Strict (
    Map,
 )

import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    ToJSONKey (..),
    camelTo2,
    defaultOptions,
    fieldLabelModifier,
    genericParseJSON,
    genericToJSON,
 )

import Data.Aeson.Types (
    Options,
    toJSONKeyText,
 )

import GHC.Generics (
    Generic,
 )

import Servant (
    Capture,
    Get,
    Header,
    JSON,
    OctetStream,
    Post,
    PostCreated,
    Proxy (Proxy),
    QueryParams,
    ReqBody,
    StdMethod (PUT),
    Verb,
    (:<|>),
    (:>),
 )
import Text.Read (
    readMaybe,
 )

import Web.HttpApiData (
    FromHttpApiData (..),
    ToHttpApiData (..),
 )

import Network.HTTP.Types (
    ByteRanges,
    parseByteRanges,
    renderByteRanges,
 )

import Codec.CBOR.Encoding (encodeBytes, encodeInt)
import Codec.Serialise.Decoding (decodeBool, decodeBytes, decodeInteger, decodeListLen, decodeMapLen)
import TahoeLAFS.Internal.ServantUtil (
    CBOR,
 )

type PutCreated = Verb 'PUT 201

tahoeJSONOptions :: Options
tahoeJSONOptions =
    defaultOptions
        { fieldLabelModifier = camelTo2 '-'
        }

-- The expected lengths of the secrets represented as opaque byte strings.
-- I haven't checked that these values are correct according to Tahoe-LAFS.
renewSecretLength :: Num a => a
renewSecretLength = 32
writeEnablerSecretLength :: Num a => a
writeEnablerSecretLength = 32
leaseRenewSecretLength :: Num a => a
leaseRenewSecretLength = 32
leaseCancelSecretLength :: Num a => a
leaseCancelSecretLength = 32

type ApplicationVersion = ByteString
type Size = Integer
type Offset = Integer
type QueryRange = Maybe ByteRanges

-- TODO These should probably all be byte strings instead.
type RenewSecret = String
type CancelSecret = String
type StorageIndex = String
type ShareData = ByteString

newtype ShareNumber = ShareNumber Integer
    deriving
        ( Show
        , Eq
        , Ord
        , ToJSON
        , FromJSON
        , FromJSONKey
        , Generic
        )

newtype CBORSet a = CBORSet
    { getCBORSet :: (Set.Set a)
    }
    deriving newtype (ToJSON, FromJSON, Show, Eq)

encodeCBORSet :: (Serialise a) => CBORSet a -> CSE.Encoding
encodeCBORSet (CBORSet theSet) =
    CSE.encodeTag 258
        <> CSE.encodeListLen (fromIntegral $ Set.size theSet) -- XXX don't trust fromIntegral
        <> Set.foldr (\x r -> encode x <> r) mempty theSet

decodeCBORSet :: (Serialise a, Ord a) => CSD.Decoder s (CBORSet a)
decodeCBORSet = do
    _ <- CSD.decodeTag -- probly fine
    listLength <- decodeListLen
    CBORSet . Set.fromList <$> replicateM listLength decode

instance (Serialise a, Ord a) => Serialise (CBORSet a) where
    encode = encodeCBORSet
    decode = decodeCBORSet

instance Serialise ShareNumber where
    decode = decodeShareNumber
    encode = encodeShareNumber

encodeShareNumber :: ShareNumber -> CSE.Encoding
encodeShareNumber (ShareNumber i) = CSE.encodeInteger i

decodeShareNumber :: CSD.Decoder s ShareNumber
decodeShareNumber = ShareNumber <$> CSD.decodeInteger

instance ToHttpApiData ShareNumber where
    toQueryParam = pack . show . toInteger

instance FromHttpApiData ShareNumber where
    parseUrlPiece t =
        case readMaybe $ unpack t of
            Nothing -> Left "failed to parse"
            Just i -> case shareNumber i of
                Nothing -> Left "number out of bounds"
                Just s -> Right s
    parseQueryParam = parseUrlPiece
    parseHeader = parseUrlPiece . decodeUtf8

instance ToJSONKey ShareNumber where
    toJSONKey = toJSONKeyText (pack . show)

shareNumber :: Integer -> Maybe ShareNumber
shareNumber n =
    if n < 0
        then Nothing
        else Just $ ShareNumber n

toInteger :: ShareNumber -> Integer
toInteger (ShareNumber i) = i

data Version1Parameters = Version1Parameters
    { maximumImmutableShareSize :: Size
    , maximumMutableShareSize :: Size
    , availableSpace :: Size
    }
    deriving (Show, Eq, Generic)

encodeVersion1Parameters :: Version1Parameters -> CSE.Encoding
encodeVersion1Parameters Version1Parameters{..} =
    CSE.encodeMapLen 3 -- three rings for the elven kings
        <> CSE.encodeBytes "maximum-immutable-share-size"
        <> CSE.encodeInteger maximumImmutableShareSize
        <> CSE.encodeBytes "maximum-mutable-share-size"
        <> CSE.encodeInteger maximumMutableShareSize
        <> CSE.encodeBytes "available-space"
        <> CSE.encodeInteger availableSpace

-- | XXX this will break if the order changes, and likely require a 'real' parser when/if that happens
decodeVersion1Parameters :: CSD.Decoder s Version1Parameters
decodeVersion1Parameters = do
    len <- decodeMapLen
    case len of
        3 ->
            Version1Parameters
                <$ CSD.decodeBytes -- "maximum-immutable-share-size"
                <*> CSD.decodeInteger
                <* CSD.decodeBytes -- "maximum-mutable-share-size"
                <*> CSD.decodeInteger
                <* CSD.decodeBytes -- "available-space"
                <*> CSD.decodeInteger
        _ -> fail "invalid encoding of Version1Parameters"

instance Serialise Version1Parameters where
    encode = encodeVersion1Parameters
    decode = decodeVersion1Parameters

instance ToJSON Version1Parameters where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON Version1Parameters where
    parseJSON = genericParseJSON tahoeJSONOptions

data Version = Version
    { parameters :: Version1Parameters
    , applicationVersion :: ApplicationVersion
    }
    deriving (Show, Eq, Generic)

encodeApplicationVersion :: ApplicationVersion -> CSE.Encoding
encodeApplicationVersion = CSE.encodeBytes

decodeApplicationVersion :: CSD.Decoder s ApplicationVersion
decodeApplicationVersion = CSD.decodeBytes

encodeVersion :: Version -> CSE.Encoding
encodeVersion Version{..} =
    CSE.encodeMapLen 2
        <> encodeBytes "http://allmydata.org/tahoe/protocols/storage/v1"
        <> encodeVersion1Parameters parameters
        <> encodeBytes "application-version"
        <> encodeApplicationVersion applicationVersion

decodeVersion :: CSD.Decoder s Version
decodeVersion = do
    mapLen <- CSD.decodeMapLen -- hope it's 2
    case mapLen of
        2 -> Version <$ CSD.decodeBytes <*> decodeVersion1Parameters <* CSD.decodeBytes <*> decodeApplicationVersion
        _ -> fail "decodeVersion got bad input"

instance Serialise Version where
    encode = encodeVersion
    decode = decodeVersion

instance ToJSON Version where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON Version where
    parseJSON = genericParseJSON tahoeJSONOptions

data AllocateBuckets = AllocateBuckets
    { renewSecret :: RenewSecret
    , cancelSecret :: CancelSecret
    , shareNumbers :: [ShareNumber]
    , allocatedSize :: Size
    }
    deriving (Show, Eq, Generic)

instance Serialise AllocateBuckets

instance ToJSON AllocateBuckets where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON AllocateBuckets where
    parseJSON = genericParseJSON tahoeJSONOptions

data AllocationResult = AllocationResult
    { alreadyHave :: [ShareNumber]
    , allocated :: [ShareNumber]
    }
    deriving (Show, Eq, Generic)

instance Serialise AllocationResult

instance ToJSON AllocationResult where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON AllocationResult where
    parseJSON = genericParseJSON tahoeJSONOptions

data ShareType
    = Mutable
    | Immutable
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data CorruptionDetails = CorruptionDetails
    { reason :: String
    }
    deriving (Show, Eq, Generic)

instance Serialise CorruptionDetails

instance ToJSON CorruptionDetails where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON CorruptionDetails where
    parseJSON = genericParseJSON tahoeJSONOptions

instance FromHttpApiData ByteRanges where
    parseHeader bs =
        case parseByteRanges bs of
            Nothing -> Left "parse failed"
            Just br -> Right br

instance ToHttpApiData ByteRanges where
    toHeader = renderByteRanges

type StorageAPI =
    -- General server information

    --
    -- GET /v1/version
    -- Retrieve information about the server version and behavior
    "v1" :> "version" :> Get '[CBOR, JSON] Version
        -- PUT /storage/v1/lease/:storage_index
        -- :<|> "v1" :> "lease" :> Capture "storage_index" StorageIndex :> Header "X-Tahoe-Authorization" [LeaseSecret] :> Get '[JSON] ()
        -- Immutable share interactions

        --
        -- POST /v1/immutable/:storage_index
        -- Initialize a new immutable storage index
        :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> ReqBody '[CBOR, JSON] AllocateBuckets :> PostCreated '[CBOR, JSON] AllocationResult
        --
        -- PUT /v1/immutable/:storage_index/:share_number
        -- Write data for an immutable share to an allocated storage index
        --
        -- Note this accepts JSON to facilitate code generation by servant-py.  This
        -- is total nonsense and supplying JSON here will almost certainly break.
        -- At some point hopefully we'll fix servant-py to not need this and then
        -- fix the signature here.
        :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> ReqBody '[OctetStream] ShareData :> Header "Content-Range" ByteRanges :> PutCreated '[CBOR, JSON] ()
        --
        -- POST /v1/immutable/:storage_index/:share_number/corrupt
        -- Advise the server of a corrupt share data
        :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> "corrupt" :> ReqBody '[CBOR, JSON] CorruptionDetails :> Post '[CBOR, JSON] ()
        --
        -- GET /v1/immutable/storage_index/shares
        -- Retrieve the share numbers available for a storage index
        :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> "shares" :> Get '[CBOR, JSON] (CBORSet ShareNumber)
        --
        -- GET /storage/v1/immutable/<storage_index:storage_index>/<int(signed=False):share_number>"
        -- Read from an immutable storage index, possibly from multiple shares, possibly limited to certain ranges
        :<|> "v1" :> "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> Header "Content-Range" ByteRanges :> Get '[CBOR, JSON] ShareData
        -- Mutable share interactions

        --
        -- POST /v1/mutable/:storage_index
        -- Initialize a new mutable storage index
        :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> ReqBody '[CBOR, JSON] AllocateBuckets :> Post '[CBOR, JSON] AllocationResult
        --
        -- POST /v1/mutable/:storage_index/read-test-write
        -- General purpose read-test-and-write operation.
        :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> "read-test-write" :> ReqBody '[CBOR, JSON] ReadTestWriteVectors :> Post '[CBOR, JSON] ReadTestWriteResult
        --
        -- GET /v1/mutable/:storage_index
        -- Read from a mutable storage index
        :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> QueryParams "share_number" ShareNumber :> QueryParams "offset" Offset :> QueryParams "size" Size :> Get '[CBOR, JSON] ReadResult
        --
        -- GET /v1/mutable/:storage_index/shares
        -- Retrieve the share numbers available for a storage index
        :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> "shares" :> Get '[CBOR, JSON] [ShareNumber]
        --
        -- POST /v1/mutable/:storage_index/:share_number/corrupt
        -- Advise the server of a corrupt share data
        :<|> "v1" :> "mutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> "corrupt" :> ReqBody '[CBOR, JSON] CorruptionDetails :> Post '[CBOR, JSON] ()

type ReadResult = Map ShareNumber [ShareData]

data ReadVectors = ReadVectors
    { shares :: [ShareNumber]
    , readVectors :: [ReadVector]
    }
    deriving (Show, Eq, Generic)

instance ToJSON ReadVectors where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadVectors where
    parseJSON = genericParseJSON tahoeJSONOptions

data ReadTestWriteResult = ReadTestWriteResult
    { success :: Bool
    , readData :: ReadResult
    }
    deriving (Show, Eq, Generic)

instance Serialise ReadTestWriteResult

instance ToJSON ReadTestWriteResult where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadTestWriteResult where
    parseJSON = genericParseJSON tahoeJSONOptions

data ReadTestWriteVectors = ReadTestWriteVectors
    { secrets :: SlotSecrets
    , testWriteVectors :: Map ShareNumber TestWriteVectors
    , readVector :: [ReadVector]
    }
    deriving (Show, Eq, Generic)

instance Serialise ReadTestWriteVectors

instance ToJSON ReadTestWriteVectors where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadTestWriteVectors where
    parseJSON = genericParseJSON tahoeJSONOptions

data ReadVector = ReadVector
    { offset :: Offset
    , readSize :: Size
    }
    deriving (Show, Eq, Generic)

instance Serialise ReadVector

instance ToJSON ReadVector where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadVector where
    parseJSON = genericParseJSON tahoeJSONOptions

data TestWriteVectors = TestWriteVectors
    { test :: [TestVector]
    , write :: [WriteVector]
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Serialise TestWriteVectors

data TestOperator
    = Lt
    | Le
    | Eq
    | Ne
    | Ge
    | Gt
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Serialise TestOperator

data TestVector = TestVector
    { testOffset :: Offset
    , testSize :: Size
    , operator :: TestOperator
    , specimen :: ShareData
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Serialise TestVector

data WriteVector = WriteVector
    { writeOffset :: Offset
    , shareData :: ShareData
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Serialise WriteVector

data SlotSecrets = SlotSecrets
    { writeEnabler :: WriteEnablerSecret
    , leaseRenew :: LeaseRenewSecret
    , leaseCancel :: LeaseCancelSecret
    }
    deriving (Show, Eq, Generic)

instance Serialise SlotSecrets

instance ToJSON SlotSecrets where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON SlotSecrets where
    parseJSON = genericParseJSON tahoeJSONOptions

type WriteEnablerSecret = String
type LeaseRenewSecret = String
type LeaseCancelSecret = String

api :: Proxy StorageAPI
api = Proxy
