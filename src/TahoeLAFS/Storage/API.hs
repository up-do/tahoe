{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
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
    ReadVector (..),
    QueryRange,
    TestVector (..),
    ReadResult,
    CorruptionDetails (..),
    TestOperator (..),
    StorageAPI,
    LeaseSecret (..),
    UploadSecret (..),
    WriteEnablerSecret (..),
    isUploadSecret,
    api,
    renewSecretLength,
    writeEnablerSecretLength,
    leaseRenewSecretLength,
    leaseCancelSecretLength,
    CBOR,
    CBORSet (..),
    readv,
    writev,
    testv,
) where

import Codec.CBOR.Encoding (encodeBytes)
import Codec.Serialise.Class
import Codec.Serialise.Decoding (decodeListLen)
import qualified Codec.Serialise.Decoding as CSD
import qualified Codec.Serialise.Encoding as CSE
import Control.Monad
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
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString as B
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64
import qualified Data.Map as Map
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import Data.Map.Strict (
    Map,
 )
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text.Encoding (
    decodeUtf8',
 )
import GHC.Generics (
    Generic,
 )
import Network.HTTP.Types (
    ByteRanges,
    parseByteRanges,
    renderByteRanges,
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
    Put,
    ReqBody,
    StdMethod (PATCH),
    Verb,
    (:<|>),
    (:>),
 )
import Tahoe.Storage.Backend (
    AllocateBuckets (..),
    AllocationResult (..),
    ApplicationVersion,
    CBORSet (..),
    CorruptionDetails (..),
    LeaseSecret (..),
    Offset,
    QueryRange,
    ReadResult,
    ReadTestWriteResult (..),
    ReadTestWriteVectors (..),
    ReadVector (..),
    ShareData,
    ShareNumber (..),
    Size,
    StorageIndex,
    TestOperator (..),
    TestVector (..),
    TestWriteVectors (..),
    UploadSecret (..),
    Version (..),
    Version1Parameters (..),
    WriteEnablerSecret (..),
    WriteVector (..),
    readv,
    testv,
    writev,
 )
import TahoeLAFS.Internal.ServantUtil (
    CBOR,
 )
import Text.Read (
    readMaybe,
 )
import Web.HttpApiData (
    FromHttpApiData (..),
    ToHttpApiData (..),
 )
import Prelude hiding (
    toInteger,
 )

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

-- | Encode a CBORSet using a CBOR "set" tag and a determinate length list.
encodeCBORSet :: (Serialise a) => CBORSet a -> CSE.Encoding
encodeCBORSet (CBORSet theSet) =
    CSE.encodeTag 258
        <> CSE.encodeListLen (fromIntegral $ Set.size theSet) -- XXX don't trust fromIntegral
        <> Set.foldr (\x r -> encode x <> r) mempty theSet

-- | Decode a determinate length list with a CBOR "set" tag.
decodeCBORSet :: (Serialise a, Ord a) => CSD.Decoder s (CBORSet a)
decodeCBORSet = do
    tag <- CSD.decodeTag
    if tag /= 258
        then fail $ "expected set tag (258), found " <> show tag
        else do
            listLength <- decodeListLen
            CBORSet . Set.fromList <$> replicateM listLength decode

-- | Define serialisation for CBORSets in a way that is compatible with GBS.
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
    toQueryParam = T.pack . show . toInteger

instance FromHttpApiData ShareNumber where
    parseUrlPiece t =
        case readMaybe $ T.unpack t of
            Nothing -> Left "failed to parse"
            Just i -> case shareNumber i of
                Nothing -> Left "number out of bounds"
                Just s -> Right s
    parseQueryParam = parseUrlPiece
    parseHeader bs =
        case parseUrlPiece <$> decodeUtf8' bs of
            Left err ->
                Left $
                    T.concat
                        [ "FromHttpApiData ShareNumber instance failed to decode number from header: "
                        , T.pack . show $ err
                        ]
            Right sn -> sn

instance ToJSONKey ShareNumber where
    toJSONKey = toJSONKeyText (T.pack . show)

shareNumber :: Integer -> Maybe ShareNumber
shareNumber n =
    if n < 0
        then Nothing
        else Just $ ShareNumber n

toInteger :: ShareNumber -> Integer
toInteger (ShareNumber i) = i

encodeVersion1Parameters :: Version1Parameters -> CSE.Encoding
encodeVersion1Parameters Version1Parameters{..} =
    CSE.encodeMapLen 3 -- three rings for the elven kings
        <> CSE.encodeBytes "maximum-immutable-share-size"
        <> CSE.encodeInteger maximumImmutableShareSize
        <> CSE.encodeBytes "maximum-mutable-share-size"
        <> CSE.encodeInteger maximumMutableShareSize
        <> CSE.encodeBytes "available-space"
        <> CSE.encodeInteger availableSpace

decodeMap :: (Ord k, Serialise k, Serialise v) => CSD.Decoder s (Map k v)
decodeMap = do
    lenM <- CSD.decodeMapLenOrIndef
    case lenM of
        Nothing -> Map.fromList <$> decodeMapIndef
        Just len -> Map.fromList <$> decodeMapOfLen len
  where
    decodeMapIndef = do
        atTheEnd <- CSD.decodeBreakOr
        if atTheEnd
            then pure []
            else do
                k <- decode
                v <- decode
                ((k, v) :) <$> decodeMapIndef

    decodeMapOfLen 0 = pure []
    decodeMapOfLen n = do
        k <- decode
        v <- decode
        ((k, v) :) <$> decodeMapOfLen (n - 1)

decodeVersion1Parameters :: CSD.Decoder s Version1Parameters
decodeVersion1Parameters = do
    m <- decodeMap
    case (Map.size m, map (`Map.lookup` m) keys) of
        (3, [Just availableSpace, Just maximumImmutableShareSize, Just maximumMutableShareSize]) ->
            pure Version1Parameters{..}
        _ -> fail "invalid encoding of Version1Parameters"
  where
    keys = ["available-space", "maximum-immutable-share-size", "maximum-mutable-share-size"] :: [B.ByteString]

instance Serialise Version1Parameters where
    encode = encodeVersion1Parameters
    decode = decodeVersion1Parameters

instance ToJSON Version1Parameters where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON Version1Parameters where
    parseJSON = genericParseJSON tahoeJSONOptions

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
    mapLen <- CSD.decodeMapLen
    case mapLen of
        2 -> do
            -- Take care to handle either order of fields in the map.
            k1 <- CSD.decodeBytes
            case k1 of
                "http://allmydata.org/tahoe/protocols/storage/v1" -> do
                    parameters <- decodeVersion1Parameters
                    k2 <- CSD.decodeBytes
                    case k2 of
                        "application-version" -> do
                            applicationVersion <- decodeApplicationVersion
                            pure Version{..}
                        _ -> fail "decodeVersion got bad input"
                "application-version" -> do
                    applicationVersion <- decodeApplicationVersion
                    k2 <- CSD.decodeBytes
                    case k2 of
                        "http://allmydata.org/tahoe/protocols/storage/v1" -> do
                            parameters <- decodeVersion1Parameters
                            pure Version{..}
                        _ -> fail "decodeVersion got bad input"
                _ -> fail "decodeVersion got bad input"
        _ -> fail "decodeVersion got bad input"

instance Serialise Version where
    encode = encodeVersion
    decode = decodeVersion

instance ToJSON Version where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON Version where
    parseJSON = genericParseJSON tahoeJSONOptions

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise AllocateBuckets

instance ToJSON AllocateBuckets where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON AllocateBuckets where
    parseJSON = genericParseJSON tahoeJSONOptions

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise AllocationResult

instance ToJSON AllocationResult where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON AllocationResult where
    parseJSON = genericParseJSON tahoeJSONOptions

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
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

    parseUrlPiece _ = Left "Cannot parse ByteRanges from URL piece"
    parseQueryParam _ = Left "Cannot parse ByteRanges from query params"

instance ToHttpApiData ByteRanges where
    toHeader = renderByteRanges

    toUrlPiece _ = error "Cannot serialize ByteRanges to URL piece"
    toQueryParam _ = error "Cannot serialize ByteRanges to query params"

isUploadSecret :: LeaseSecret -> Bool
isUploadSecret (Upload _) = True
isUploadSecret _ = False

instance FromHttpApiData LeaseSecret where
    parseHeader bs =
        do
            let [key, val] = B.split 32 bs
            case key of
                "lease-renew-secret" -> bimap T.pack Renew $ Base64.decode val
                "lease-cancel-secret" -> bimap T.pack Cancel $ Base64.decode val
                "upload-secret" -> bimap T.pack (Upload . UploadSecret) $ Base64.decode val
                "write-enabler" -> bimap T.pack (Write . WriteEnablerSecret) $ Base64.decode val
                _ -> Left $ T.concat ["Cannot interpret secret: ", T.pack . show $ key]

    parseUrlPiece _ = Left "Cannot parse LeaseSecret from URL piece"
    parseQueryParam _ = Left "Cannot parse LeaseSecret from query params"

instance FromHttpApiData [LeaseSecret] where
    -- XXX Consider whitespace?
    parseHeader =
        mapM parseHeader . B.split (fromIntegral $ fromEnum ',')

    parseUrlPiece _ = Left "Cannot parse [LeaseSecret] from URL piece"
    parseQueryParam _ = Left "Cannot parse [LeaseSecret] from query params"

instance ToHttpApiData LeaseSecret where
    toHeader (Renew bs) = "lease-renew-secret " <> Base64.encode bs
    toHeader (Cancel bs) = "lease-cancel-secret " <> Base64.encode bs
    toHeader (Upload (UploadSecret bs)) = "lease-cancel-secret " <> Base64.encode bs
    toHeader (Write (WriteEnablerSecret bs)) = "write-enabler " <> Base64.encode bs

    toUrlPiece _ = error "Cannot serialize LeaseSecret to URL piece"
    toQueryParam _ = error "Cannot serialize LeaseSecret to query params"

instance ToHttpApiData [LeaseSecret] where
    toHeader = B.intercalate "," . map toHeader
    toUrlPiece _ = error "Cannot serialize [LeaseSecret] to URL piece"
    toQueryParam _ = error "Cannot serialize [LeaseSecret] to query params"

-- Request authorization information
type Authz = Header "X-Tahoe-Authorization" [LeaseSecret]

-- GET .../version
-- Retrieve information about the server version and behavior
type GetVersion = "version" :> Get '[CBOR, JSON] Version

-- PUT .../lease/:storage_index
type RenewLease = "lease" :> Capture "storage_index" StorageIndex :> Authz :> Get '[CBOR, JSON] ()

-- POST .../immutable/:storage_index
-- Initialize a new immutable storage index
type CreateImmutableStorageIndex = "immutable" :> Capture "storage_index" StorageIndex :> Authz :> ReqBody '[CBOR, JSON] AllocateBuckets :> PostCreated '[CBOR, JSON] AllocationResult

--
-- PATCH .../immutable/:storage_index/:share_number
-- Write data for an immutable share to an allocated storage index
--
-- Note this accepts JSON to facilitate code generation by servant-py.  This
-- is total nonsense and supplying JSON here will almost certainly break.
-- At some point hopefully we'll fix servant-py to not need this and then
-- fix the signature here.
type WriteImmutableShareData = "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> Authz :> ReqBody '[OctetStream, JSON] ShareData :> Header "Content-Range" ByteRanges :> Verb 'PATCH 201 '[CBOR, JSON] ()

-- PUT .../immutable/:storage_index/:share_number/unstableSort
-- Cancel an incomplete immutable share upload.
type AbortImmutableUpload = "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> "abort" :> Authz :> Put '[JSON] ()

-- POST .../immutable/:storage_index/:share_number/corrupt
-- Advise the server of a corrupt share data
type AdviseCorrupt = Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> "corrupt" :> ReqBody '[CBOR, JSON] CorruptionDetails :> Post '[CBOR, JSON] ()

-- GET .../{mutable,immutable}/storage_index/shares
-- Retrieve the share numbers available for a storage index
type GetShareNumbers = Capture "storage_index" StorageIndex :> "shares" :> Get '[CBOR, JSON] (CBORSet ShareNumber)

--
-- GET .../v1/immutable/<storage_index:storage_index>/<int(signed=False):share_number>"
-- Read from an immutable storage index, possibly from multiple shares, possibly limited to certain ranges
type ReadImmutableShareData = "immutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> Header "Content-Range" ByteRanges :> Get '[OctetStream, JSON] ShareData

-- POST .../v1/mutable/:storage_index/read-test-write
-- General purpose read-test-and-write operation.
type ReadTestWrite = "mutable" :> Capture "storage_index" StorageIndex :> "read-test-write" :> Authz :> ReqBody '[CBOR, JSON] ReadTestWriteVectors :> Post '[CBOR, JSON] ReadTestWriteResult

-- GET /v1/mutable/:storage_index/:share_number
-- Read from a mutable storage index
type ReadMutableShareData = "mutable" :> Capture "storage_index" StorageIndex :> Capture "share_number" ShareNumber :> Header "Content-Range" ByteRanges :> Get '[OctetStream, JSON] ShareData

type StorageAPI =
    "storage"
        :> "v1"
        :> ( GetVersion
                :<|> RenewLease
                -- Immutables
                :<|> CreateImmutableStorageIndex
                :<|> WriteImmutableShareData
                :<|> AbortImmutableUpload
                :<|> ReadImmutableShareData
                :<|> "immutable" :> GetShareNumbers
                :<|> "immutable" :> AdviseCorrupt
                -- Mutables
                :<|> ReadTestWrite
                :<|> ReadMutableShareData
                :<|> "mutable" :> GetShareNumbers
                :<|> "mutable" :> AdviseCorrupt
           )

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise ReadTestWriteResult

instance ToJSON ReadTestWriteResult where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadTestWriteResult where
    parseJSON = genericParseJSON tahoeJSONOptions

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise ReadTestWriteVectors

instance ToJSON ReadTestWriteVectors where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadTestWriteVectors where
    parseJSON = genericParseJSON tahoeJSONOptions

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise ReadVector

instance ToJSON ReadVector where
    toJSON = genericToJSON tahoeJSONOptions

instance FromJSON ReadVector where
    parseJSON = genericParseJSON tahoeJSONOptions

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise TestWriteVectors

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise TestOperator

-- XXX This derived instance is surely not compatible with Tahoe-LAFS.
instance Serialise TestVector

instance Serialise WriteVector

api :: Proxy StorageAPI
api = Proxy
