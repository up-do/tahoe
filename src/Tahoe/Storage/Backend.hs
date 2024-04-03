{- | Types involved in the implementation of the backend for a Tahoe-LAFS
 storage server.
-}
module Tahoe.Storage.Backend where

import Control.Exception (
    Exception,
 )
import Data.Aeson (
    FromJSON (..),
    FromJSONKey (..),
    ToJSON (..),
    Value (String),
    object,
    withObject,
    withText,
    (.:),
    (.=),
 )
import Data.ByteArray (constEq)
import qualified Data.ByteString as B
import Data.Hashable (Hashable (hashWithSalt))
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import GHC.Generics (
    Generic,
 )
import Network.HTTP.Types (
    ByteRange,
    ByteRanges,
 )
import qualified "base64-bytestring" Data.ByteString.Base64 as Base64

-- | A human-readable description of the backend software in use.
type ApplicationVersion = B.ByteString

-- | Give certain operational details about this storage server.
data Version1Parameters = Version1Parameters
    { maximumImmutableShareSize :: Size
    , maximumMutableShareSize :: Size
    , availableSpace :: Size
    }
    deriving (Show, Eq, Generic)

-- | Carry a version string and operational parameters.
data Version = Version
    { parameters :: Version1Parameters
    , applicationVersion :: ApplicationVersion
    }
    deriving (Show, Eq, Generic)

-- | The number of bytes in some data.
type Size = Integer

{- | A position in some data identified by a number of bytes into that data
 from the beginning.
-}
type Offset = Integer

{- | Some ranges of data, possibly.  Nothing conventionally refers to the
 entire data.
-}
type QueryRange = Maybe ByteRanges

{- | An opaque identifier for a single storage area.  Multiple storage objects
 may exist at one storage index if they have different share numbers.  TODO:
 This should probably be ByteString instead.
-}
type StorageIndex = String

-- | Some data.
type ShareData = B.ByteString

{- | The identifier of a distinct storage object, unique within the context of
 a particular storage index.  In practice, values are in the range [0..255]
 (endpoints included).
-}
newtype ShareNumber = ShareNumber Integer
    deriving
        ( Show
        , Eq
        , Ord
        , Generic
        )
    deriving newtype
        ( ToJSON
        , FromJSON
        , FromJSONKey
        )

instance Hashable ShareNumber where
    hashWithSalt i (ShareNumber num) = hashWithSalt i num

{- | A new type for which we can define our own CBOR serialisation rules.  The
 cborg library provides a Serialise instance for Set which is not compatible
 with the representation required by Tahoe-LAFS.
-}
newtype CBORSet a = CBORSet
    { getCBORSet :: Set.Set a
    }
    deriving newtype (ToJSON, FromJSON, Show, Eq)

-- | Describe a client-detected incidence of data corruption.
newtype CorruptionDetails = CorruptionDetails
    { reason :: String
    }
    deriving (Show, Eq, Generic)

{- | A secret shared between a client and this storage server for the purpose
 of authorizing certain operations related to immutable uploads.
-}
newtype UploadSecret = UploadSecret B.ByteString

instance Eq UploadSecret where
    (UploadSecret left) == (UploadSecret right) = constEq left right

{- | A secret shared between an SSK write capability holder and this storage
 server for the purpose of authorizing write operations on a mutable object.
-}
newtype WriteEnablerSecret = WriteEnablerSecret B.ByteString

instance Eq WriteEnablerSecret where
    (WriteEnablerSecret left) == (WriteEnablerSecret right) = constEq left right

-- | Describe tests, reads, and writes to perform on a mutable object.
data ReadTestWriteVectors = ReadTestWriteVectors
    { testWriteVectors :: Map.Map ShareNumber TestWriteVectors
    , readVector :: [ReadVector]
    }
    deriving (Show, Eq, Generic)

-- | Describe tests and writes to perform on a mutable object.
data TestWriteVectors = TestWriteVectors
    { test :: [TestVector]
    , write :: [WriteVector]
    , newLength :: Maybe Integer
    -- ^ If given, truncate or extend the object to the given size.  If
    -- necessary, fill new space with NUL.
    }
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Describe a single test to perform on a mutable object.
data TestVector = TestVector
    { testOffset :: Offset
    , testSize :: Size
    , operator :: TestOperator
    , specimen :: ShareData
    }
    deriving (Show, Eq, Generic)

-- | Describe a single write to perform on a mutable object.
data WriteVector = WriteVector
    { writeOffset :: Offset
    , shareData :: ShareData
    }
    deriving (Show, Eq, Generic)

-- | Describe one read to perform on an immutable object.
data ReadVector = ReadVector
    { offset :: Offset
    , readSize :: Size
    }
    deriving (Show, Eq, Generic)

{- | The result of a request to read and/or write some data from/to some
 shares.
-}
data ReadTestWriteResult = ReadTestWriteResult
    { success :: Bool
    , readData :: ReadResult
    }
    deriving (Show, Eq, Generic)

-- | The result of a request to read some data from some shares.
type ReadResult = Map.Map ShareNumber [ShareData]

instance Semigroup TestWriteVectors where
    (TestWriteVectors testL writeL _) <> (TestWriteVectors testR writeR newLengthR) =
        TestWriteVectors (testL <> testR) (writeL <> writeR) newLengthR

instance Monoid TestWriteVectors where
    mempty = TestWriteVectors mempty mempty Nothing

instance Monoid ReadTestWriteVectors where
    mempty = ReadTestWriteVectors mempty []

instance Semigroup ReadTestWriteVectors where
    (ReadTestWriteVectors wv0 rv0) <> (ReadTestWriteVectors wv1 rv1) =
        ReadTestWriteVectors (merge preserveMissing preserveMissing (zipWithMatched $ \_ l r -> l <> r) wv0 wv1) (rv0 <> rv1)

{- | Create a ReadTestWriteVectors which performs one read at the given offset
 of the given size.
-}
readv :: Offset -> Size -> ReadTestWriteVectors
readv offset size = mempty{readVector = [ReadVector offset size]}

{- | Create a ReadTestWriteVectors which performs one write on the given share
 number at the given offset.
-}
writev :: ShareNumber -> Offset -> ShareData -> ReadTestWriteVectors
writev shareNum offset bytes = mempty{testWriteVectors = Map.singleton shareNum (mempty{write = [WriteVector offset bytes]})}

{- | Create a ReadTestWriteVectors which performs one test on the given share
 number using the given specimen.
-}
testv :: ShareNumber -> Offset -> ShareData -> ReadTestWriteVectors
testv shareNum offset specimen =
    mempty
        { testWriteVectors = Map.singleton shareNum (mempty{test = [TestVector offset (fromIntegral $ B.length specimen) Eq specimen]})
        }

-- | Describe some allocations for immutable objects requested by a client.
data AllocateBuckets = AllocateBuckets
    { shareNumbers :: [ShareNumber]
    , allocatedSize :: Size
    }
    deriving (Show, Eq, Generic)

{- | Describe the server's willingness to allocate space for some immutable
 objects.
-}
data AllocationResult = AllocationResult
    { alreadyHave :: [ShareNumber]
    , allocated :: [ShareNumber]
    }
    deriving (Show, Eq, Generic)

instance Semigroup AllocationResult where
    AllocationResult xa ya <> AllocationResult xb yb = AllocationResult (xa <> xb) (ya <> yb)

instance Monoid AllocationResult where
    mempty = AllocationResult mempty mempty

{- | One of several kinds of shared secrets for authorizing a client to
 perform various operations.
-}
data LeaseSecret = Renew B.ByteString | Cancel B.ByteString | Upload UploadSecret | Write WriteEnablerSecret

-- XXX Most of these operators have been removed from the spec.
data TestOperator
    = Lt
    | Le
    | Eq
    | Ne
    | Ge
    | Gt
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | A ByteString wrapper which can have its own serialization semantics.
newtype Base64 = Base64 {unBase64 :: B.ByteString}

-- | Serialize a ByteString by encoding with Base64.
instance ToJSON Base64 where
    toJSON = String . T.decodeLatin1 . Base64.encode . unBase64

-- | Deserialize a ByteString which has been encoded with Base64.
instance FromJSON Base64 where
    parseJSON =
        withText
            "Base64-encoded ByteString"
            ( \bs ->
                case Base64.decode $ T.encodeUtf8 bs of
                    Left err -> fail ("Base64 decoding failed: " <> err)
                    Right bytes -> pure $ Base64 bytes
            )

-- | Serialize to the JSON as specified by the specification.
instance ToJSON TestVector where
    toJSON (TestVector{..}) =
        object ["offset" .= testOffset, "size" .= testSize, "specimen" .= Base64 specimen]

-- | Deserialize from JSON as specified by the specification.
instance FromJSON TestVector where
    parseJSON = withObject "TestVector" $ \v ->
        TestVector <$> v .: "offset" <*> v .: "size" <*> pure Eq <*> (unBase64 <$> v .: "specimen")

-- | Serialize to the JSON as specified by the specification.
instance ToJSON WriteVector where
    toJSON (WriteVector{..}) =
        object ["offset" .= writeOffset, "data" .= Base64 shareData]

-- | Deserialize from JSON as specified by the specification.
instance FromJSON WriteVector where
    parseJSON = withObject "WriteVector" $ \v ->
        WriteVector <$> v .: "offset" <*> (unBase64 <$> v .: "data")

{- | Exceptional cases which might be encounted by various backend for
 operations related to immutable shares.
-}
data WriteImmutableError
    = -- | Used to reject an immutable allocate or upload with no upload secret.
      MissingUploadSecret
    | -- | Used to reject an immutable upload with an incorrect upload secret.
      IncorrectUploadSecret
    | -- | Used to reject an immutable allocate of a size that does not match
      -- existing shares with the same storage index.
      ShareSizeMismatch
    | -- | Used to reject an immutable allocate of a size greater than the
      -- maximum allowed by the server.
      MaximumShareSizeExceeded
        { maximumShareSizeExceededLimit :: Integer
        , maximumShareSizeExceededGiven :: Integer
        }
    | -- | Used to reject an immutable write to a share that is already
      -- completely written.
      ImmutableShareAlreadyWritten
    | -- | Used to reject an immutable write to a share which has not been
      -- allocated.
      ShareNotAllocated
    | -- | Used to reject an immutable write that overlaps with data that has
      -- already been written.
      ConflictingWrite
    deriving (Ord, Eq, Show)

instance Exception WriteImmutableError

{- | Exceptional cases which might be encounted by various backend for
 operations related to mutable shares.
-}
data WriteMutableError
    = -- | Used to reject a mutable write with no write enabler secret.
      MissingWriteEnablerSecret
    | -- | Used to reject a mutable write with an incorrect write enabler secret.
      IncorrectWriteEnablerSecret
    deriving (Ord, Eq, Show)

instance Exception WriteMutableError

class Backend b where
    version :: b -> IO Version

    -- | Update the lease expiration time on the shares associated with the
    -- given storage index.
    renewLease :: b -> StorageIndex -> [LeaseSecret] -> IO ()

    createImmutableStorageIndex :: b -> StorageIndex -> Maybe [LeaseSecret] -> AllocateBuckets -> IO AllocationResult

    -- May throw ImmutableShareAlreadyWritten
    -- XXX Return should indicate what remains to be written
    writeImmutableShare :: b -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> ShareData -> Maybe ByteRange -> IO ()
    abortImmutableUpload :: b -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> IO ()
    adviseCorruptImmutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
    getImmutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    readImmutableShare :: b -> StorageIndex -> ShareNumber -> Maybe ByteRange -> IO ShareData

    -- | Read some ranges of all shares held and/or, if test conditions are
    -- met, overwrite some ranges of some shares.
    readvAndTestvAndWritev ::
        b ->
        -- | The storage index at which to operate.
        StorageIndex ->
        -- | A shared secret which the backend can use to authorize the writes.
        WriteEnablerSecret ->
        -- | The reads, tests, and writes to perform.
        ReadTestWriteVectors ->
        IO ReadTestWriteResult

    readMutableShare :: b -> StorageIndex -> ShareNumber -> Maybe ByteRange -> IO ShareData
    getMutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
