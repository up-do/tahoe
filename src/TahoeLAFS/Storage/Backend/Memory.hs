{-# LANGUAGE FlexibleInstances #-}

module TahoeLAFS.Storage.Backend.Memory (
    MemoryBackend (MemoryBackend),
    memoryBackend,
    MutableShareSize (..),
    shareDataSize,
    toMutableShareSize,
) where

import Control.Exception (
    throw,
    throwIO,
 )
import Control.Foldl.ByteString (Word8)
import Data.ByteArray (constEq)
import qualified Data.ByteString as B
import Data.IORef (
    IORef,
    atomicModifyIORef',
    modifyIORef,
    newIORef,
    readIORef,
 )
import Data.Map.Merge.Strict (merge, preserveMissing, zipWithMatched)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid (Last (Last, getLast))
import qualified Data.Set as Set
import Network.HTTP.Types (ByteRange (ByteRangeFrom, ByteRangeFromTo, ByteRangeSuffix))
import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (..),
    CBORSet (..),
    Offset,
    QueryRange,
    ReadTestWriteResult (..),
    ReadTestWriteVectors (..),
    ReadVector (ReadVector, offset, readSize),
    ShareData,
    ShareNumber,
    Size,
    StorageIndex,
    TestWriteVectors (..),
    UploadSecret (UploadSecret),
    Version (..),
    Version1Parameters (..),
    WriteEnablerSecret (WriteEnablerSecret),
    WriteVector (..),
 )
import TahoeLAFS.Storage.Backend (
    Backend (..),
    WriteImmutableError (..),
    WriteMutableError (..),
    withUploadSecret,
 )
import Prelude hiding (
    lookup,
    map,
 )

data ImmutableShare = Complete ShareData | Uploading UploadSecret ShareData

data Bucket = Bucket
    { bucketSize :: Size
    , bucketShares :: Map.Map ShareNumber ImmutableShare
    }

data SecretProtected a = SecretProtected WriteEnablerSecret a

readSecret :: SecretProtected a -> WriteEnablerSecret
readSecret (SecretProtected s _) = s

readProtected :: SecretProtected a -> a
readProtected (SecretProtected _ p) = p

{- | Apply a function in a SecretProtected to a value in a SecretProtected.  The
 result is in SecretProtected with the function's secret.

 This is almost liftA2 but it's not clear to me how to have lawful handling of
 the secret.
-}
liftProtected2 :: (a -> a -> a) -> SecretProtected a -> SecretProtected a -> SecretProtected a
liftProtected2 f (SecretProtected secretL x) (SecretProtected _ y) = SecretProtected secretL (f x y)

instance Functor SecretProtected where
    fmap f (SecretProtected secret a) = SecretProtected secret (f a)

type MutableShareStorage = Map.Map StorageIndex (SecretProtected (Map.Map ShareNumber [WriteVector]))

data MutableShareSize = MutableShareSize Offset Size deriving (Show, Eq)

instance Semigroup MutableShareSize where
    (MutableShareSize writeOffsetL sizeL) <> (MutableShareSize writeOffsetR sizeR) =
        MutableShareSize minOffset maxSize
      where
        minOffset = min writeOffsetL writeOffsetR
        maxSize = max (writeOffsetL + sizeL) (writeOffsetR + sizeR) - minOffset

instance Monoid MutableShareSize where
    mempty = MutableShareSize 0 0

toMutableShareSize :: WriteVector -> MutableShareSize
toMutableShareSize (WriteVector offset bytes) = MutableShareSize offset (fromIntegral $ B.length bytes)

shareDataSize :: [WriteVector] -> Size
shareDataSize writev = offset + size
  where
    (MutableShareSize offset size) = foldMap toMutableShareSize writev

data MemoryBackend = MemoryBackend
    { memoryBackendBuckets :: Map.Map StorageIndex Bucket -- Completely or partially written immutable share data
    , mutableShares :: MutableShareStorage -- Completely written mutable shares
    }

getShareNumbers :: StorageIndex -> MemoryBackend -> CBORSet ShareNumber
getShareNumbers storageIndex backend = shareSet
  where
    shareSet = CBORSet . Set.fromList $ shareNumbers
    shareNumbers = case Map.lookup storageIndex (memoryBackendBuckets backend) of
        Nothing -> mempty
        Just bucket -> Map.keys . bucketShares $ bucket

-- Attempt to allocate space at a certain storage index for some numbered
-- shares.  The space is only allocated if there is not yet any data for those
-- share numbers at that storage index.  The modified backend and a report of
-- the allocation done are returned.
allocate ::
    -- | The storage index at which to attempt the allocation.
    StorageIndex ->
    -- | The share numbers to attempt to allocate.
    [ShareNumber] ->
    -- | A shared secret authorizing write attempts to the allocated shares.
    UploadSecret ->
    -- | The size in bytes to allocate for each share.
    Size ->
    -- | The backend in which to do the allocation.
    MemoryBackend ->
    -- | The modified backend and the results of the allocation.
    (MemoryBackend, AllocationResult)
allocate storageIndex shareNumbers uploadSecret size backend@MemoryBackend{memoryBackendBuckets}
    | maybe size bucketSize existing /= size = throw ShareSizeMismatch
    | size > maxSize =
        throw
            MaximumShareSizeExceeded
                { maximumShareSizeExceededLimit = maxSize
                , maximumShareSizeExceededGiven = size
                }
    | otherwise =
        ( backend{memoryBackendBuckets = updated}
        , result
        )
  where
    existing = Map.lookup storageIndex memoryBackendBuckets
    updated = Map.insertWith mergeBuckets storageIndex newBucket memoryBackendBuckets

    maxSize = maximumImmutableShareSize . makeVersionParams $ 0

    alreadyHave = maybe [] (Map.keys . bucketShares) existing
    allocated = filter (`notElem` alreadyHave) shareNumbers
    result = AllocationResult alreadyHave allocated

    -- Merge two buckets given precedence to the right-hand bucket for overlap.
    mergeBuckets (Bucket _ newShares) (Bucket _ oldShares) = Bucket size (newShares <> oldShares)

    -- The bucket we would allocate if there were no relevant existing state.
    newBucket = Bucket size (Map.fromList (zip shareNumbers (repeat newUpload)))
    newUpload = Uploading uploadSecret ""

abort ::
    StorageIndex ->
    ShareNumber ->
    UploadSecret ->
    MemoryBackend ->
    (MemoryBackend, ())
abort storageIndex shareNumber (UploadSecret abortSecret) b@MemoryBackend{memoryBackendBuckets} = (b{memoryBackendBuckets = updated memoryBackendBuckets}, ())
  where
    updated :: Map.Map StorageIndex Bucket -> Map.Map StorageIndex Bucket
    updated = Map.adjust abortIt storageIndex

    abortIt :: Bucket -> Bucket
    abortIt bucket@Bucket{bucketShares} = bucket{bucketShares = Map.update abortIt' shareNumber bucketShares}

    abortIt' :: ImmutableShare -> Maybe ImmutableShare
    abortIt' (Uploading (UploadSecret existingSecret) _) = if constEq existingSecret abortSecret then Nothing else throw IncorrectUploadSecret
    abortIt' _ = throw ImmutableShareAlreadyWritten

writeImm ::
    StorageIndex ->
    ShareNumber ->
    UploadSecret ->
    B.ByteString ->
    MemoryBackend ->
    (MemoryBackend, ())
writeImm storageIndex shareNum (UploadSecret uploadSecret) newData b@MemoryBackend{memoryBackendBuckets}
    | isNothing share = throw ShareNotAllocated
    | otherwise = (b{memoryBackendBuckets = updated}, ())
  where
    bucket = Map.lookup storageIndex memoryBackendBuckets
    share = bucket >>= Map.lookup shareNum . bucketShares
    size = bucketSize <$> bucket
    updated = Map.adjust (\bkt -> bkt{bucketShares = Map.adjust writeToShare shareNum (bucketShares bkt)}) storageIndex memoryBackendBuckets

    writeToShare :: ImmutableShare -> ImmutableShare
    writeToShare (Complete _) = throw ImmutableShareAlreadyWritten
    writeToShare (Uploading (UploadSecret existingSecret) existingData)
        | authorized =
            (if Just True == (complete existingData newData <$> size) then Complete else Uploading (UploadSecret existingSecret)) (existingData <> newData)
        | otherwise = throw IncorrectUploadSecret
      where
        authorized = constEq existingSecret uploadSecret

    complete x y = (B.length x + B.length y ==) . fromIntegral

instance Show MemoryBackend where
    show _ = "<MemoryBackend>"

makeVersionParams totalSize =
    Version1Parameters
        { maximumImmutableShareSize = 1024 * 1024 * 64
        , maximumMutableShareSize = 1024 * 1024 * 64
        , availableSpace = (1024 * 1024 * 1024) - totalSize
        }

instance Backend (IORef MemoryBackend) where
    version backend = do
        totalSize <- readIORef backend >>= totalShareSize
        return
            Version
                { applicationVersion = "(memory)"
                , parameters = makeVersionParams totalSize
                }

    getMutableShareNumbers :: IORef MemoryBackend -> StorageIndex -> IO (CBORSet ShareNumber)
    getMutableShareNumbers backend storageIndex = do
        sharemap <- fmap readProtected . Map.lookup storageIndex . mutableShares <$> readIORef backend
        return
            . CBORSet
            . Set.fromList
            . maybe [] Map.keys
            $ sharemap

    readvAndTestvAndWritev :: IORef MemoryBackend -> StorageIndex -> WriteEnablerSecret -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readvAndTestvAndWritev
        backend
        storageIndex
        secret
        (ReadTestWriteVectors testWritev readv) = do
            -- TODO implement testv parts.

            (CBORSet allShareNums) <- getMutableShareNumbers backend storageIndex
            let queryRange = readvToQueryRange readv

            readData <- mapM (\shareNum -> (shareNum,) <$> readMutableShare' backend storageIndex shareNum queryRange) (Set.toList allShareNums)
            outcome <- atomicModifyIORef' backend tryWrite
            case outcome of
                TestSuccess ->
                    return
                        ReadTestWriteResult
                            { readData = Map.fromList readData
                            , success = True
                            }
                TestFail ->
                    return
                        ReadTestWriteResult
                            { readData = Map.fromList readData
                            , success = False
                            }
                SecretMismatch ->
                    throwIO IncorrectWriteEnablerSecret
          where
            readvToQueryRange :: [ReadVector] -> QueryRange
            --            readvToQueryRange [] = Nothing
            readvToQueryRange rv = Just (go rv)
              where
                go [] = []
                go (r : rs) = ByteRangeFromTo off end : go rs
                  where
                    off = offset r
                    end = off + readSize r - 1

            tryWrite m@MemoryBackend{mutableShares} =
                case addShares storageIndex secret mutableShares (Map.map write testWritev) of
                    Nothing -> (m, SecretMismatch)
                    Just newShares -> (m{mutableShares = newShares}, TestSuccess)

    readMutableShare backend storageIndex shareNum queryRange =
        B.concat <$> readMutableShare' backend storageIndex shareNum queryRange

    createImmutableStorageIndex backend storageIndex secrets (AllocateBuckets shareNums size) =
        withUploadSecret secrets $ \secret ->
            atomicModifyIORef' backend (allocate storageIndex shareNums secret size)

    abortImmutableUpload backend storageIndex shareNumber secrets =
        withUploadSecret secrets $ \secret ->
            atomicModifyIORef' backend (abort storageIndex shareNumber secret)

    writeImmutableShare backend storageIndex shareNumber secrets shareData Nothing = do
        withUploadSecret secrets $ \secret ->
            atomicModifyIORef' backend (writeImm storageIndex shareNumber secret shareData)
    writeImmutableShare _ _ _ _ _ _ = error "writeImmutableShare got bad input"

    adviseCorruptImmutableShare _backend _ _ _ =
        return mempty

    getImmutableShareNumbers backend storageIndex = getShareNumbers storageIndex <$> readIORef backend

    readImmutableShare backend storageIndex shareNum _qr = do
        buckets <- memoryBackendBuckets <$> readIORef backend
        case Map.lookup storageIndex buckets of
            Nothing -> pure mempty
            Just bucket -> case Map.lookup shareNum (bucketShares bucket) of
                Just (Complete shareData) -> pure shareData
                _ -> pure mempty

totalShareSize :: MemoryBackend -> IO Size
totalShareSize backend = do
    let imm = memoryBackendBuckets backend
        mut = mutableShares backend
    let immSize = sum $ Map.map bucketTotalSize imm
    let mutSize = sum $ Map.map (length . readProtected) mut
    return $ toInteger $ immSize + fromIntegral mutSize

bucketTotalSize :: Bucket -> Size
bucketTotalSize Bucket{bucketSize, bucketShares} = bucketSize * fromIntegral (Map.size bucketShares)

addShare :: StorageIndex -> WriteEnablerSecret -> ShareNumber -> [WriteVector] -> MutableShareStorage -> MutableShareStorage
addShare storageIndex secret shareNum writev =
    Map.insertWith (liftProtected2 f) storageIndex newShare
  where
    f :: Map.Map ShareNumber [WriteVector] -> Map.Map ShareNumber [WriteVector] -> Map.Map ShareNumber [WriteVector]
    f = merge preserveMissing preserveMissing (zipWithMatched (const (<>)))

    newShare = SecretProtected secret (Map.singleton shareNum writev)

addShares :: StorageIndex -> WriteEnablerSecret -> MutableShareStorage -> Map.Map ShareNumber [WriteVector] -> Maybe MutableShareStorage
addShares storageIndex secret existing updates
    | isNothing existingSecret = Just go
    | existingSecret == Just secret = Just go
    | otherwise = Nothing
  where
    go = Map.foldrWithKey (addShare storageIndex secret) existing updates

    existingSecret = readSecret <$> Map.lookup storageIndex existing

memoryBackend :: IO (IORef MemoryBackend)
memoryBackend = do
    newIORef $ MemoryBackend mempty mempty

readMutableShare' :: IORef MemoryBackend -> StorageIndex -> ShareNumber -> QueryRange -> IO [ShareData]
readMutableShare' backend storageIndex shareNum queryRange = do
    storage <- mutableShares <$> readIORef backend
    pure $ doOneRead <$> rv storage <*> pure storage
  where
    rv :: MutableShareStorage -> [ReadVector]
    rv storage = queryRangeToReadVector storage queryRange

    getShareData storage =
        Map.lookup storageIndex storage >>= Map.lookup shareNum . readProtected

    doOneRead :: ReadVector -> MutableShareStorage -> ShareData
    doOneRead readv storage =
        maybe "" (readOneVector readv) (getShareData storage)

    queryRangeToReadVector :: MutableShareStorage -> QueryRange -> [ReadVector]
    queryRangeToReadVector storage Nothing = [ReadVector 0 size]
      where
        size = maybe 0 shareDataSize (getShareData storage)
    queryRangeToReadVector storage (Just ranges) = toReadVector <$> ranges
      where
        toReadVector (ByteRangeFrom start) = ReadVector start size
          where
            size = maybe 0 shareDataSize (getShareData storage)
        toReadVector (ByteRangeFromTo start end) = ReadVector start (end - start + 1)
        toReadVector (ByteRangeSuffix len) = ReadVector (size - len) len
          where
            size = maybe 0 shareDataSize (getShareData storage)

    readOneVector :: ReadVector -> [WriteVector] -> ShareData
    readOneVector ReadVector{offset, readSize} wv =
        B.pack (extractBytes <$> positions)
      where
        positions = [offset .. (offset + readSize - 1)]

        extractBytes :: Integer -> Word8
        extractBytes p = fromMaybe 0 (go wv)
          where
            -- New writes are added to the end of the list so give the Last
            -- write precedence over others.
            go = getLast . foldMap (Last . byteFromShare p)

        byteFromShare :: Integer -> WriteVector -> Maybe Word8
        byteFromShare p (WriteVector off bytes)
            | p >= off && p < off + fromIntegral (B.length bytes) = Just (B.index bytes (fromIntegral $ p - off))
            | otherwise = Nothing

-- | Internal type tracking the result of an attempted mutable write.
data WriteResult
    = -- | The test condition succeeded and the write was performed.
      TestSuccess
    | -- | The test condition failed and the write was not performed.
      TestFail
    | -- | The supplied secret was incorrect and the write was not performed.
      SecretMismatch
