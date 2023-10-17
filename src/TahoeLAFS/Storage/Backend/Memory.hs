{-# LANGUAGE FlexibleInstances #-}

module TahoeLAFS.Storage.Backend.Memory (
    MemoryBackend (MemoryBackend),
    memoryBackend,
) where

import Control.Exception (
    throw,
 )
import Data.ByteArray (constEq)
import qualified Data.ByteString as B
import Data.IORef (
    IORef,
    atomicModifyIORef',
    modifyIORef,
    newIORef,
    readIORef,
 )
import Data.Map.Strict (
    Map,
    adjust,
    fromList,
    insert,
    keys,
    lookup,
    map,
    toList,
 )
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets),
    AllocationResult (..),
    CBORSet (..),
    ReadTestWriteResult (..),
    ReadTestWriteVectors (..),
    ShareData,
    ShareNumber,
    Size,
    StorageIndex,
    TestWriteVectors (..),
    UploadSecret,
    Version (..),
    Version1Parameters (..),
    WriteVector (..),
    shareNumbers,
 )
import TahoeLAFS.Storage.Backend (
    Backend (..),
    WriteImmutableError (ImmutableShareAlreadyWritten, IncorrectUploadSecret, ShareNotAllocated, ShareSizeMismatch),
    withUploadSecret,
 )
import Prelude hiding (
    lookup,
    map,
 )

data Share = Complete ShareData | Uploading UploadSecret ShareData deriving (Show)

data Bucket = Bucket
    { bucketSize :: Size
    , bucketShares :: Map ShareNumber Share
    }
    deriving (Show)

type ShareStorage = Map StorageIndex (Map ShareNumber ShareData)

data MemoryBackend = MemoryBackend
    { memoryBackendBuckets :: Map StorageIndex Bucket -- Completely or partially written immutable share data
    , mutableShares :: ShareStorage -- Completely written mutable shares
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
    | otherwise =
        ( backend{memoryBackendBuckets = updated}
        , result
        )
  where
    existing = Map.lookup storageIndex memoryBackendBuckets
    updated = Map.insertWith mergeBuckets storageIndex newBucket memoryBackendBuckets

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
abort storageIndex shareNumber abortSecret b@MemoryBackend{memoryBackendBuckets} = (b{memoryBackendBuckets = updated memoryBackendBuckets}, ())
  where
    updated :: Map StorageIndex Bucket -> Map StorageIndex Bucket
    updated = Map.adjust abortIt storageIndex

    abortIt :: Bucket -> Bucket
    abortIt bucket@Bucket{bucketShares} = bucket{bucketShares = Map.update abortIt' shareNumber bucketShares}

    abortIt' :: Share -> Maybe Share
    abortIt' (Uploading existingSecret _) = if constEq existingSecret abortSecret then Nothing else throw IncorrectUploadSecret
    abortIt' _ = throw ImmutableShareAlreadyWritten

writeImm ::
    StorageIndex ->
    ShareNumber ->
    B.ByteString ->
    B.ByteString ->
    MemoryBackend ->
    (MemoryBackend, ())
writeImm storageIndex shareNum uploadSecret newData b@MemoryBackend{memoryBackendBuckets}
    | isNothing share = throw ShareNotAllocated
    | otherwise = (b{memoryBackendBuckets = updated}, ())
  where
    bucket = Map.lookup storageIndex memoryBackendBuckets
    share = bucket >>= Map.lookup shareNum . bucketShares
    size = bucketSize <$> bucket
    updated = Map.adjust (\bkt -> bkt{bucketShares = Map.adjust writeToShare shareNum (bucketShares bkt)}) storageIndex memoryBackendBuckets

    writeToShare :: Share -> Share
    writeToShare (Complete _) = throw ImmutableShareAlreadyWritten
    writeToShare (Uploading existingSecret existingData)
        | authorized =
            (if Just True == (complete existingData newData <$> size) then Complete else Uploading existingSecret) (existingData <> newData)
        | otherwise = throw IncorrectUploadSecret
      where
        authorized = constEq existingSecret uploadSecret

    complete x y = (B.length x + B.length y ==) . fromIntegral

instance Show MemoryBackend where
    show _ = "<MemoryBackend>"

instance Backend (IORef MemoryBackend) where
    version backend = do
        totalSize <- readIORef backend >>= totalShareSize
        return
            Version
                { applicationVersion = "(memory)"
                , parameters =
                    Version1Parameters
                        { maximumImmutableShareSize = 1024 * 1024 * 64
                        , maximumMutableShareSize = 1024 * 1024 * 64
                        , availableSpace = (1024 * 1024 * 1024) - totalSize
                        }
                }

    getMutableShareNumbers :: IORef MemoryBackend -> StorageIndex -> IO (CBORSet ShareNumber)
    getMutableShareNumbers backend storageIndex = do
        shares' <- mutableShares <$> readIORef backend
        return $
            CBORSet . Set.fromList $
                maybe [] keys $
                    lookup storageIndex shares'

    readvAndTestvAndWritev :: IORef MemoryBackend -> StorageIndex -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readvAndTestvAndWritev
        backend
        storageIndex
        (ReadTestWriteVectors testWritev _readv) = do
            -- TODO implement readv and testv parts.
            modifyIORef backend $ \m@MemoryBackend{mutableShares} -> m{mutableShares = addShares storageIndex (shares' testWritev) mutableShares}
            return
                ReadTestWriteResult
                    { success = True
                    , readData = mempty
                    }
          where
            shares' ::
                Map ShareNumber TestWriteVectors ->
                [(ShareNumber, ShareData)]
            shares' testWritevs =
                [ (shareNumber, shareData writev)
                | (shareNumber, testWritev') <- toList testWritevs
                , writev <- write testWritev'
                ]

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
        case lookup storageIndex buckets of
            Nothing -> pure mempty
            Just bucket -> case lookup shareNum (bucketShares bucket) of
                Just (Complete shareData) -> pure shareData
                _ -> pure mempty

totalShareSize :: MemoryBackend -> IO Size
totalShareSize backend = do
    let imm = memoryBackendBuckets backend
        mut = mutableShares backend
    let immSize = sum $ map bucketTotalSize imm
    let mutSize = sum $ map length mut
    return $ toInteger $ immSize + fromIntegral mutSize

bucketTotalSize :: Bucket -> Size
bucketTotalSize Bucket{bucketSize, bucketShares} = bucketSize * fromIntegral (Map.size bucketShares)

addShares :: StorageIndex -> [(ShareNumber, ShareData)] -> ShareStorage -> ShareStorage
addShares _storageIndex [] shareStorage = shareStorage
addShares storageIndex ((shareNumber, shareData) : rest) shareStorage =
    let added = case lookup storageIndex shareStorage of
            Nothing ->
                insert storageIndex (fromList [(shareNumber, shareData)]) shareStorage
            Just _shares ->
                adjust addShare' storageIndex shareStorage
              where
                addShare' = insert shareNumber shareData
     in addShares storageIndex rest added

memoryBackend :: IO (IORef MemoryBackend)
memoryBackend = do
    newIORef $ MemoryBackend mempty mempty
