module TahoeLAFS.Storage.Backend.Memory (
    MemoryBackend (MemoryBackend),
    memoryBackend,
) where

import Prelude hiding (
    lookup,
    map,
 )

import Network.HTTP.Types (
    ByteRanges,
 )

import Control.Exception (
    throwIO,
 )
import Data.Maybe (fromMaybe)

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
    filterWithKey,
    fromList,
    insert,
    keys,
    lookup,
    map,
    toList,
 )
import qualified Data.Set as Set

import TahoeLAFS.Storage.API (
    AllocateBuckets,
    AllocationResult (..),
    CBORSet (..),
    CorruptionDetails,
    LeaseSecret,
    Offset,
    QueryRange,
    ReadResult,
    ReadTestWriteResult (..),
    ReadTestWriteVectors (..),
    ShareData,
    ShareNumber,
    Size,
    StorageIndex,
    TestWriteVectors (..),
    Version (..),
    Version1Parameters (..),
    WriteVector (..),
    shareNumbers,
 )

import TahoeLAFS.Storage.Backend (
    Backend (..),
    ImmutableShareAlreadyWritten (ImmutableShareAlreadyWritten),
 )

type ShareStorage = Map StorageIndex (Map ShareNumber ShareData)
type BucketStorage = Map StorageIndex (Map ShareNumber (Size, ShareData))

data MemoryBackend = MemoryBackend
    { immutableShares :: IORef ShareStorage -- Completely written immutable shares
    , mutableShares :: IORef ShareStorage -- Completely written mutable shares
    , buckets :: IORef BucketStorage -- In-progress immutable share uploads
    }

instance Show MemoryBackend where
    show _ = "<MemoryBackend>"

instance Backend MemoryBackend where
    version backend = do
        totalSize <- totalShareSize backend
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

    getMutableShareNumbers :: MemoryBackend -> StorageIndex -> IO (CBORSet ShareNumber)
    getMutableShareNumbers backend storageIndex = do
        shares' <- readIORef $ mutableShares backend
        return $
            CBORSet . Set.fromList $
                maybe [] keys $
                    lookup storageIndex shares'

    readvAndTestvAndWritev :: MemoryBackend -> StorageIndex -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readvAndTestvAndWritev
        backend
        storageIndex
        (ReadTestWriteVectors testWritev _readv) = do
            -- TODO implement readv and testv parts.
            let shares = mutableShares backend
            modifyIORef shares $ addShares storageIndex (shares' testWritev)
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

    createImmutableStorageIndex _backend _idx secrets params =
        return
            AllocationResult
                { alreadyHave = mempty
                , allocated = shareNumbers params
                }

    writeImmutableShare backend storageIndex shareNumber _secrets shareData Nothing = do
        -- shares <- readIORef (immutableShares backend) -- XXX uh, is this right?!
        changed <- atomicModifyIORef' (immutableShares backend) $
            \shares ->
                case lookup storageIndex shares >>= lookup shareNumber of
                    Just _ ->
                        -- It is not allowed to write new data for an immutable share that
                        -- has already been written.
                        (shares, False)
                    Nothing ->
                        (addShares storageIndex [(shareNumber, shareData)] shares, True)
        if changed
            then return ()
            else throwIO ImmutableShareAlreadyWritten
    writeImmutableShare _ _ _ _ _ _ = error "writeImmutableShare got bad input"

    adviseCorruptImmutableShare _backend _ _ _ =
        return mempty

    getImmutableShareNumbers backend storageIndex = do
        shares' <- readIORef $ immutableShares backend
        return $ CBORSet . Set.fromList $ maybe [] keys $ lookup storageIndex shares'

    readImmutableShare backend storageIndex shareNum _qr = do
        shares' <- readIORef $ immutableShares backend
        let result = case lookup storageIndex shares' of
                Nothing -> mempty
                Just shares'' -> lookup shareNum shares''
        pure $ fromMaybe mempty result

totalShareSize :: MemoryBackend -> IO Size
totalShareSize backend = do
    imm <- readIORef $ immutableShares backend
    mut <- readIORef $ mutableShares backend
    let immSize = sum $ map length imm
    let mutSize = sum $ map length mut
    return $ toInteger $ immSize + mutSize

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

memoryBackend :: IO MemoryBackend
memoryBackend = do
    immutableShares <- newIORef mempty
    mutableShares <- newIORef mempty
    buckets <- newIORef mempty
    return $ MemoryBackend immutableShares mutableShares buckets
