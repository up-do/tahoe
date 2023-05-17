{-# LANGUAGE DataKinds #-}

module TahoeLAFS.Storage.Backend (
    Backend (..),
    ImmutableShareAlreadyWritten (ImmutableShareAlreadyWritten),
    writeMutableShare,
) where

import Control.Exception (
    Exception,
    throw,
 )

import Data.Map.Strict (
    fromList,
 )

import qualified Data.Set as Set
import Network.HTTP.Types (
    ByteRanges,
 )
import TahoeLAFS.Storage.API (
    AllocateBuckets,
    AllocationResult,
    CBOR,
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
    SlotSecrets,
    StorageIndex,
    TestWriteVectors (..),
    Version,
    WriteVector (..),
 )

data ImmutableShareAlreadyWritten = ImmutableShareAlreadyWritten
    deriving (Show)
instance Exception ImmutableShareAlreadyWritten

class Backend b where
    version :: b -> IO Version

    -- | Update the lease expiration time on the shares associated with the
    -- given storage index.
    renewLease :: b -> StorageIndex -> [LeaseSecret] -> IO ()

    createImmutableStorageIndex :: b -> StorageIndex -> AllocateBuckets -> IO AllocationResult

    -- May throw ImmutableShareAlreadyWritten
    writeImmutableShare :: b -> StorageIndex -> ShareNumber -> ShareData -> Maybe ByteRanges -> IO ()
    adviseCorruptImmutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
    getImmutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    readImmutableShare :: b -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData

    createMutableStorageIndex :: b -> StorageIndex -> AllocateBuckets -> IO AllocationResult
    readvAndTestvAndWritev :: b -> StorageIndex -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readMutableShares :: b -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    getMutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()

writeMutableShare ::
    Backend b =>
    b ->
    SlotSecrets ->
    StorageIndex ->
    ShareNumber ->
    ShareData ->
    Maybe ByteRanges ->
    IO ()
writeMutableShare b secrets storageIndex shareNumber shareData Nothing = do
    let testWriteVectors =
            fromList
                [
                    ( shareNumber
                    , TestWriteVectors
                        { test = []
                        , write =
                            [ WriteVector
                                { writeOffset = 0
                                , shareData = shareData
                                }
                            ]
                        }
                    )
                ]
    let vectors =
            ReadTestWriteVectors
                { secrets = secrets
                , testWriteVectors = testWriteVectors
                , readVector = mempty
                }
    result <- readvAndTestvAndWritev b storageIndex vectors
    if success result
        then return ()
        else throw WriteRefused
writeMutableShare _ _ _ _ _ _ = error "writeMutableShare got bad input"

data WriteRefused = WriteRefused deriving (Show, Eq)
instance Exception WriteRefused
