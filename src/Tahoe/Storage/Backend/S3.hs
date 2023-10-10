{-# LANGUAGE NamedFieldPuns #-}

module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import Amazonka.Auth
import qualified Amazonka.S3 as S3
import Control.Monad
import TahoeLAFS.Storage.API (AllocateBuckets (..), AllocationResult (..))
import TahoeLAFS.Storage.Backend (Backend (..))

data S3Backend = S3Backend AWS.Env S3.BucketName

instance Backend S3Backend where
    createImmutableStorageIndex (S3Backend env name) storageIndex (AllocateBuckets{shareNumbers, allocatedSize}) =
        runResourceT $ do
            objects <- AWS.send env (S3.newListObjectsV2 name)
            pure $ AllocationResult{alreadyHave = [], allocated = []}

{-
TODO: fill in more methods
1. writeImmutableShare
2. readImmutableShare
would give us a round-trip
the flow is
1. create immutable storage index
2. write immutable share
3. read immutable share

the class that needs methods implemented:
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
    readMutableShare :: b -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    getMutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()

-}
