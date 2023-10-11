{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Control.Monad.IO.Class
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import TahoeLAFS.Storage.API
import TahoeLAFS.Storage.Backend (Backend (..))

-- Where's my Cow?
type AllocatedShares = Map (StorageIndex, ShareNumber) S3.CreateMultipartUploadResponse

data S3Backend = S3Backend AWS.Env S3.BucketName (IORef AllocatedShares)

instance Backend S3Backend where
    createImmutableStorageIndex (S3Backend env bucketName allocatedShares) storageIndex (AllocateBuckets{shareNumbers, allocatedSize}) =
        runResourceT $ do
            -- switch to sendEither when more brain is available
            let objectKeys = (storageIndexShareNumberToObjectKey storageIndex <$> shareNumbers)
                uploadCreates = S3.newCreateMultipartUpload bucketName <$> objectKeys
            -- XXX HOW TO HANDLE FAILURE? this AWS.send should really be AWS.sendEither !
            -- and then we handle errors that we can
            multipartUploadResponse <- mapM (AWS.send env) uploadCreates
            let keys = zip (repeat storageIndex) shareNumbers :: [(StorageIndex, ShareNumber)]
                newMap = Map.fromList $ zip keys multipartUploadResponse
            -- don't send anything that's already there
            liftIO $ modifyIORef allocatedShares (newMap `Map.union`) -- (Map.insert storageIndexShareNumberToObjectKey storageIndex)
            pure $ AllocationResult{alreadyHave = [], allocated = []}

storageIndexShareNumberToObjectKey :: StorageIndex -> ShareNumber -> S3.ObjectKey
storageIndexShareNumberToObjectKey si sn = S3.ObjectKey $ Text.pack $ si <> show sn

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
