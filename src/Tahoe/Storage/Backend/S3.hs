{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Amazonka.S3.CreateMultipartUpload (createMultipartUploadResponse_uploadId)
import Conduit (yield)
import Control.Lens (view)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text as Text
import TahoeLAFS.Storage.API
import TahoeLAFS.Storage.Backend (Backend (..))

-- Where's my Cow?
type AllocatedShares = Map (StorageIndex, ShareNumber) UploadState

newtype UploadPart = UploadPart S3.UploadPartResponse

data UploadState = UploadState
    { uploadStateSize :: Integer
    , uploadParts :: [UploadPart]
    , uploadResponse :: S3.CreateMultipartUploadResponse
    }

data S3Backend = S3Backend AWS.Env S3.BucketName (IORef AllocatedShares)

{- | Instantiate a new S3 backend which will interact with objects in the
 given bucket using the given AWS Env.
-}
newS3Backend :: AWS.Env -> S3.BucketName -> IO S3Backend
newS3Backend env bucketName = S3Backend env bucketName <$> newIORef mempty

{- | A storage backend which keeps shares in objects in buckets behind and
 S3-compatible API.

  The backend is configured with credentials to use the API and the name of a
  bucket to use.

  Object keys like `allocated/<storage index>/<share number>` are used to
  allocated and incompletely written shares.  Certain metadata required to
  complete the upload is recorded in tags on the object.

  On completion of immutable upload, objects are renamed to keys like
  `shares/<storage index>/<share number>`.
-}
instance Backend S3Backend where
    createImmutableStorageIndex (S3Backend env bucketName allocatedShares) storageIndex (AllocateBuckets{shareNumbers, allocatedSize})
        -- The maximum S3 object size is 5 BT
        | allocatedSize > 5 * 1024 * 1024 * 1024 * 1024 = error "blub"
        | otherwise = runResourceT $ do
            -- switch to sendEither when more brain is available
            -- XXX Check to see if we already have local state related to this upload
            -- XXX Check to see if S3 already has state related to this upload
            -- XXX Use thread-safe primitives so the server can be multithreaded
            let objectKeys = storageIndexShareNumberToObjectKey storageIndex <$> shareNumbers
                uploadCreates = S3.newCreateMultipartUpload bucketName <$> objectKeys
            -- XXX HOW TO HANDLE FAILURE? this AWS.send should really be AWS.sendEither !
            -- and then we handle errors that we can
            multipartUploadResponse <- mapM (fmap (UploadState allocatedSize []) . AWS.send env) uploadCreates
            let keys = zip (repeat storageIndex) shareNumbers :: [(StorageIndex, ShareNumber)]
                newMap = Map.fromList $ zip keys multipartUploadResponse
            -- don't send anything that's already there
            liftIO $ modifyIORef allocatedShares (newMap `Map.union`)
            pure $ AllocationResult{alreadyHave = [], allocated = shareNumbers}

    writeImmutableShare (S3Backend env bucketName allocatedShares) storageIndex shareNumber shareData byteRange = do
        state <- Map.lookup (storageIndex, shareNumber) <$> readIORef allocatedShares
        case state of
            Nothing -> error "not uploading"
            Just ustate@UploadState{uploadStateSize, uploadParts, uploadResponse} -> runResourceT $ do
                let reqBody = AWS.Chunked (AWS.ChunkedBody (AWS.ChunkSize (1024 * 1024)) (fromIntegral $ B.length shareData) (yield shareData))
                response <- AWS.send env (S3.newUploadPart bucketName (storageIndexShareNumberToObjectKey storageIndex shareNumber) (length uploadParts + 1) (view createMultipartUploadResponse_uploadId uploadResponse) reqBody)
                let uploadPart = UploadPart response
                -- XXX If we just finished the last piece, also send the CompleteMultipartUpload bit and then clean up our local bookkeeping.
                liftIO $ modifyIORef allocatedShares (Map.insert (storageIndex, shareNumber) (ustate{uploadParts = uploadPart : uploadParts}))

storageIndexShareNumberToObjectKey :: StorageIndex -> ShareNumber -> S3.ObjectKey
storageIndexShareNumberToObjectKey si sn =
    S3.ObjectKey $ T.concat ["allocated/", Text.pack si, "/", T.pack (show sn)]

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
