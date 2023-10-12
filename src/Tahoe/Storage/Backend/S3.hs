{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Amazonka.S3.CreateMultipartUpload (createMultipartUploadResponse_uploadId)
import Amazonka.S3.Lens (completeMultipartUpload_multipartUpload, listObjectsResponse_contents, listObjects_prefix, object_key)
import qualified Amazonka.S3.Lens as S3
import Conduit (yield)
import Control.Lens (set, view, (.~), (?~), (^.))
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.IORef
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text as Text
import Network.HTTP.Types (ByteRange (ByteRangeFrom))
import TahoeLAFS.Storage.API
import TahoeLAFS.Storage.Backend (Backend (..))
import Text.Read (readMaybe)

-- Where's my Cow?
type AllocatedShares = Map (StorageIndex, ShareNumber) UploadState

newtype PartNumber = PartNumber Int deriving newtype (Eq)

data UploadPart
    = UploadStarted
        { uploadPartNumber :: PartNumber
        }
    | UploadFinished
        { uploadPartNumber :: PartNumber
        , uploadPartSize :: Integer
        , uploadPartResponse :: S3.UploadPartResponse
        }

data UploadState = UploadState
    { uploadStateSize :: Integer
    , uploadParts :: [UploadPart]
    , uploadResponse :: S3.CreateMultipartUploadResponse
    }

-- | Add another part upload to the given state and return the part number and upload id to use with it.
startUpload :: UploadState -> (UploadState, (PartNumber, T.Text))
startUpload u@UploadState{uploadParts, uploadResponse} = (u{uploadParts = UploadStarted partNum : uploadParts}, (partNum, view createMultipartUploadResponse_uploadId uploadResponse))
  where
    partNum = PartNumber $ length uploadParts

finishUpload :: PartNumber -> Integer -> S3.UploadPartResponse -> UploadState -> (UploadState, (Bool, [S3.CompletedPart]))
finishUpload finishedPartNum finishedPartSize response u@UploadState{uploadStateSize, uploadParts} = (u{uploadParts = newUploadParts}, (uploadedSize == uploadStateSize, mapMaybe toCompleted newUploadParts))
  where
    newUploadParts = finishPart <$> uploadParts

    finishPart c@(UploadStarted candidatePartNum)
        | finishedPartNum == candidatePartNum = UploadFinished finishedPartNum finishedPartSize response
        | otherwise = c
    finishPart f@UploadFinished{} = f

    uploadedSize = sum $ partSize <$> uploadParts

    partSize (UploadStarted _) = 0
    partSize (UploadFinished _ size _) = size

    toCompleted (UploadStarted _) = Nothing
    toCompleted (UploadFinished (PartNumber partNum) _ partResp) = completed
      where
        completed = S3.newCompletedPart partNum <$> (partResp ^. S3.uploadPartResponse_eTag)

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

    writeImmutableShare s3 storageIndex shareNum shareData Nothing = do
        -- If no range is given, this is the whole thing.
        writeImmutableShare s3 storageIndex shareNum shareData (Just [ByteRangeFrom 0])
    writeImmutableShare (S3Backend env bucketName allocatedShares) storageIndex shareNum shareData (Just byteRanges) = do
        let stateKey = (storageIndex, shareNum)
        uploadInfo <- liftIO $ atomicModifyIORef allocatedShares (adjust' startUpload stateKey)
        case uploadInfo of
            Nothing -> error "not uploading"
            Just (PartNumber partNum', uploadId) -> runResourceT $ do
                let reqBody = AWS.Chunked (AWS.ChunkedBody (AWS.ChunkSize (1024 * 1024)) (fromIntegral $ B.length shareData) (yield shareData))
                    objKey = storageIndexShareNumberToObjectKey storageIndex shareNum
                    uploadPart = S3.newUploadPart bucketName objKey partNum' uploadId reqBody
                response <- AWS.send env uploadPart
                newState <- liftIO $ atomicModifyIORef allocatedShares (adjust' (finishUpload (PartNumber partNum') (fromIntegral $ B.length shareData) response) stateKey)
                case newState of
                    Nothing ->
                        error "It went missing?"
                    Just (True, []) ->
                        error "No parts upload but we're done, huh?"
                    Just (True, p : parts) -> do
                        let completedMultipartUpload = Just $ (S3.completedMultipartUpload_parts ?~ (p :| parts)) S3.newCompletedMultipartUpload
                            completeMultipartUpload = S3.newCompleteMultipartUpload bucketName objKey uploadId

                        liftIO $ print completeMultipartUpload
                        resp <- AWS.send env ((completeMultipartUpload_multipartUpload .~ completedMultipartUpload) completeMultipartUpload)
                        liftIO $ print resp
                        pure ()
                    Just (False, _) ->
                        pure ()

    getImmutableShareNumbers (S3Backend env bucketName _) storageIndex = runResourceT $ do
        resp <- AWS.send env (set listObjects_prefix (Just . storageIndexPrefix $ storageIndex) $ S3.newListObjects bucketName)
        case view listObjectsResponse_contents resp of
            Nothing -> pure $ CBORSet mempty
            Just objects -> pure . CBORSet $ shareNumbers
              where
                shareNumbers = Set.fromList (mapMaybe objToShareNum objects)

                objToShareNum :: S3.Object -> Maybe ShareNumber
                objToShareNum obj = case T.split (== '/') (view (object_key . S3._ObjectKey) obj) of
                    ["allocated", si, shareNum] ->
                        if T.unpack si == storageIndex
                            then ShareNumber <$> readMaybe (T.unpack shareNum)
                            else Nothing
                    _ -> Nothing

storageIndexShareNumberToObjectKey :: StorageIndex -> ShareNumber -> S3.ObjectKey
storageIndexShareNumberToObjectKey si (ShareNumber sn) =
    S3.ObjectKey $ T.concat [storageIndexPrefix si, T.pack (show sn)]

storageIndexPrefix :: StorageIndex -> T.Text
storageIndexPrefix si = T.concat ["allocated/", Text.pack si, "/"]

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

adjust' :: Ord k => (v -> (v, b)) -> k -> Map.Map k v -> (Map k v, Maybe b)
adjust' f k m = case Map.lookup k m of
    Nothing -> (m, Nothing)
    Just v -> (Map.insert k newV m, Just result)
      where
        (newV, result) = f v
