{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import Amazonka.S3 (newPutObject)
import qualified Amazonka.S3 as S3
import Amazonka.S3.Lens
import qualified Amazonka.S3.Lens as S3
import Conduit (sinkList)
import Control.Exception (throwIO)
import Control.Lens (set, view, (?~), (^.))
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Bifunctor (Bifunctor (first))
import Data.ByteArray (constEq)
import qualified Data.ByteString as B
import Data.IORef (
    IORef,
    atomicModifyIORef,
    modifyIORef',
    newIORef,
    readIORef,
 )
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text as Text
import Network.HTTP.Types (ByteRange (ByteRangeFrom))
import TahoeLAFS.Storage.API (
    AllocateBuckets (AllocateBuckets, allocatedSize, shareNumbers),
    AllocationResult (AllocationResult, allocated, alreadyHave),
    CBORSet (CBORSet),
    LeaseSecret (Upload),
    ReadTestWriteResult (ReadTestWriteResult),
    ReadTestWriteVectors (..),
    ShareNumber (..),
    StorageIndex,
    UploadSecret,
 )
import TahoeLAFS.Storage.Backend (
    Backend (
        abortImmutableUpload,
        createImmutableStorageIndex,
        getImmutableShareNumbers,
        getMutableShareNumbers,
        readImmutableShare,
        readvAndTestvAndWritev,
        writeImmutableShare
    ),
    WriteImmutableError (
        ImmutableShareAlreadyWritten,
        IncorrectUploadSecret,
        MissingUploadSecret
    ),
    withUploadSecret,
 )
import Text.Read (readMaybe)

newtype PartNumber = PartNumber Int
    deriving newtype (Eq)
    deriving (Show)

data UploadPart
    = UploadStarted
        { uploadPartNumber :: PartNumber
        }
    | UploadFinished
        { uploadPartNumber :: PartNumber
        , uploadPartSize :: Integer
        , uploadPartResponse :: S3.UploadPartResponse
        }
    deriving (Eq, Show)

-- Where's my Cow? -- Terry Pratchett

data UploadState = UploadState
    { uploadStateSize :: Integer
    , uploadParts :: [UploadPart]
    , uploadResponse :: S3.CreateMultipartUploadResponse
    , uploadSecret :: UploadSecret
    }
    deriving (Eq, Show)

-- | where's mah parts? Here they are!
type AllocatedShares = Map (StorageIndex, ShareNumber) UploadState

-- | This saves in-progress uploads so we can finish or abort
data S3Backend = S3Backend
    { s3BackendEnv :: AWS.Env
    , s3BackendBucket :: S3.BucketName
    , s3BackendPrefix :: T.Text
    , s3BackendState :: IORef AllocatedShares
    }

-- | Add another part upload to the given state and return the part number and upload id to use with it.
startUpload :: UploadState -> (UploadState, (PartNumber, T.Text))
startUpload u@UploadState{uploadParts, uploadResponse} = (u{uploadParts = UploadStarted partNum : uploadParts}, (partNum, view createMultipartUploadResponse_uploadId uploadResponse))
  where
    partNum = PartNumber $ 1 + length uploadParts

{- | Mark a part upload as finished and compute the new overall state of the
 multipart upload.
-}
finishUpload :: PartNumber -> Integer -> S3.UploadPartResponse -> UploadState -> (Maybe UploadState, (Bool, [S3.CompletedPart]))
finishUpload finishedPartNum finishedPartSize response u@UploadState{uploadStateSize, uploadParts} =
    ( if multipartFinished then Nothing else Just u{uploadParts = newUploadParts}
    ,
        ( multipartFinished
        , mapMaybe toCompleted newUploadParts
        )
    )
  where
    multipartFinished = newUploadedSize == uploadStateSize
    newUploadParts = finishPart <$> uploadParts
    newUploadedSize = sum $ partSize <$> newUploadParts

    finishPart c@(UploadStarted candidatePartNum)
        | finishedPartNum == candidatePartNum = UploadFinished finishedPartNum finishedPartSize response
        | otherwise = c
    finishPart f@UploadFinished{} = f

    partSize UploadStarted{} = 0
    partSize UploadFinished{uploadPartSize} = uploadPartSize

    toCompleted (UploadStarted _) = Nothing
    toCompleted (UploadFinished (PartNumber partNum) _ partResp) = completed
      where
        completed = S3.newCompletedPart partNum <$> (partResp ^. S3.uploadPartResponse_eTag)

{- | Instantiate a new S3 backend which will interact with objects in the
 given bucket using the given AWS Env.
-}
newS3Backend :: AWS.Env -> S3.BucketName -> T.Text -> IO S3Backend
newS3Backend s3BackendEnv s3BackendBucket s3BackendPrefix = do
    s3BackendState <- newIORef mempty
    pure S3Backend{..}

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
    createImmutableStorageIndex (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix, s3BackendState}) storageIndex mbLeaseSecret (AllocateBuckets{shareNumbers, allocatedSize})
        -- The maximum S3 object size is 5 BT
        | allocatedSize > 5 * 1024 * 1024 * 1024 * 1024 = error "blub"
        | otherwise = withUploadSecret mbLeaseSecret $ \uploadSecret ->
            runResourceT $
                do
                    -- switch to sendEither when more brain is available
                    -- XXX Check to see if we already have local state related to this upload
                    -- XXX Check to see if S3 already has state related to this upload
                    -- XXX Use thread-safe primitives so the server can be multithreaded
                    let objectKeys = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex <$> shareNumbers
                        uploadCreates = S3.newCreateMultipartUpload s3BackendBucket <$> objectKeys
                    -- XXX HOW TO HANDLE FAILURE? this AWS.send should really be AWS.sendEither !
                    -- and then we handle errors that we can
                    multipartUploadResponse <- mapM (fmap (flip (UploadState allocatedSize []) uploadSecret) . AWS.send s3BackendEnv) uploadCreates
                    let keys = zip (repeat storageIndex) shareNumbers :: [(StorageIndex, ShareNumber)]
                        newMap = Map.fromList $ zip keys multipartUploadResponse
                    -- don't send anything that's already there
                    liftIO $ modifyIORef' s3BackendState (newMap `Map.union`)
                    pure $ AllocationResult{alreadyHave = [], allocated = shareNumbers}

    writeImmutableShare _s3 _storageIndex _shareNum Nothing _shareData _ = throwIO MissingUploadSecret
    writeImmutableShare s3 storageIndex shareNum mbLeaseSecret shareData Nothing = do
        -- If no range is given, this is the whole thing.
        writeImmutableShare s3 storageIndex shareNum mbLeaseSecret shareData (Just [ByteRangeFrom 0])
    writeImmutableShare s3backend@(S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix, s3BackendState}) storageIndex shareNum mbLeaseSecret shareData (Just _byteRanges) = do
        -- call list objects on the backend, if this share already exists, reject the new write with ImmutableShareAlreadyWritten
        CBORSet shareNumbers <- getImmutableShareNumbers s3backend storageIndex
        when (shareNum `elem` shareNumbers) $ throwIO ImmutableShareAlreadyWritten

        thing <- readIORef s3BackendState
        let stateKey = (storageIndex, shareNum)
        let leaseSecret = uploadSecret <$> Map.lookup stateKey thing :: Maybe UploadSecret
        unless (validUploadSecret leaseSecret mbLeaseSecret) $ throwIO IncorrectUploadSecret

        uploadInfo <- liftIO $ atomicModifyIORef s3BackendState (adjust' (first Just . startUpload) stateKey)
        case uploadInfo of
            Nothing -> throwIO ImmutableShareAlreadyWritten
            Just (PartNumber partNum', uploadId) -> runResourceT $ do
                let objKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
                    uploadPart = S3.newUploadPart s3BackendBucket objKey partNum' uploadId (AWS.toBody shareData)
                response <- AWS.send s3BackendEnv uploadPart
                newState <- liftIO $ atomicModifyIORef s3BackendState (adjust' (finishUpload (PartNumber partNum') (fromIntegral $ B.length shareData) response) stateKey)
                case newState of
                    Nothing ->
                        error "It went missing?"
                    Just (True, []) ->
                        error "No parts upload but we're done, huh?"
                    Just (True, p : parts) -> do
                        let completedMultipartUpload = (S3.completedMultipartUpload_parts ?~ (p :| parts)) S3.newCompletedMultipartUpload
                            completeMultipartUpload =
                                (completeMultipartUpload_multipartUpload ?~ completedMultipartUpload)
                                    (S3.newCompleteMultipartUpload s3BackendBucket objKey uploadId)

                        void $ AWS.send s3BackendEnv completeMultipartUpload
                    Just (False, _) -> pure ()

    abortImmutableUpload :: S3Backend -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> IO ()
    abortImmutableUpload _ _ _ Nothing = throwIO MissingUploadSecret
    abortImmutableUpload (S3Backend{s3BackendState}) storageIndex shareNum mbLeaseSecret = do
        thing <- readIORef s3BackendState
        let leaseSecret = uploadSecret <$> Map.lookup (storageIndex, shareNum) thing :: Maybe UploadSecret
        -- XXX WOW THIS IS WRONG, COME BACK AND FIX IT PLEASE
        -- TODO: tell s3 we're done with it plz
        if validUploadSecret leaseSecret mbLeaseSecret then pure () else throwIO IncorrectUploadSecret

    getImmutableShareNumbers (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex = runResourceT $ do
        resp <- AWS.send s3BackendEnv (set listObjects_prefix (Just . storageIndexPrefix s3BackendPrefix $ storageIndex) $ S3.newListObjects s3BackendBucket)
        case view listObjectsResponse_contents resp of
            Nothing -> pure $ CBORSet mempty
            Just objects -> pure $ CBORSet shareNumbers
              where
                shareNumbers = Set.fromList (mapMaybe objToShareNum objects)

                objToShareNum :: S3.Object -> Maybe ShareNumber
                objToShareNum obj =
                    case parsed of
                        [prefix, "allocated", si, shareNum] ->
                            if prefix == s3BackendPrefix && T.unpack si == storageIndex
                                then ShareNumber <$> readMaybe (T.unpack shareNum)
                                else Nothing
                        _ -> Nothing
                  where
                    parsed = T.split (== '/') (view (object_key . S3._ObjectKey) obj)

    readImmutableShare (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex shareNum _range = runResourceT $ do
        resp <- AWS.send s3BackendEnv (S3.newGetObject s3BackendBucket objectKey)
        B.concat <$> AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList
      where
        objectKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum

    readvAndTestvAndWritev :: S3Backend -> StorageIndex -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readvAndTestvAndWritev S3Backend{s3BackendPrefix, s3BackendBucket, s3BackendEnv} storageIndex (ReadTestWriteVectors{testWriteVectors}) = do
        -- for every ShareNumber key in ReadTestWriteVectors create an empty s3 object
        let objectsToPut = (newPutObject s3BackendBucket . storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex <$> Map.keys testWriteVectors) <*> [""]
        _something <- runResourceT $ mapM (AWS.send s3BackendEnv) objectsToPut
        pure $ ReadTestWriteResult True mempty

    getMutableShareNumbers = getImmutableShareNumbers

storageIndexShareNumberToObjectKey :: T.Text -> StorageIndex -> ShareNumber -> S3.ObjectKey
storageIndexShareNumberToObjectKey prefix si (ShareNumber sn) =
    S3.ObjectKey $ T.concat [storageIndexPrefix prefix si, T.pack (show sn)]

storageIndexPrefix :: T.Text -> StorageIndex -> T.Text
storageIndexPrefix prefix si = T.concat [prefix, "/allocated/", Text.pack si, "/"]

{-
the remaining methods to implement:
class Backend b where
    version :: b -> IO Version

    -- | Update the lease expiration time on the shares associated with the
    -- given storage index.
    renewLease :: b -> StorageIndex -> [LeaseSecret] -> IO ()

    adviseCorruptImmutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()

    createMutableStorageIndex :: b -> StorageIndex -> AllocateBuckets -> IO AllocationResult
    readvAndTestvAndWritev :: b -> StorageIndex -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readMutableShare :: b -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    getMutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
-}

adjust' :: Ord k => (v -> (Maybe v, b)) -> k -> Map.Map k v -> (Map k v, Maybe b)
adjust' f k m = case Map.lookup k m of
    Nothing -> (m, Nothing)
    Just v -> (Map.update (const newV) k m, Just result)
      where
        (newV, result) = f v

validUploadSecret :: Maybe UploadSecret -> Maybe [LeaseSecret] -> Bool
validUploadSecret (Just us1) (Just [Upload us2]) = constEq us1 us2
validUploadSecret _ _ = False
