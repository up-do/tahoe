module Tahoe.Storage.Backend.S3 where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import qualified Amazonka.S3.Lens as S3
import Conduit (ResourceT, sinkList)
import Control.Concurrent.Async (concurrently, mapConcurrently, mapConcurrently_)
import Control.Concurrent.STM.Lifted (STM, atomically)
import qualified Control.Concurrent.STM.Map as SMap
import Control.Exception (Exception, SomeException, catch, throw, throwIO, try)
import Control.Lens (set, view, (.~), (?~), (^.))
import Control.Monad (void, when)
import Control.Monad.Catch (MonadCatch, onException)
import Data.Bifunctor (Bifunctor (first, second))
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C8
import Data.Either (rights)
import Data.Foldable (fold, foldl')
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable (hashWithSalt))
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Stack (HasCallStack)
import Network.HTTP.Types (ByteRange (ByteRangeFrom, ByteRangeFromTo), Status (Status, statusCode))
import Network.HTTP.Types.Header (renderByteRange)
import Tahoe.Storage.Backend (
    AllocateBuckets (AllocateBuckets, allocatedSize, shareNumbers),
    AllocationResult (..),
    Backend (..),
    CBORSet (..),
    LeaseSecret (..),
    QueryRange,
    ReadTestWriteResult (..),
    ReadTestWriteVectors (..),
    ReadVector (..),
    ShareData,
    ShareNumber (..),
    StorageIndex,
    TestOperator (Eq),
    TestVector (..),
    TestWriteVectors (..),
    UploadSecret,
    Version (Version),
    Version1Parameters (..),
    WriteEnablerSecret (..),
    WriteImmutableError (..),
    WriteMutableError (..),
    WriteVector (..),
 )
import TahoeLAFS.Storage.Backend (
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

-- | This saves in-progress uploads so we can finish or abort
data UploadState = UploadState
    { uploadStateSize :: Integer
    , uploadParts :: [UploadPart]
    , -- | Nothing after we initialize our internal state but are on our way to
      -- initializing our external state, then Just some value after external
      -- state is initialized.
      uploadResponse :: Maybe S3.CreateMultipartUploadResponse
    , uploadSecret :: UploadSecret
    }
    deriving (Eq)

-- | where's mah parts? Here they are!
type AllocatedShares = SMap.Map (StorageIndex, ShareNumber) UploadState

{- | A storage backend for tahoe-great-black-swamp which relies on S3 objects
 to store immutable and mutable Tahoe-LAFS objects.
-}
data S3Backend = S3Backend
    { -- | The Amazonka context with which to make requests of S3.
      s3BackendEnv :: AWS.Env
    , -- | The bucket into which all Tahoe-LAFS objects will be placed.
      s3BackendBucket :: S3.BucketName
    , -- | A prefix to use when naming all S3 objects placed in the bucket.
      s3BackendPrefix :: T.Text
    , -- | A record of state related to operations that are currently in
      -- progress.  For example, immutable shares might have been allocated but
      -- all bytes may not yet have been uploaded.
      s3BackendState :: AllocatedShares
    }

internalAllocate :: StorageIndex -> UploadSecret -> Integer -> ShareNumber -> AllocatedShares -> STM Bool
internalAllocate storageIndex secret allocatedSize shareNumber curState = do
    SMap.member (storageIndex, shareNumber) curState >>= \case
        True -> pure False
        False -> do
            SMap.insert (storageIndex, shareNumber) newShareState curState
            pure True
  where
    newShareState =
        UploadState
            { uploadStateSize = allocatedSize
            , uploadParts = []
            , uploadResponse = Nothing
            , uploadSecret = secret
            }

undoInternalAllocate :: StorageIndex -> ShareNumber -> AllocatedShares -> STM ()
undoInternalAllocate storageIndex shareNum = SMap.delete (storageIndex, shareNum)

internalAllocateComplete :: StorageIndex -> ShareNumber -> S3.CreateMultipartUploadResponse -> AllocatedShares -> STM ()
internalAllocateComplete storageIndex shareNum response curState = do
    SMap.lookup stateKey curState >>= \case
        Just state ->
            SMap.insert stateKey state{uploadResponse = Just response} curState
        Nothing ->
            error "Oh nooo contact Shae in building 42"
  where
    stateKey = (storageIndex, shareNum)

{- | If the identified share has been allocated and the given secrets match
 its upload secret then return new state including one new part upload for
 that share and that part upload's state.  Otherwise, throw an error.
-}
internalStartUpload ::
    -- | Secrets provided to authorize this upload.
    Maybe [LeaseSecret] ->
    -- | The storage index of the share the upload will be part of.
    StorageIndex ->
    -- | The number of the share the upload will be part of.
    ShareNumber ->
    -- | The current total share state.
    AllocatedShares ->
    -- | The updated total share state and the state necessary to complete the
    -- upload to the S3 server.
    STM (Maybe PartUpload)
internalStartUpload secrets storageIndex shareNum curState = do
    uploadState <- SMap.lookup stateKey curState
    let secret = uploadSecret <$> uploadState
    case secret of
        Nothing -> throw ShareNotAllocated
        Just secret'
            | validUploadSecret secret' secrets ->
                adjust' (first Just . startPartUpload) stateKey curState
            | otherwise -> throw IncorrectUploadSecret
  where
    stateKey = (storageIndex, shareNum)

internalFinishUpload ::
    Int ->
    C8.ByteString ->
    StorageIndex ->
    ShareNumber ->
    S3.UploadPartResponse ->
    AllocatedShares ->
    STM (Maybe (Bool, [S3.CompletedPart]))
internalFinishUpload partNum' shareData storageIndex shareNum response =
    adjust' (finishPartUpload (PartNumber partNum') (fromIntegral $ B.length shareData) response) stateKey
  where
    stateKey = (storageIndex, shareNum)

infiniteRetry :: MonadCatch m => m b -> m b
infiniteRetry a = a `onException` infiniteRetry a

externalAllocate :: S3Backend -> StorageIndex -> ShareNumber -> ResourceT IO S3.CreateMultipartUploadResponse
externalAllocate S3Backend{..} storageIndex shareNum =
    -- XXX HOW TO HANDLE FAILURE? this AWS.send should really be AWS.sendEither !
    -- and then we handle errors that we can
    infiniteRetry $ AWS.send s3BackendEnv uploadCreate
  where
    objectKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
    uploadCreate = S3.newCreateMultipartUpload s3BackendBucket objectKey

-- runResourceT $
--     do
--         -- switch to sendEither when more brain is available
--         -- XXX Check to see if we already have local state related to this upload
--         -- XXX Check to see if S3 already has state related to this upload
--         -- XXX Use thread-safe primitives so the server can be multithreaded
--         let objectKeys = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex <$> shareNumbers
--             uploadCreates = S3.newCreateMultipartUpload s3BackendBucket <$> objectKeys
--         -- XXX HOW TO HANDLE FAILURE? this AWS.send should really be AWS.sendEither !
--         -- and then we handle errors that we can
--         multipartUploadResponse <- mapM (fmap (flip (UploadState allocatedSize []) uploadSecret) . AWS.send s3BackendEnv) uploadCreates
--         let keys = zip (repeat storageIndex) shareNumbers :: [(StorageIndex, ShareNumber)]
--             newMap = Map.fromList $ zip keys multipartUploadResponse
--         -- don't send anything that's already there
--         liftIO $ modifyIORef' s3BackendState (newMap `Map.union`)
--         pure $ AllocationResult{alreadyHave = [], allocated = shareNumbers}

data PartUpload = Started PartNumber T.Text | NotReady

-- | Add another part upload to the given state and return the part number and upload id to use with it.
startPartUpload :: UploadState -> (UploadState, PartUpload)
startPartUpload u@UploadState{uploadParts, uploadResponse} = result
  where
    result = case view S3.createMultipartUploadResponse_uploadId <$> uploadResponse of
        Nothing -> (u, NotReady)
        Just uploadId -> (u{uploadParts = UploadStarted partNum : uploadParts}, Started partNum uploadId)

    partNum = PartNumber $ 1 + length uploadParts

{- | Mark a part upload as finished and compute the new overall state of the
 multipart upload.
-}
finishPartUpload :: PartNumber -> Integer -> S3.UploadPartResponse -> UploadState -> (Maybe UploadState, (Bool, [S3.CompletedPart]))
finishPartUpload finishedPartNum finishedPartSize response u@UploadState{uploadStateSize, uploadParts} =
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
    -- XXX Load existing s3BackendState from S3 itself.  List the
    -- MultipartUploads and figure out what is in progress.  Also need to
    -- encode uploadStateSize and uploadSecret in the MultipartUpload somehow,
    -- probably.
    s3BackendState <- atomically SMap.empty

    pure S3Backend{..}

-- The maximum S3 object size is 5 TB.  We currently add no bookkeeping
-- overhead so the client-supplied share can use all of this.
maxShareSize :: Integer
maxShareSize = 5 * 2 ^ (40 :: Integer)

{- | Something has gone wrong with the backend that does not fit into the
 generic exceptions defined by tahoe-great-black-swamp.
-}
data BackendError
    = ShareAllocationIncomplete
    deriving (Eq, Show)

instance Exception BackendError

{- | A storage backend which keeps shares in objects in buckets behind and
 S3-compatible API.

  The backend is configured with credentials to use the API and the name of a
  bucket to use.

  Uploads for immutable objects are handled using S3 "multipart uploads" to
  send the data from each write made on to us onwards to S3 as a separate
  part.  The overall upload is completed when the last write is received.

  Objects are given S3 keys like `<storage index>/<share number>`.
-}
instance Backend S3Backend where
    version _ = pure $ Version params appVer
      where
        params =
            Version1Parameters
                { maximumImmutableShareSize = maxShareSize
                , maximumMutableShareSize = maxShareSize
                , -- Just estimate ...
                  availableSpace = 2 ^ (50 :: Integer)
                }

        appVer = "tahoe-s3/0.0.0.0"

    -- Open a logical transaction for creating some new shares at a certain
    -- storage index.
    --
    -- For each share number, this can succeed if and only if that share
    -- number has not already been created at the given storage index.
    --
    -- Each share number for which this operation succeeds will be included in
    -- the "allocated" set of the result.
    --
    -- Each share number for which creation has already *completed* will be
    -- included in the "alreadyHave" set of the result.
    --
    -- Each share number which is in the process of being created will be
    -- included in neither set.
    --
    -- Creation may also fail because of an error from the S3 server.  In this
    -- case, the affected share's state is not changed but if there were other
    -- shares mentioned in the request their state may be changed.
    createImmutableStorageIndex :: S3Backend -> StorageIndex -> Maybe [LeaseSecret] -> AllocateBuckets -> IO AllocationResult
    createImmutableStorageIndex s3 storageIndex secrets AllocateBuckets{..}
        | allocatedSize > maxShareSize = throwIO $ MaximumShareSizeExceeded maxShareSize allocatedSize
        | otherwise =
            -- Creation may also fail because no upload secret has been given.
            withUploadSecret secrets $ \uploadSecret -> do
                -- Try to allocate each share from the request.
                results <-
                    mapConcurrently
                        (try @SomeException . createOneImmutableShare s3 storageIndex allocatedSize uploadSecret)
                        shareNumbers
                -- Combine all of the successful allocations for the response
                -- to the caller.  We expect that an errors encountered are
                -- already handled sufficiently to leave state consistent.
                pure . fold . rights $ results

    writeImmutableShare :: S3Backend -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> ShareData -> QueryRange -> IO ()
    writeImmutableShare _s3 _storageIndex _shareNum Nothing _shareData _ = throwIO MissingUploadSecret
    writeImmutableShare s3 storageIndex shareNum secrets shareData Nothing = do
        -- If no range is given, this is the whole thing.
        writeImmutableShare s3 storageIndex shareNum secrets shareData (Just [ByteRangeFrom 0])
    writeImmutableShare s3backend@(S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix, s3BackendState}) storageIndex shareNum secrets shareData (Just _byteRanges) = do
        -- XXX Do something with _byteRanges
        --
        -- call list objects on the backend, if this share already exists, reject the new write with ImmutableShareAlreadyWritten
        CBORSet shareNumbers <- getImmutableShareNumbers s3backend storageIndex
        when (shareNum `elem` shareNumbers) $ throwIO ImmutableShareAlreadyWritten

        -- If possible, update internal state to record a new part upload
        -- being attempted.
        uploadInfo <- atomically (internalStartUpload secrets storageIndex shareNum s3BackendState)

        case uploadInfo of
            Nothing -> throwIO ImmutableShareAlreadyWritten
            Just NotReady -> throwIO ShareAllocationIncomplete
            Just (Started (PartNumber partNum') uploadId) -> do
                let objKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
                    uploadPart = S3.newUploadPart s3BackendBucket objKey partNum' uploadId (AWS.toBody shareData)
                -- XXX This could, of course, fail...  Then we have some state to roll back.
                response <- runResourceT $ AWS.send s3BackendEnv uploadPart
                newState <- atomically $ internalFinishUpload partNum' shareData storageIndex shareNum response s3BackendState
                case newState of
                    Nothing ->
                        error "It went missing?"
                    Just (True, []) ->
                        error "No parts upload but we're done, huh?"
                    Just (True, p : parts) -> do
                        let completedMultipartUpload = (S3.completedMultipartUpload_parts ?~ (p :| parts)) S3.newCompletedMultipartUpload
                            completeMultipartUpload =
                                (S3.completeMultipartUpload_multipartUpload ?~ completedMultipartUpload)
                                    (S3.newCompleteMultipartUpload s3BackendBucket objKey uploadId)

                        -- XXX Oops, this could fail too.
                        void $ runResourceT $ AWS.send s3BackendEnv completeMultipartUpload
                    Just (False, _) -> pure ()

    abortImmutableUpload :: S3Backend -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> IO ()
    abortImmutableUpload _ _ _ Nothing = throwIO MissingUploadSecret
    abortImmutableUpload (S3Backend{..}) storageIndex shareNum secrets = do
        -- Try to find the matching upload state and discard it if the secret matches.
        toCancel <- atomically $ adjust' internalCancel stateKey s3BackendState
        -- If we found it, also cancel the state in the S3 server.
        maybe (throwIO IncorrectUploadSecret) cancelMultipartUpload toCancel
      where
        stateKey = (storageIndex, shareNum)

        cancelMultipartUpload uploadId = runResourceT $ do
            -- XXX Check the response for errors
            _resp <- AWS.send s3BackendEnv $ S3.newAbortMultipartUpload s3BackendBucket (storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum) uploadId
            pure ()

        internalCancel UploadState{uploadResponse, uploadSecret}
            | validUploadSecret uploadSecret secrets =
                maybe (throw ShareAllocationIncomplete) ((Nothing,) . (^. S3.createMultipartUploadResponse_uploadId)) uploadResponse
            | otherwise = throw IncorrectUploadSecret

    getImmutableShareNumbers (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex = runResourceT $ do
        resp <- AWS.send s3BackendEnv (set S3.listObjects_prefix (Just . storageIndexPrefix s3BackendPrefix $ storageIndex) $ S3.newListObjects s3BackendBucket)
        case view S3.listObjectsResponse_contents resp of
            Nothing -> pure $ CBORSet mempty
            Just objects -> pure $ CBORSet shareNumbers
              where
                shareNumbers = Set.fromList (mapMaybe objToShareNum objects)

                objToShareNum :: S3.Object -> Maybe ShareNumber
                objToShareNum obj =
                    case parsed of
                        [prefix, si, shareNum] ->
                            if prefix == s3BackendPrefix && T.unpack si == storageIndex
                                then ShareNumber <$> readMaybe (T.unpack shareNum)
                                else Nothing
                        _ -> Nothing
                  where
                    parsed = T.split (== '/') (view (S3.object_key . S3._ObjectKey) obj)

    readImmutableShare :: S3Backend -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    readImmutableShare (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex shareNum qrange =
        runResourceT $ do
            huh <- readEach qrange
            pure $ B.concat huh
      where
        objectKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum
        readEach :: Maybe [ByteRange] -> ResourceT IO [ShareData]
        readEach Nothing = do
            resp <- AWS.send s3BackendEnv (S3.newGetObject s3BackendBucket objectKey)
            pure . B.concat <$> AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList
        readEach (Just ranges) = mapM readOne ranges
        readOne :: ByteRange -> ResourceT IO ShareData
        readOne br = do
            let getObj = set S3.getObject_range (Just . ("bytes=" <>) . T.pack . C8.unpack $ renderByteRange br) $ S3.newGetObject s3BackendBucket objectKey
            resp <- AWS.send s3BackendEnv getObj
            B.concat <$> AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList

    readvAndTestvAndWritev :: HasCallStack => S3Backend -> StorageIndex -> WriteEnablerSecret -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readvAndTestvAndWritev s3@S3Backend{s3BackendPrefix, s3BackendBucket, s3BackendEnv} storageIndex secret (ReadTestWriteVectors{readVector, testWriteVectors}) =
        -- XXX
        --
        -- 1. Check results from S3 for errors
        --
        -- 2. Chunk data somehow to avoid catastrophic performance problems
        -- from the current strategy of whole-object updates for every write.
        --
        case testWriteVectorList of
            [] ->
                -- There are no writes so skip the secret check and the test/write logic.
                ReadTestWriteResult True <$> doReads
            (arbShareNum, _) : _ -> do
                expectedSecret <-
                    ( metadataWriteEnabler . fst
                            <$> readShare s3 storageIndex arbShareNum (Just [ByteRangeFromTo 0 0])
                        )
                        `catch` on404 Nothing

                when (isJust expectedSecret && Just secret /= expectedSecret) $ throwIO IncorrectWriteEnablerSecret

                (readResults, testResults) <- concurrently doReads doTests
                if and . fmap and $ testResults
                    then ReadTestWriteResult True readResults <$ doWrites
                    else pure $ ReadTestWriteResult False readResults
      where
        testWriteVectorList = Map.toList testWriteVectors

        doReads = do
            (CBORSet knownShareNumbers) <- getMutableShareNumbers s3 storageIndex
            let shareNumberList = Set.toList knownShareNumbers
            shareData <- mapConcurrently id $ readMutableShare s3 storageIndex <$> shareNumberList <*> pure (toQueryRange readVector)
            pure $ Map.fromList (zip shareNumberList ((: []) <$> shareData))

        toQueryRange = Just . fmap toSingleRange
          where
            toSingleRange (ReadVector offset size) = ByteRangeFromTo offset (offset + size - 1)

        doTests =
            mapConcurrently
                (\(shareNum, testWriteVectors') -> zipWith runTestVector (test testWriteVectors') <$> mapM (readSomeThings shareNum) (test testWriteVectors'))
                testWriteVectorList

        doWrites =
            mapConcurrently_
                (\(shareNum, testWriteVectors') -> readEverything shareNum >>= writeEverything shareNum . (`applyWriteVectors` write testWriteVectors'))
                testWriteVectorList

        runTestVector (TestVector _ _ Eq specimen) shareData = specimen == shareData
        runTestVector _ _ = False

        readSomeThings shareNum (TestVector offset size _ _) =
            readMutableShare s3 storageIndex shareNum (Just [ByteRangeFromTo offset (offset + size - 1)])
                `catch` on404 ""

        readEverything shareNum =
            readMutableShare s3 storageIndex shareNum Nothing `catch` on404 ""

        writeEverything shareNum bs =
            runResourceT $
                AWS.send
                    s3BackendEnv
                    ( (S3.putObject_metadata .~ HashMap.singleton writeEnablerSecretMetadataKey (base64WriteEnabler secret)) $
                        S3.newPutObject
                            s3BackendBucket
                            (storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum)
                            (AWS.toBody bs)
                    )

        base64WriteEnabler (WriteEnablerSecret bytes) = T.decodeLatin1 . Base64.encode $ bytes

    getMutableShareNumbers = getImmutableShareNumbers

    readMutableShare :: S3Backend -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    readMutableShare = readImmutableShare

{- | Given the complete bytes of a share, apply a single write vector and
 return the modified bytes.
-}
applyWriteVector :: B.ByteString -> WriteVector -> B.ByteString
applyWriteVector bs (WriteVector offset shareData) =
    prefix <> filler <> shareData <> suffix
  where
    prefix = B.take (fromIntegral offset) bs
    suffix = B.drop (fromIntegral offset + B.length shareData) bs
    filler = B.replicate (fromIntegral offset - B.length bs) 0

-- | Given the complete bytes of a share, apply a
applyWriteVectors :: Foldable f => B.ByteString -> f WriteVector -> B.ByteString
applyWriteVectors = foldl' applyWriteVector

storageIndexShareNumberToObjectKey :: T.Text -> StorageIndex -> ShareNumber -> S3.ObjectKey
storageIndexShareNumberToObjectKey prefix si (ShareNumber sn) =
    S3.ObjectKey $ T.concat [storageIndexPrefix prefix si, T.pack (show sn)]

storageIndexPrefix :: T.Text -> StorageIndex -> T.Text
storageIndexPrefix prefix si = T.concat [prefix, "/", T.pack si, "/"]

{-
the remaining methods to implement:
class Backend b where
    -- | Update the lease expiration time on the shares associated with the
    -- given storage index.
    renewLease :: b -> StorageIndex -> [LeaseSecret] -> IO ()

    adviseCorruptImmutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
    adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
-}

{- | If a map includes the given key, compute a new value and a result value for
 the current value.  If the new computed value is Nothing the key is removed.
 If the map does not include the key, do nothing.  Otherwise, replace the old
 value with the new value and return the result value.
-}
adjust' :: Hashable k => (v -> (Maybe v, b)) -> k -> SMap.Map k v -> STM (Maybe b)
adjust' f k m = do
    SMap.lookup k m >>= \case
        Nothing -> pure Nothing
        Just v -> do
            maybe (SMap.delete k m) (\v' -> SMap.insert k v' m) newV
            pure $ Just result
          where
            (newV, result) = f v

validUploadSecret :: UploadSecret -> Maybe [LeaseSecret] -> Bool
validUploadSecret us1 (Just [Upload us2]) = us1 == us2
validUploadSecret _ _ = False

data Metadata = Metadata
    { metadataWriteEnabler :: Maybe WriteEnablerSecret
    }

readShare :: S3Backend -> StorageIndex -> ShareNumber -> QueryRange -> IO (Metadata, ShareData)
readShare (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) storageIndex shareNum qrange =
    runResourceT $ second B.concat <$> readEach qrange
  where
    objectKey = storageIndexShareNumberToObjectKey s3BackendPrefix storageIndex shareNum

    readEach :: Maybe [ByteRange] -> ResourceT IO (Metadata, [ShareData])
    readEach Nothing = do
        resp <- AWS.send s3BackendEnv (S3.newGetObject s3BackendBucket objectKey)
        body <- AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList
        pure (readMetadata resp, body)
    readEach (Just []) = pure (Metadata Nothing, [])
    readEach (Just ranges) = do
        (metadata : _, shareData) <- unzip <$> mapM readOne ranges
        pure (metadata, shareData)

    readOne :: ByteRange -> ResourceT IO (Metadata, ShareData)
    readOne br = do
        let getObj =
                set S3.getObject_range (Just . ("bytes=" <>) . T.pack . C8.unpack $ renderByteRange br) $
                    S3.newGetObject s3BackendBucket objectKey
        resp <- AWS.send s3BackendEnv getObj
        body <- B.concat <$> AWS.sinkBody (resp ^. S3.getObjectResponse_body) sinkList
        pure (readMetadata resp, body)

writeEnablerSecretMetadataKey :: T.Text
writeEnablerSecretMetadataKey = "write-enabler-secret"

readMetadata :: S3.GetObjectResponse -> Metadata
readMetadata resp =
    Metadata
        { metadataWriteEnabler = (either (const Nothing) (Just . WriteEnablerSecret) . Base64.decode . T.encodeUtf8) =<< HashMap.lookup writeEnablerSecretMetadataKey (resp ^. S3.getObjectResponse_metadata)
        }

on404 :: a -> AWS.Error -> IO a
on404 result (AWS.ServiceError AWS.ServiceError'{status = Status{statusCode = 404}}) = pure result
on404 _ e = throwIO e

{- | Allocate tracking state, locally and in the S3 server, for a new share of
 the given size at the given location.
-}
createOneImmutableShare ::
    S3Backend ->
    StorageIndex ->
    Integer ->
    UploadSecret ->
    ShareNumber ->
    IO AllocationResult
createOneImmutableShare s3@S3Backend{..} storageIndex allocatedSize uploadSecret shareNum = do
    -- Initialize local tracking state
    allocated <- atomically $ internalAllocate storageIndex uploadSecret allocatedSize shareNum s3BackendState

    if allocated
        then do
            -- Create corresponding state in the S3 server.
            response <-
                runResourceT (externalAllocate s3 storageIndex shareNum) `onException` do
                    atomically (undoInternalAllocate storageIndex shareNum s3BackendState)

            -- Update the local state with the details of the allocations
            -- on the S3 server.  We need the multipart upload identifier
            -- to associate future part uploads with the right thing.
            atomically $ internalAllocateComplete storageIndex shareNum response s3BackendState

            pure mempty{allocated = [shareNum]}
        else pure mempty{alreadyHave = [shareNum]}

-- XXX Take these from tahoe-great-black-swamp-types
instance Semigroup AllocationResult where
    AllocationResult xa ya <> AllocationResult xb yb = AllocationResult (xa <> xb) (ya <> yb)

instance Monoid AllocationResult where
    mempty = AllocationResult mempty mempty

instance Hashable ShareNumber where
    hashWithSalt i (ShareNumber num) = hashWithSalt i num
