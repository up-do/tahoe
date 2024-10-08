{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
    storageStartSegment,
    partitionM,
    pathOfShare,
    incomingPathOf,
) where

import Control.Exception (
    throwIO,
    try,
    tryJust,
 )
import Control.Monad (unless, when)
import Control.Monad.Extra (concatMapM)
import Data.Bifunctor (Bifunctor (bimap, second))
import Data.ByteArray (constEq)
import Data.ByteString (
    hPut,
    readFile,
    writeFile,
 )
import qualified Data.ByteString as B
import qualified Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe (
    mapMaybe,
 )
import qualified Data.Set as Set
import Network.HTTP.Types (ByteRange (..))
import System.Directory (
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    listDirectory,
    removeFile,
    renameFile,
 )
import System.FilePath (
    takeDirectory,
    (</>),
 )
import System.IO (
    Handle,
    IOMode (ReadMode, ReadWriteMode),
    SeekMode (..),
    hFileSize,
    hSeek,
    withBinaryFile,
 )
import System.IO.Error (isDoesNotExistError)
import Tahoe.Storage.Backend (
    Backend (..),
    WriteImmutableError (..),
    WriteMutableError (..),
 )
import TahoeLAFS.Storage.API (
    AllocateBuckets (..),
    AllocationResult (..),
    CBORSet (..),
    Offset,
    ReadResult,
    ReadTestWriteResult (ReadTestWriteResult, readData, success),
    ReadTestWriteVectors (ReadTestWriteVectors),
    ShareData,
    ShareNumber,
    Size,
    StorageIndex,
    TestVector (..),
    TestWriteVectors (..),
    UploadSecret (..),
    Version (..),
    Version1Parameters (..),
    WriteEnablerSecret (..),
    WriteVector (WriteVector),
    shareNumber,
 )
import qualified TahoeLAFS.Storage.API as Storage
import TahoeLAFS.Storage.Backend (withUploadSecret)
import TahoeLAFS.Storage.Backend.Util (readvToByteRange)
import Prelude hiding (
    readFile,
    writeFile,
 )

newtype FilesystemBackend = FilesystemBackend FilePath
    deriving (Show)

versionString :: Storage.ApplicationVersion
versionString = "tahoe-lafs (gbs) 0.1.0"

-- Copied from the Python implementation.  Kind of arbitrary.
maxMutableShareSize :: Storage.Size
maxMutableShareSize = 69_105 * 1_000 * 1_000 * 1_000 * 1_000

-- Perhaps should be based on the underlying filesystem?  Currently a single
-- file is used per share so we cannot hold a share larger than the
-- filesystem's largest support file.
maximumShareSize :: (Integral i) => i
maximumShareSize = fromIntegral (maxBound :: Int)

--  storage/
--  storage/shares/incoming
--    incoming/ holds temp dirs named $START/$STORAGEINDEX/$SHARENUM which will
--    be moved to storage/shares/$START/$STORAGEINDEX/$SHARENUM upon success
--  storage/shares/$START/$STORAGEINDEX
--  storage/shares/$START/$STORAGEINDEX/$SHARENUM

--  Where "$START" denotes the first 10 bits worth of $STORAGEINDEX (that's 2
--  base-32 chars).

instance Backend FilesystemBackend where
    version (FilesystemBackend _path) = do
        -- Hard-code some arbitrary amount of space.  There is a statvfs
        -- package that can inspect the system and tell us a more correct
        -- answer but it is somewhat unmaintained and fails to build in some
        -- important environments.
        let available = 1_000_000_000
        return
            Version
                { applicationVersion = versionString
                , parameters =
                    Version1Parameters
                        { maximumImmutableShareSize = maximumShareSize
                        , maximumMutableShareSize = maxMutableShareSize
                        , -- TODO: Copy the "reserved space" feature of the Python
                          -- implementation.
                          availableSpace = available
                        }
                }

    createImmutableStorageIndex backend storageIndex secrets params =
        withUploadSecret secrets $ \uploadSecret -> do
            let exists = haveShare backend storageIndex
            (alreadyHave, allocated) <- partitionM exists (shareNumbers params)
            mapM_ (flip (allocate backend storageIndex (allocatedSize params)) uploadSecret) allocated
            return
                AllocationResult
                    { alreadyHave = alreadyHave
                    , allocated = allocated
                    }

    -- TODO Handle ranges.
    -- TODO Make sure the share storage was allocated.
    -- TODO Don't allow target of rename to exist.
    -- TODO Concurrency
    writeImmutableShare (FilesystemBackend root) storageIndex shareNumber' secrets shareData Nothing =
        withUploadSecret secrets $ \uploadSecret -> do
            alreadyHave <- haveShare (FilesystemBackend root) storageIndex shareNumber'
            if alreadyHave
                then throwIO ImmutableShareAlreadyWritten
                else do
                    let finalSharePath = pathOfShare root storageIndex shareNumber'
                    let incomingSharePath = incomingPathOf root storageIndex shareNumber'

                    secretCheck <- try $ checkUploadSecret incomingSharePath uploadSecret
                    case secretCheck of
                        Left e
                            | isDoesNotExistError e -> throwIO ShareNotAllocated
                            | otherwise -> throwIO e
                        Right () -> do
                            writeFile incomingSharePath shareData
                            let createParents = True
                            createDirectoryIfMissing createParents $ takeDirectory finalSharePath
                            removeFileIfExists (secretPath incomingSharePath)
                            renameFile incomingSharePath finalSharePath

    abortImmutableUpload (FilesystemBackend root) storageIndex shareNumber' secrets =
        withUploadSecret secrets $ \uploadSecret -> do
            let incomingSharePath = incomingPathOf root storageIndex shareNumber'
            checkUploadSecret incomingSharePath uploadSecret
            removeFileIfExists incomingSharePath
            removeFileIfExists (secretPath incomingSharePath)

    getImmutableShareNumbers (FilesystemBackend root) storageIndex = do
        let storageIndexPath = pathOfStorageIndex root storageIndex
        storageIndexChildren <-
            tryJust (Just . isDoesNotExistError) $ listDirectory storageIndexPath
        let sharePaths =
                case storageIndexChildren of
                    Left _ -> []
                    Right children -> children
        return $ CBORSet . Set.fromList $ mapMaybe (shareNumber . read) sharePaths

    -- TODO Handle ranges.
    -- TODO Make sure the share storage was allocated.
    readImmutableShare (FilesystemBackend root) storageIndex shareNum _qr =
        let _storageIndexPath = pathOfStorageIndex root storageIndex
            readShare = readFile . pathOfShare root storageIndex
         in readShare shareNum

    getMutableShareNumbers = getImmutableShareNumbers

    readMutableShare backend storageIndex shareNum Nothing =
        readMutableShare backend storageIndex shareNum (Just $ ByteRangeFrom 0)
    readMutableShare (FilesystemBackend root) storageIndex shareNum (Just ranges) =
        readMutableShare' root storageIndex shareNum ranges

    readvAndTestvAndWritev
        backend@(FilesystemBackend root)
        storageIndex
        secret
        (ReadTestWriteVectors testWritev readv) = do
            checkWriteEnabler secret (pathOfStorageIndex root storageIndex)
            readData <- runAllReads
            success <- fmap and . concatMapM (uncurry checkTestVectors . second test) . Map.toList $ testWritev
            when success $ mapM_ applyWriteVectors $ Map.toList testWritev
            return
                ReadTestWriteResult
                    { success = success
                    , readData = readData
                    }
          where
            runAllReads :: IO ReadResult
            runAllReads = do
                (CBORSet allShareNumbers) <- getMutableShareNumbers backend storageIndex
                let allShareNumbers' = Set.toList allShareNumbers
                Map.fromList . zip allShareNumbers' <$> mapM readvOneShare allShareNumbers'

            readvOneShare :: ShareNumber -> IO [ShareData]
            readvOneShare shareNum = mapM (readMutableShare' root storageIndex shareNum) . fmap readvToByteRange $ readv

            checkTestVectors :: ShareNumber -> [TestVector] -> IO [Bool]
            checkTestVectors = mapM . checkTestVector

            checkTestVector :: ShareNumber -> TestVector -> IO Bool
            checkTestVector shareNum TestVector{..} =
                (specimen ==) <$> readMutableShare' root storageIndex shareNum (ByteRangeFromTo testOffset (testOffset + testSize - 1))

            applyWriteVectors ::
                (ShareNumber, TestWriteVectors) ->
                IO ()
            applyWriteVectors (shareNumber', testWriteVectors) =
                mapM_ (applyShareWrite shareNumber') (write testWriteVectors)

            applyShareWrite ::
                ShareNumber ->
                WriteVector ->
                IO ()
            applyShareWrite shareNumber' (WriteVector offset shareData) = do
                createDirectoryIfMissing createParents $ takeDirectory sharePath
                withBinaryFile sharePath ReadWriteMode (writeAtPosition offset shareData)
              where
                sharePath = pathOfShare root storageIndex shareNumber'
                createParents = True

            writeAtPosition ::
                Offset ->
                ShareData ->
                Handle ->
                IO ()
            writeAtPosition offset shareData' handle = do
                hSeek handle AbsoluteSeek offset
                hPut handle shareData'

-- Does the given backend have the complete share indicated?
haveShare ::
    FilesystemBackend -> -- The backend to check
    StorageIndex -> -- The storage index the share belongs to
    ShareNumber -> -- The number of the share
    IO Bool -- True if it has the share, False otherwise.
haveShare (FilesystemBackend path) storageIndex shareNumber' =
    doesPathExist $ pathOfShare path storageIndex shareNumber'

pathOfStorageIndex ::
    FilePath -> -- The storage backend root path
    StorageIndex -> -- The storage index to consider
    FilePath -- The path to the directory containing shares for the
    -- storage index.
pathOfStorageIndex root storageIndex =
    root </> "shares" </> storageStartSegment storageIndex </> storageIndex

pathOfShare :: FilePath -> StorageIndex -> ShareNumber -> FilePath
pathOfShare root storageIndex shareNumber' =
    pathOfStorageIndex root storageIndex </> show (Storage.toInteger shareNumber')

incomingPathOf :: FilePath -> StorageIndex -> ShareNumber -> FilePath
incomingPathOf root storageIndex shareNumber' =
    root </> "shares" </> "incoming" </> storageStartSegment storageIndex </> storageIndex </> show (Storage.toInteger shareNumber')

storageStartSegment :: StorageIndex -> FilePath
storageStartSegment [] = fail "illegal short storage index"
storageStartSegment [_] = storageStartSegment []
storageStartSegment (a : b : _) = [a, b]

-- Create spaces to write data for several incoming shares.
allocate ::
    FilesystemBackend ->
    StorageIndex ->
    Size ->
    ShareNumber ->
    UploadSecret ->
    IO ()
allocate (FilesystemBackend root) storageIndex size shareNum (UploadSecret secret)
    | size > maximumShareSize =
        throwIO
            MaximumShareSizeExceeded
                { maximumShareSizeExceededLimit = maximumShareSize
                , maximumShareSizeExceededGiven = size
                }
    | otherwise =
        let sharePath = incomingPathOf root storageIndex shareNum
            shareDirectory = takeDirectory sharePath
            createParents = True
         in do
                createDirectoryIfMissing createParents shareDirectory
                writeFile (secretPath sharePath) secret
                writeFile sharePath ""
                return ()

{- | Given the path of an immutable share, construct a path to use to hold the
 upload secret for that share.
-}
secretPath :: FilePath -> FilePath
secretPath = (<> ".secret")

{- | Compare the upload secret for an immutable share at a given path to a
 given upload secret and produce unit if and only if they are equal.

 If they are not, throw IncorrectUploadSecret.
-}
checkUploadSecret :: FilePath -> UploadSecret -> IO ()
checkUploadSecret sharePath (UploadSecret uploadSecret) = do
    matches <- constEq uploadSecret <$> readFile (secretPath sharePath)
    unless matches (throwIO IncorrectUploadSecret)

-- | Partition a list based on the result of a monadic predicate.
partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred' items = bimap (fst <$>) (fst <$>) . Data.List.partition snd . zip items <$> mapM pred' items

{- | Throw IncorrectUploadSecret if the given secret does not match the
 existing secret for the given storage index path or succeed with () if it
 does.  If there is no secret yet, record the given one and succeed with ().
-}
checkWriteEnabler :: WriteEnablerSecret -> FilePath -> IO ()
checkWriteEnabler (WriteEnablerSecret given) storageIndexPath = do
    x <- try . B.readFile $ path
    case x of
        Left e
            | isDoesNotExistError e -> do
                -- If there is no existing value, this check initializes it to the given
                -- value.
                createDirectoryIfMissing True (takeDirectory path)
                B.writeFile path given
            | otherwise -> throwIO e
        Right existing
            | WriteEnablerSecret given == WriteEnablerSecret existing -> pure ()
            | otherwise -> throwIO IncorrectWriteEnablerSecret
  where
    path = secretPath storageIndexPath

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists p =
    try (removeFile p) >>= \case
        Left e
            | isDoesNotExistError e -> pure ()
            | otherwise -> throwIO e
        Right () -> pure ()

readMutableShare' :: FilePath -> StorageIndex -> ShareNumber -> ByteRange -> IO ShareData
readMutableShare' root storageIndex shareNum range =
    withBinaryFile path ReadMode (readTheRange range)
  where
    path = pathOfShare root storageIndex shareNum

    readTheRange (ByteRangeFrom start) shareFile = do
        hSeek shareFile AbsoluteSeek start
        B.hGetContents shareFile
    readTheRange (ByteRangeFromTo start end) shareFile = do
        hSeek shareFile AbsoluteSeek start
        B.hGetSome shareFile (fromIntegral $ end - start + 1)
    readTheRange (ByteRangeSuffix len) shareFile = do
        realSize <- hFileSize shareFile
        hSeek shareFile SeekFromEnd (-(min realSize len))
        B.hGetContents shareFile
