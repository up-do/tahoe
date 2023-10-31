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
    tryJust,
 )
import Control.Monad (unless)
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteArray (constEq)
import Data.ByteString (
    hPut,
    readFile,
    writeFile,
 )
import qualified Data.List
import Data.Map.Strict (
    toList,
 )
import Data.Maybe (
    mapMaybe,
 )
import qualified Data.Set as Set
import System.Directory (
    createDirectoryIfMissing,
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
    IOMode (ReadWriteMode),
    SeekMode (AbsoluteSeek),
    hSeek,
    withBinaryFile,
 )
import System.IO.Error (
    isDoesNotExistError,
 )
import TahoeLAFS.Storage.API (
    AllocateBuckets (..),
    AllocationResult (..),
    CBORSet (..),
    Offset,
    ReadTestWriteResult (ReadTestWriteResult, readData, success),
    ReadTestWriteVectors (ReadTestWriteVectors),
    ShareData,
    ShareNumber,
    StorageIndex,
    TestWriteVectors (write),
    UploadSecret (..),
    Version (..),
    Version1Parameters (..),
    WriteVector (WriteVector),
    shareNumber,
 )
import qualified TahoeLAFS.Storage.API as Storage
import TahoeLAFS.Storage.Backend (
    Backend (..),
    WriteImmutableError (ImmutableShareAlreadyWritten, IncorrectUploadSecret),
    withUploadSecret,
 )
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
                        { maximumImmutableShareSize = available
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
            mapM_ (flip (allocate backend storageIndex) uploadSecret) allocated
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
                    checkUploadSecret incomingSharePath uploadSecret
                    writeFile incomingSharePath shareData
                    let createParents = True
                    createDirectoryIfMissing createParents $ takeDirectory finalSharePath
                    removeFile (secretPath incomingSharePath)
                    renameFile incomingSharePath finalSharePath

    abortImmutableUpload (FilesystemBackend root) storageIndex shareNumber' secrets =
        withUploadSecret secrets $ \uploadSecret -> do
            let incomingSharePath = incomingPathOf root storageIndex shareNumber'
            checkUploadSecret incomingSharePath uploadSecret
            removeFile incomingSharePath

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

    readvAndTestvAndWritev
        (FilesystemBackend root)
        storageIndex
        _secrets
        (ReadTestWriteVectors testWritev _readv) = do
            -- TODO implement readv and testv parts.
            -- TODO implement secrets
            mapM_ (applyWriteVectors root storageIndex) $ toList testWritev
            return
                ReadTestWriteResult
                    { success = True
                    , readData = mempty
                    }
          where
            applyWriteVectors ::
                FilePath ->
                StorageIndex ->
                (ShareNumber, TestWriteVectors) ->
                IO ()
            applyWriteVectors _root _storageIndex (shareNumber', testWriteVectors) =
                mapM_ (applyShareWrite root storageIndex shareNumber') (write testWriteVectors)

            applyShareWrite ::
                FilePath ->
                StorageIndex ->
                ShareNumber ->
                WriteVector ->
                IO ()
            applyShareWrite _root _storageIndex shareNumber' (WriteVector offset shareData) =
                let sharePath = pathOfShare root storageIndex shareNumber'
                    createParents = True
                 in do
                        createDirectoryIfMissing createParents $ takeDirectory sharePath
                        withBinaryFile sharePath ReadWriteMode (writeAtPosition offset shareData)
              where
                writeAtPosition ::
                    Offset ->
                    ShareData ->
                    Handle ->
                    IO ()
                writeAtPosition _offset shareData' handle = do
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
    ShareNumber ->
    UploadSecret ->
    IO ()
allocate (FilesystemBackend root) storageIndex shareNum (UploadSecret secret) =
    let sharePath = incomingPathOf root storageIndex shareNum
        shareDirectory = takeDirectory sharePath
        createParents = True
     in do
            createDirectoryIfMissing createParents shareDirectory
            writeFile (secretPath sharePath) secret
            writeFile sharePath ""
            return ()

secretPath = (<> ".secret")

checkUploadSecret :: FilePath -> UploadSecret -> IO ()
checkUploadSecret sharePath (UploadSecret uploadSecret) = do
    matches <- constEq uploadSecret <$> readFile (secretPath sharePath)
    unless matches (throwIO IncorrectUploadSecret)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred' items = bimap (fst <$>) (fst <$>) . Data.List.partition snd . zip items <$> mapM pred' items
