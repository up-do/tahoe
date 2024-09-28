{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Tahoe.Server (
    nullStorageServer,
    memoryStorageServer,
    directoryStorageServer,
    directoryStorageServer',
) where

import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

import Control.Exception (
    Exception,
    catch,
    throwIO,
 )
import Tahoe.CHK.Server (
    StorageServer (..),
 )
import Tahoe.CHK.Types (
    Offset,
    ShareNum,
    StorageIndex,
 )

import Data.IORef (
    IORef,
    modifyIORef',
    newIORef,
    readIORef,
 )

import System.Directory (
    createDirectoryIfMissing,
    listDirectory,
 )
import System.FilePath (
    (</>),
 )

import qualified Data.ByteString as BS
import Data.ByteString.Base32 (encodeBase32Unpadded)
import qualified Data.Text as T
import System.IO (
    IOMode (..),
    SeekMode (..),
    hSeek,
    withBinaryFile,
 )
import System.IO.Error (
    isDoesNotExistError,
 )

{- | Create a storage server backed by a certain directory which already
 exists.
-}
directoryStorageServer :: FilePath -> StorageServer
directoryStorageServer serverRoot =
    StorageServer
        { storageServerID = T.pack serverRoot
        , storageServerWrite = writeShareDataAt serverRoot
        , storageServerRead = \index sharenum ->
            withBinaryFile (sharePath serverRoot index sharenum) ReadMode BS.hGetContents
        , storageServerGetBuckets = getBuckets
        }
  where
    writeShareDataAt :: FilePath -> StorageIndex -> ShareNum -> Offset -> BS.ByteString -> IO ()
    writeShareDataAt shareRoot' storageIndex shareNum offset xs = do
        createDirectoryIfMissing True (bucketPath shareRoot' storageIndex)
        withBinaryFile (sharePath shareRoot' storageIndex shareNum) ReadWriteMode $ \f ->
            hSeek f AbsoluteSeek offset >> BS.hPut f xs

    -- Get the path to the directory where shares for the given storage
    -- index should be written.
    bucketPath :: FilePath -> StorageIndex -> FilePath
    bucketPath root storageIndex = root </> bucketName
      where
        bucketName = "shares" </> shortPiece </> fullName
        fullName = T.unpack . T.toLower . encodeBase32Unpadded $ storageIndex
        shortPiece = take 2 fullName

    -- Get the path to the file where data for the given share of the given
    -- storage index should be written.
    sharePath :: FilePath -> StorageIndex -> ShareNum -> FilePath
    sharePath root storageIndex shareNum =
        bucketPath root storageIndex </> show shareNum

    getBuckets :: StorageIndex -> IO (Set.Set ShareNum)
    getBuckets storageIndex =
        readShareFilenames `catch` doesNotExist
      where
        readShareFilenames =
            Set.fromList . map read <$> listDirectory (bucketPath serverRoot storageIndex)

        doesNotExist e =
            if isDoesNotExistError e
                then return Set.empty
                else ioError e

{- | Create a storage server backed by a certain directory which may or may
 not already exist.
-}
directoryStorageServer' :: FilePath -> IO StorageServer
directoryStorageServer' shareRoot = do
    createDirectoryIfMissing True shareRoot
    pure $ directoryStorageServer shareRoot

-- | Create a storage server backed only by in-memory data.
memoryStorageServer :: IO StorageServer
memoryStorageServer = do
    shares :: IORef (M.Map (StorageIndex, ShareNum) BS.ByteString) <- newIORef mempty

    let storageServerID = "memory"

        storageServerWrite index sharenum offset sharedata =
            modifyIORef' shares $ M.alter (appendBytes offset sharedata) (index, sharenum)

        appendBytes :: Offset -> BS.ByteString -> Maybe BS.ByteString -> Maybe BS.ByteString
        appendBytes 0 sharedata Nothing = Just sharedata
        appendBytes n _sharedata Nothing =
            error $
                "memoryStorageServer appendBytes requires append-only usage; 0 bytes written but offset is "
                    <> show n
        appendBytes n sharedata (Just existing)
            | fromIntegral (BS.length existing) /= n =
                error $
                    "memoryStorageServer appendBytes requires append-only usage; "
                        <> show (BS.length existing)
                        <> " bytes written but offset is "
                        <> show n
            | otherwise = Just (existing <> sharedata)

        storageServerRead :: StorageIndex -> ShareNum -> IO BS.ByteString
        storageServerRead index sharenum =
            fromMaybe "" . M.lookup (index, sharenum) <$> readIORef shares

        storageServerGetBuckets :: StorageIndex -> IO (Set.Set ShareNum)
        storageServerGetBuckets index =
            Set.fromList . map snd . filter ((== index) . fst) . M.keys <$> readIORef shares

    pure $ StorageServer{..}

{- | Create a StorageServer that discards writes to it and throws errors on
 reads.
-}
nullStorageServer :: StorageServer
nullStorageServer =
    StorageServer
        { storageServerID = "null-server"
        , storageServerWrite = \_index _sharenum _offset _data -> return ()
        , storageServerRead = \_index _sharenum -> throwIO IThrewYourDataAway
        , storageServerGetBuckets = \_index -> return mempty
        }

data ReadError = IThrewYourDataAway deriving (Show)
instance Exception ReadError
