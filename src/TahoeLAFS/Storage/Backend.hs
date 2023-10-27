{-# LANGUAGE DataKinds #-}

module TahoeLAFS.Storage.Backend (
    Backend (..),
    WriteImmutableError (..),
    writeMutableShare,
    withUploadSecret,
) where

import Control.Exception (
    Exception,
    throw,
    throwIO,
 )

import Data.Map.Strict (
    fromList,
 )

import Network.HTTP.Types (
    ByteRanges,
 )
import TahoeLAFS.Storage.API (
    AllocateBuckets,
    AllocationResult,
    CBORSet (..),
    CorruptionDetails,
    LeaseSecret (..),
    QueryRange,
    ReadTestWriteResult (..),
    ReadTestWriteVectors (..),
    ShareData,
    ShareNumber,
    StorageIndex,
    TestWriteVectors (..),
    UploadSecret (..),
    Version,
    WriteEnablerSecret,
    WriteVector (..),
    isUploadSecret,
 )

data WriteImmutableError
    = MissingUploadSecret
    | ShareSizeMismatch
    | ImmutableShareAlreadyWritten
    | ShareNotAllocated
    | IncorrectUploadSecret
    deriving (Ord, Eq, Show)
instance Exception WriteImmutableError

class Backend b where
    version :: b -> IO Version

    -- | Update the lease expiration time on the shares associated with the
    -- given storage index.
    renewLease :: b -> StorageIndex -> [LeaseSecret] -> IO ()

    createImmutableStorageIndex :: b -> StorageIndex -> Maybe [LeaseSecret] -> AllocateBuckets -> IO AllocationResult

    -- May throw ImmutableShareAlreadyWritten
    writeImmutableShare :: b -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> ShareData -> Maybe ByteRanges -> IO ()
    abortImmutableUpload :: b -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> IO ()
    adviseCorruptImmutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()
    getImmutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    readImmutableShare :: b -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData

    readvAndTestvAndWritev :: b -> StorageIndex -> Maybe [LeaseSecret] -> ReadTestWriteVectors -> IO ReadTestWriteResult
    readMutableShare :: b -> StorageIndex -> ShareNumber -> QueryRange -> IO ShareData
    getMutableShareNumbers :: b -> StorageIndex -> IO (CBORSet ShareNumber)
    adviseCorruptMutableShare :: b -> StorageIndex -> ShareNumber -> CorruptionDetails -> IO ()

writeMutableShare ::
    Backend b =>
    b ->
    StorageIndex ->
    ShareNumber ->
    WriteEnablerSecret ->
    ShareData ->
    Maybe ByteRanges ->
    IO ()
writeMutableShare b storageIndex shareNumber writeEnablerSecret shareData Nothing = do
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
                        , newLength = Nothing -- XXX expose this?
                        }
                    )
                ]
    let vectors =
            ReadTestWriteVectors
                { testWriteVectors = testWriteVectors
                , readVector = mempty
                }
    result <- readvAndTestvAndWritev b storageIndex (Just [Write writeEnablerSecret]) vectors
    if success result
        then return ()
        else throw WriteRefused
writeMutableShare _ _ _ _ _ _ = error "writeMutableShare got bad input"

data WriteRefused = WriteRefused deriving (Show, Eq)
instance Exception WriteRefused

withUploadSecret :: Maybe [LeaseSecret] -> (UploadSecret -> IO a) -> IO a
withUploadSecret ss f =
    case filter isUploadSecret <$> ss of
        Just [Upload s] -> f s
        _ -> throwIO MissingUploadSecret
