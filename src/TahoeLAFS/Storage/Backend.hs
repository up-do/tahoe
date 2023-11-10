{-# LANGUAGE DataKinds #-}

module TahoeLAFS.Storage.Backend (
    Backend (..),
    WriteImmutableError (..),
    writeMutableShare,
    withUploadSecret,
    module Tahoe.Storage.Backend,
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
import Tahoe.Storage.Backend
import TahoeLAFS.Storage.API (
    isUploadSecret,
 )

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
    result <- readvAndTestvAndWritev b storageIndex writeEnablerSecret vectors
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
