module TahoeLAFS.Storage.Server (
    StorageServerConfig (StorageServerConfig),
    app,
    main,
) where

import Control.Exception (
    Exception,
    throw,
    throwIO,
 )
import Control.Monad.IO.Class (
    liftIO,
 )
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (
    ByteRanges,
 )
import Network.Wai (
    Application,
 )
import Network.Wai.Handler.Warp (
    Port,
    defaultSettings,
    runSettings,
    setPort,
 )
import Network.Wai.Handler.WarpTLS (
    runTLS,
    tlsSettings,
 )
import Servant (
    Handler,
    Server,
    serve,
    (:<|>) (..),
 )
import TahoeLAFS.Storage.API (
    AllocateBuckets,
    AllocationResult (..),
    CBORSet (..),
    CorruptionDetails,
    LeaseSecret (Upload, Write),
    QueryRange,
    ReadTestWriteResult (..),
    ReadTestWriteVectors,
    ShareData,
    ShareNumber,
    StorageAPI,
    StorageIndex,
    Version (..),
    WriteEnablerSecret (WriteEnablerSecret),
    api,
 )
import TahoeLAFS.Storage.Backend (WriteImmutableError (MissingUploadSecret))
import qualified TahoeLAFS.Storage.Backend as Backend
import TahoeLAFS.Storage.Backend.Filesystem (
    FilesystemBackend (FilesystemBackend),
 )

version :: Backend.Backend b => b -> Handler Version
version backend =
    liftIO (Backend.version backend)

renewLease :: Backend.Backend b => b -> StorageIndex -> Maybe [LeaseSecret] -> Handler ()
renewLease backend storageIndex secrets = liftIO (Backend.renewLease backend storageIndex (fromMaybe [] secrets))

createImmutableStorageIndex :: Backend.Backend b => b -> StorageIndex -> Maybe [LeaseSecret] -> AllocateBuckets -> Handler AllocationResult
createImmutableStorageIndex backend storageIndex secrets params =
    liftIO (Backend.createImmutableStorageIndex backend storageIndex secrets params)

writeImmutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> ShareData -> Maybe ByteRanges -> Handler ()
writeImmutableShare backend storage_index share_number secrets share_data content_ranges =
    liftIO (Backend.writeImmutableShare backend storage_index share_number secrets share_data content_ranges)

abortImmutableUpload :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> Maybe [LeaseSecret] -> Handler ()
abortImmutableUpload backend storageIndex shareNum secrets = liftIO (Backend.abortImmutableUpload backend storageIndex shareNum secrets)

adviseCorruptImmutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> CorruptionDetails -> Handler ()
adviseCorruptImmutableShare backend storage_index share_number details =
    liftIO (Backend.adviseCorruptImmutableShare backend storage_index share_number details)

getImmutableShareNumbers :: Backend.Backend b => b -> StorageIndex -> Handler (CBORSet ShareNumber)
getImmutableShareNumbers backend storage_index =
    liftIO (Backend.getImmutableShareNumbers backend storage_index)

readImmutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> QueryRange -> Handler ShareData
readImmutableShare backend storage_index share_number qr =
    -- TODO Need to return NO CONTENT if the result is empty.
    -- TODO Need to make sure content-range is set in the header otherwise
    liftIO (Backend.readImmutableShare backend storage_index share_number qr)

readvAndTestvAndWritev :: Backend.Backend b => b -> StorageIndex -> Maybe [LeaseSecret] -> ReadTestWriteVectors -> Handler ReadTestWriteResult
readvAndTestvAndWritev _ _ Nothing _ = throw MissingUploadSecret
readvAndTestvAndWritev _ _ (Just []) _ = throw MissingUploadSecret
readvAndTestvAndWritev backend storageIndex (Just (Write secret : _)) vectors =
    liftIO (Backend.readvAndTestvAndWritev backend storageIndex secret vectors)
readvAndTestvAndWritev backend storageIndex (Just (_ : ss)) vectors =
    readvAndTestvAndWritev backend storageIndex (Just ss) vectors

readMutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> QueryRange -> Handler ShareData
readMutableShare backend storage_index share_numbers params =
    liftIO (Backend.readMutableShare backend storage_index share_numbers params)

getMutableShareNumbers :: Backend.Backend b => b -> StorageIndex -> Handler (CBORSet ShareNumber)
getMutableShareNumbers backend storage_index =
    liftIO (Backend.getMutableShareNumbers backend storage_index)

adviseCorruptMutableShare :: Backend.Backend b => b -> StorageIndex -> ShareNumber -> CorruptionDetails -> Handler ()
adviseCorruptMutableShare backend storage_index share_number details =
    liftIO (Backend.adviseCorruptMutableShare backend storage_index share_number details)

data MisconfiguredTLS = MisconfiguredTLS
    deriving (Show)
instance Exception MisconfiguredTLS

data StorageServerConfig = StorageServerConfig
    { storagePath :: FilePath
    , listenPort :: Port
    , certificate :: Maybe FilePath
    , key :: Maybe FilePath
    }
    deriving (Show, Eq)

app :: Backend.Backend b => b -> Application
app backend =
    serve api storageServer
  where
    storageServer :: Server StorageAPI
    storageServer =
        version backend
            :<|> renewLease backend
            :<|> createImmutableStorageIndex backend
            :<|> writeImmutableShare backend
            :<|> abortImmutableUpload backend
            :<|> readImmutableShare backend
            :<|> getImmutableShareNumbers backend
            :<|> adviseCorruptImmutableShare backend
            :<|> readvAndTestvAndWritev backend
            :<|> readMutableShare backend
            :<|> getMutableShareNumbers backend
            :<|> adviseCorruptMutableShare backend

main :: StorageServerConfig -> IO ()
main config =
    run $ app (FilesystemBackend $ storagePath config)
  where
    settings = setPort (listenPort config) defaultSettings
    run a =
        case (certificate config, key config) of
            (Nothing, Nothing) -> runSettings settings a
            (Just c, Just k) -> runTLS (tlsSettings c k) settings a
            _ -> throw MisconfiguredTLS
