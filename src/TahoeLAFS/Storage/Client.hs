{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module TahoeLAFS.Storage.Client (
    -- General server info
    version,
    -- Mutable or immutable
    renewLease,
    -- Immutable operations
    createImmutableStorageIndex,
    writeImmutableShare,
    abortImmutableUpload,
    readImmutableShare,
    getImmutableShareNumbers,
    adviseCorruptImmutableShare,
    -- Mutable operations
    readTestWrite,
    readMutableShares,
    getMutableShareNumbers,
    adviseCorruptMutableShare,
    parseNURL,
    runGBS,
    NURL (..),
) where

import Control.Monad ((>=>))
import qualified "base64" Data.ByteString.Base64.URL as Base64URL
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.TLS (
    newTlsManagerWith,
 )
import Network.Socket (HostName, PortNumber)
import Network.URI (
    -- URI (URI, uriAuthority, uriPath),
    URIAuth (URIAuth, uriPort, uriRegName, uriUserInfo),
    parseURI,
 )
import Servant (
    URI (URI, uriAuthority, uriFragment, uriPath),
    type (:<|>) ((:<|>)),
 )
import Servant.Client (
    BaseUrl (BaseUrl),
    ClientError,
    ClientM,
    Scheme (Https),
    client,
    mkClientEnv,
    runClientM,
 )
import TahoeLAFS.Internal.Client (SPKIHash (SPKIHash), mkGBSManagerSettings)
import TahoeLAFS.Storage.API (
    StorageAPI,
 )
import Text.Read (readMaybe)

newApi :: Proxy StorageAPI
newApi = Proxy
( version
        :<|> renewLease
        :<|> createImmutableStorageIndex
        :<|> writeImmutableShare
        :<|> abortImmutableUpload
        :<|> readImmutableShare
        :<|> getImmutableShareNumbers
        :<|> adviseCorruptImmutableShare
        :<|> readTestWrite
        :<|> readMutableShares
        :<|> getMutableShareNumbers
        :<|> adviseCorruptMutableShare
    ) = client newApi

-- | Represent a "new" style service URL.
data NURL = NURLv1
    { -- | The cryptographic fingerprint of the server hosting the service.
      nurlv1Fingerprint :: SPKIHash
    , -- | A hint about the network location of the server hosting the service.
      nurlv1Address :: (HostName, PortNumber)
    , -- | The secret identifier for the service within the scope of the server.
      nurlv1Swissnum :: T.Text
    }
    deriving (Ord, Eq, Show)

-- | Parse a Great Black Swamp NURL from text.
parseNURL :: T.Text -> Maybe NURL
parseNURL = parseURI . T.unpack >=> uriToNURL

uriToNURL :: URI -> Maybe NURL
uriToNURL URI{uriAuthority = Just URIAuth{uriUserInfo, uriRegName = hostname, uriPort = (':' : port)}, uriPath = ('/' : swissnum), uriFragment = "#v=1"} =
    case (requiredHashE, portM) of
        (Left _, _) -> Nothing
        (_, Nothing) -> Nothing
        (Right requiredHash, Just portNum) -> Just NURLv1{nurlv1Fingerprint = requiredHash, nurlv1Address = (hostname, portNum), nurlv1Swissnum = T.pack swissnum}
  where
    requiredHashE = fmap SPKIHash . Base64URL.decodeBase64 . T.encodeUtf8 . T.pack . dropLast 1 $ uriUserInfo
    portM = readMaybe port
uriToNURL _ = Nothing

{- | Execute some client operations against the Great Black Swamp server at
 the location indicated by the given NURL.
-}
runGBS :: NURL -> ClientM a -> IO (Either ClientError a)
runGBS NURLv1{nurlv1Fingerprint, nurlv1Address = (hostname, port), nurlv1Swissnum} action = do
    manager <- newTlsManagerWith (mkGBSManagerSettings nurlv1Fingerprint nurlv1Swissnum)
    let clientEnv = mkClientEnv manager (BaseUrl Https hostname (fromIntegral port) "")
    runClientM action clientEnv

dropLast :: Int -> [a] -> [a]
dropLast n xs =
    take (length xs - n) xs
