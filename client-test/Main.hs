{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Demonstrate the use of some GBS client APIs.

 Usage:

  client-test <storage-furl> <chk-read-cap> <share-num>
-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base32 (encodeBase32Unpadded)
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence.Internal.Sorting as APIs
import qualified Data.Set as Set
import Data.Text (pack, replace, toLower, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.Connection (TLSSettings (TLSSettingsSimple))
import Network.HTTP.Client (
    ManagerSettings (managerModifyRequest),
    Request (requestHeaders),
 )
import Network.HTTP.Client.TLS (
    mkManagerSettings,
    newTlsManagerWith,
 )
import Network.HTTP.Types ()
import Network.URI (
    URI (URI, uriAuthority, uriPath),
    URIAuth (URIAuth, uriPort, uriRegName),
    parseURI,
 )
import Servant.Client (
    BaseUrl (BaseUrl),
    Scheme (Https),
    mkClientEnv,
    runClientM,
 )
import System.Environment (getArgs)
import Tahoe.CHK.Capability (
    CHK (CHKReader, CHKVerifier),
    Reader (Reader, verifier),
    Verifier (
        Verifier,
        fingerprint,
        required,
        size,
        storageIndex,
        total
    ),
    dangerRealShow,
    pCapability,
    pReader,
 )
import Tahoe.CHK.Share (getVersion)
import TahoeLAFS.Storage.API (ShareNumber (..))
import TahoeLAFS.Storage.Client (
    getImmutableShareNumbers,
    readImmutableShares,
    version,
 )
import Text.Megaparsec (parse)

main :: IO ()
main = do
    [storageFURLStr, capStr, shareNumStr] <- getArgs
    let Right (CHKReader cap@Reader{verifier = Verifier{..}}) = parse pCapability "argv[2]" (Data.Text.pack capStr)
        Just URI{uriAuthority = Just URIAuth{uriRegName = hostname, uriPort = (':' : port)}, uriPath = ('/' : swissnum)} = parseFURL storageFURLStr

    run (Data.Text.unpack . Data.Text.toLower . encodeBase32Unpadded $ storageIndex) hostname (read port) swissnum (ShareNumber (read shareNumStr))

parseFURL :: String -> Maybe URI
parseFURL = parseURI . Data.Text.unpack . Data.Text.replace "tcp:" "" . Data.Text.pack

fixAccept :: Applicative f => String -> Request -> f Request
fixAccept swissnum req =
    pure req{requestHeaders = ("Authorization", "Tahoe-LAFS " <> enc swissnum) : requestHeaders req}
  where
    enc = encodeUtf8 . encodeBase64 . encodeUtf8 . Data.Text.pack

fixAcceptPrint swissnum req = do
    print req
    fixAccept swissnum req

run ::
    -- | The base32-encoded storage index for which to request share info.
    String ->
    -- | The hostname or IP address of the storage server to query.
    String ->
    -- | The port number of the storage server to query.
    Int ->
    -- | The swissnum of the storage service
    String ->
    -- | A share number to download from the server.
    ShareNumber ->
    IO ()
run storageIndex hostname port swissnum shareNum = do
    let tlsSettings = TLSSettingsSimple True True True
        sockSettings = Nothing
        managerSettings = (mkManagerSettings tlsSettings sockSettings){managerModifyRequest = fixAccept swissnum}
    manager' <- newTlsManagerWith managerSettings
    let manager'' = manager'
        callIt = flip runClientM (mkClientEnv manager' (BaseUrl Https hostname port ""))

    putStrLn "getVersion"
    ver <- callIt version
    showIt ver
    putStrLn "getImmutableShareNumbers:"
    sharez <- callIt $ getImmutableShareNumbers storageIndex
    showIt sharez
    putStrLn "readImmutableShares - succeeds!"
    chk <- callIt $ readImmutableShares storageIndex shareNum Nothing
    showIt chk

flomp bip = dangerRealShow . CHKVerifier . verifier <$> parse pReader "" bip

showIt :: (Show a1, Show a2) => Either a1 a2 -> IO ()
showIt what = case what of
    Left err -> putStrLn $ "Error: " <> show err
    Right it -> print it
