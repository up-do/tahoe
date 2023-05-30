{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Demonstrate the use of some GBS client APIs.

 Usage:

  client-test <storage-furl> <chk-read-cap> <share-num>
-}
module Main where

import Data.ByteString.Base32 (encodeBase32Unpadded)

import qualified Data.ByteString.Base64 as Base64

import Data.Text
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
    ClientError,
    ClientM,
    Scheme (Https),
    mkClientEnv,
    runClientM,
 )
import System.Environment (getArgs)
import Tahoe.CHK.Capability (
    CHK (CHKReader),
    Reader (Reader, verifier),
    Verifier (
        Verifier,
        fingerprint,
        required,
        size,
        storageIndex,
        total
    ),
    pCapability,
 )
import TahoeLAFS.Storage.Client
import Text.Megaparsec

import TahoeLAFS.Storage.API

main :: IO ()
main = do
    [storageFURLStr, capStr, shareNumStr] <- getArgs
    let Right (CHKReader Reader{verifier = Verifier{..}}) = parse pCapability "argv[2]" (Data.Text.pack capStr)
        Just URI{uriAuthority = Just URIAuth{uriRegName = hostname, uriPort = (':' : port)}, uriPath = ('/' : swissnum)} = parseFURL storageFURLStr

    run (Data.Text.unpack . Data.Text.toLower . encodeBase32Unpadded $ storageIndex) hostname (read port) swissnum (ShareNumber (read shareNumStr))

-- Parse it like a regular URI after removing the confusing "tcp:" prefix on
-- the netloc.
parseFURL :: String -> Maybe URI
parseFURL = parseURI . Data.Text.unpack . Data.Text.replace "tcp:" "" . Data.Text.pack

-- Add the necessary authorization header.
fixAccept :: Applicative f => String -> Request -> f Request
fixAccept swissnum req =
    pure req{requestHeaders = ("Authorization", "Tahoe-LAFS " <> enc swissnum) : requestHeaders req}
  where
    enc = Base64.encode . encodeUtf8 . Data.Text.pack

fixAcceptPrint :: String -> Request -> IO Request
fixAcceptPrint swissnum req = do
    print req
    fixAccept swissnum req

-- Do some API calls and report the results.
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
    manager' <- newTlsManagerWith managerSettings
    let callIt :: ClientM a -> IO (Either ClientError a)
        callIt = flip runClientM (mkClientEnv manager' (BaseUrl Https hostname port ""))

    putStrLn "getVersion"
    ver <- callIt version
    showIt ver
    putStrLn "getImmutableShareNumbers:"
    sharez <- callIt $ getImmutableShareNumbers storageIndex
    showIt sharez
    putStrLn "readImmutableShare - succeeds!"
    chk <- callIt $ readImmutableShare storageIndex shareNum Nothing
    showIt chk
  where
    tlsSettings = TLSSettingsSimple True True True
    sockSettings = Nothing
    managerSettings = (mkManagerSettings tlsSettings sockSettings){managerModifyRequest = fixAccept swissnum}

showIt :: (Show a1, Show a2) => Either a1 a2 -> IO ()
showIt what = case what of
    Left err -> putStrLn $ "Error: " <> show err
    Right it -> print it
