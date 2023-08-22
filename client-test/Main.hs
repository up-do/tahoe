{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- | Demonstrate the use of some GBS client APIs.

 Usage:

  client-test <storage-furl> <chk-read-cap> <share-num>
-}
module Main where

import Data.ByteString.Base32 (encodeBase32Unpadded)
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.TLS (
    newTlsManagerWith,
 )
import Network.HTTP.Types ()
import Network.URI (
    URI (URI, uriAuthority, uriPath),
    URIAuth (URIAuth, uriPort, uriRegName, uriUserInfo),
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
import TahoeLAFS.Internal.Client (SPKIHash (SPKIHash), mkGBSManagerSettings)
import TahoeLAFS.Storage.API (ShareNumber (..))
import TahoeLAFS.Storage.Client (
    getImmutableShareNumbers,
    readImmutableShare,
    version,
 )
import Text.Megaparsec (parse)

main :: IO ()
main = do
    [storageFURLStr, capStr, shareNumStr] <- getArgs
    let Right (CHKReader Reader{verifier = Verifier{..}}) = parse pCapability "argv[2]" (T.pack capStr)
        Just URI{uriAuthority = Just URIAuth{uriUserInfo, uriRegName = hostname, uriPort = (':' : port)}, uriPath = ('/' : swissnum)} = parseFURL storageFURLStr
        requiredHashE = fmap SPKIHash . Base64URL.decodeBase64 . T.encodeUtf8 . T.pack . dropLast 1 $ uriUserInfo

    case requiredHashE of
        Left err ->
            print $ "Failed to parse authentication information from NURL: " <> show err
        Right requiredHash ->
            run requiredHash (T.unpack . T.toLower . encodeBase32Unpadded $ storageIndex) hostname (read port) (T.pack swissnum) (ShareNumber (read shareNumStr))

-- Parse it like a regular URI after removing the confusing "tcp:" prefix on
-- the netloc.
parseFURL :: String -> Maybe URI
parseFURL = parseURI . T.unpack . T.replace "tcp:" "" . T.pack

dropLast :: Int -> [a] -> [a]
dropLast n xs =
    take (length xs - n) xs

-- Do some API calls and report the results.
run ::
    -- | The SPKI hash to use to authenticate the server.
    SPKIHash ->
    -- | The base32-encoded storage index for which to request share info.
    String ->
    -- | The hostname or IP address of the storage server to query.
    String ->
    -- | The port number of the storage server to query.
    Int ->
    -- | The swissnum of the storage service
    T.Text ->
    -- | A share number to download from the server.
    ShareNumber ->
    IO ()
run requiredHash storageIndex hostname port swissnum shareNum = do
    manager' <- newTlsManagerWith (mkGBSManagerSettings requiredHash swissnum)
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

showIt :: (Show a1, Show a2) => Either a1 a2 -> IO ()
showIt what = case what of
    Left err -> putStrLn $ "Error: " <> show err
    Right it -> print it
