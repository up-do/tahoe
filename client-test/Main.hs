{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

{- | Demonstrate the use of some GBS client APIs.

 Usage:

  client-test <storage-furl> <chk-read-cap> <share-num>
-}
module Main where

import Control.Monad ((>=>))
import Data.ByteString.Base32 (encodeBase32Unpadded)
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Client.TLS (
    newTlsManagerWith,
 )
import Network.HTTP.Types ()
import Network.Socket (HostName, PortNumber)
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
import Text.Read (readMaybe)

main :: IO ()
main = do
    [storageNURLStr, capStr, shareNumStr] <- getArgs
    let Right (CHKReader Reader{verifier = Verifier{..}}) = parse pCapability "argv[2]" (T.pack capStr)
        nurlM = parseNURL . T.pack $ storageNURLStr

    case nurlM of
        Nothing ->
            print ("Failed to parse NURL" :: T.Text)
        Just nurl -> do
            result <- runGBS nurl $ do
                ver <- version
                sharez <- getImmutableShareNumbers storageIndexS
                chk <- readImmutableShare storageIndexS shareNum Nothing
                pure (ver, sharez, chk)

            case result of
                Left err -> print $ "Uh " <> show err
                Right (ver, sharez, chk) -> do
                    print $ "version: " <> show ver
                    print $ "share numbers: " <> show sharez
                    print $ "share bytes: " <> show chk
          where
            storageIndexS = T.unpack . T.toLower . encodeBase32Unpadded $ storageIndex
            shareNum = ShareNumber $ read shareNumStr

-- Parse it like a regular URI after removing the confusing "tcp:" prefix on
-- the netloc.
parseFURL :: String -> Maybe URI
parseFURL = parseURI . T.unpack . T.replace "tcp:" "" . T.pack

dropLast :: Int -> [a] -> [a]
dropLast n xs =
    take (length xs - n) xs

data NURL = NURLv1
    { nurlv1Fingerprint :: SPKIHash
    , nurlv1Address :: (HostName, PortNumber)
    , nurlv1Swissnum :: T.Text
    }
    deriving (Ord, Eq, Show)

parseNURL :: T.Text -> Maybe NURL
parseNURL = parseURI . T.unpack >=> uriToNURL

uriToNURL :: URI -> Maybe NURL
uriToNURL URI{uriAuthority = Just URIAuth{uriUserInfo, uriRegName = hostname, uriPort = (':' : port)}, uriPath = ('/' : swissnum)} =
    case (requiredHashE, portM) of
        (Left _, _) -> Nothing
        (_, Nothing) -> Nothing
        (Right requiredHash, Just portNum) -> Just NURLv1{nurlv1Fingerprint = requiredHash, nurlv1Address = (hostname, portNum), nurlv1Swissnum = T.pack swissnum}
  where
    requiredHashE = fmap SPKIHash . Base64URL.decodeBase64 . T.encodeUtf8 . T.pack . dropLast 1 $ uriUserInfo
    portM = readMaybe port

runGBS :: NURL -> ClientM a -> IO (Either ClientError a)
runGBS NURLv1{nurlv1Fingerprint, nurlv1Address = (hostname, port), nurlv1Swissnum} client = do
    manager <- newTlsManagerWith (mkGBSManagerSettings nurlv1Fingerprint nurlv1Swissnum)
    let clientEnv = mkClientEnv manager (BaseUrl Https hostname (fromIntegral port) "")
    runClientM client clientEnv

showIt :: (Show a1, Show a2) => Either a1 a2 -> IO ()
showIt what = case what of
    Left err -> putStrLn $ "Error: " <> show err
    Right it -> print it
