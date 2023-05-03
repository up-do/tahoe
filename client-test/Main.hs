{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Codec.CBOR.Encoding
import Codec.CBOR.FlatTerm
import Codec.CBOR.Pretty
import Codec.Serialise
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base32 (encodeBase32Unpadded)
import Data.ByteString.Base64 (encodeBase64)
import qualified Data.ByteString.Lazy as BSL
import Data.Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Text
import Data.Text.Encoding
import GHC.Generics
import Network.Connection
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Network.URI
import Servant.API
import Servant.Client
import qualified Servant.Client.Streaming as S
import Servant.Types.SourceT
import System.Environment (getArgs)
import Tahoe.CHK.Capability
import Tahoe.CHK.Share (getVersion)
import TahoeLAFS.Storage.API
import TahoeLAFS.Storage.Client
import Text.Megaparsec

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

-- make a value to write out
aVersion :: Version
aVersion = Version v1params testAV

v1params = Version1Parameters 257892218368 69105000000000000 257892218368

testAV :: ApplicationVersion
testAV = "tahoe-lafs/1.18.0.post908"

swrite :: Serialise a => FilePath -> a -> IO ()
swrite fname val = BSL.writeFile fname (serialise val)

sread :: FilePath -> IO Version
sread fname = deserialise <$> BSL.readFile fname
