{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

-- import Data.Aeson hiding

import Codec.CBOR.Encoding
import Codec.CBOR.FlatTerm
import Codec.CBOR.Pretty
import Codec.Serialise
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Map
import Data.Proxy
import qualified Data.Set as Set
import Data.Text
import GHC.Generics
import Network.Connection
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Servant.API
import Servant.Client
import qualified Servant.Client.Streaming as S
import Servant.Types.SourceT
import Tahoe.CHK.Capability
import TahoeLAFS.Storage.API
import TahoeLAFS.Storage.Client
import Text.Megaparsec

main :: IO ()
main = do
    run

fixAccept :: Applicative f => Request -> f Request
fixAccept req = pure req{requestHeaders = ("Authorization", "Tahoe-LAFS a2xwc2hmeTVqNmNyZzZnb3I0d2pyY2Fza3p0NzVncWQ=") : requestHeaders req}

run :: IO ()
run = do
    let tlsSettings = TLSSettingsSimple True True True
        sockSettings = Nothing
        managerSettings = (mkManagerSettings tlsSettings sockSettings){managerModifyRequest = fixAccept}
    manager' <- newTlsManagerWith managerSettings
    let manager'' = manager'
        callIt = flip runClientM (mkClientEnv manager' (BaseUrl Https "localhost" 33337 ""))
    print "getImmutableShareNumbers - succeeds!"
    sharez <- callIt $ getImmutableShareNumbers "p6edaziufni6ajhw6x4zoq24ni"
    showIt sharez
    print "readImmutableShares - succeeds! but returns a CHK with the wrong MIME type"
    chk <- callIt $ readImmutableShares "p6edaziufni6ajhw6x4zoq24ni" (ShareNumber 0) Nothing -- (Just [ByteRangeFrom 1])
    showIt chk

flomp bip = showBase32 . storageIndex . verifier <$> parse pReader "" bip

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

tahoe :: BSL.ByteString
tahoe = "\162X/http://allmydata.org/tahoe/protocols/storage/v1\163X\FSmaximum-immutable-share-size\ESC\NUL\NUL\NUL6+\167\230\NULX\SUBmaximum-mutable-share-size\ESC\NUL\245\130\161\161\&4\DLE\NULOavailable-space\ESC\NUL\NUL\NUL6+\167\230\NULSapplication-versionX\EMtahoe-lafs/1.18.0.post908"
