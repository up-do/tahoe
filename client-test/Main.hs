{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Map
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client hiding (Proxy)
import Network.HTTP.Types
import Servant.API
import Servant.Client
import qualified Servant.Client.Streaming as S
import Servant.Types.SourceT
import TahoeLAFS.Storage.API

version :<|> immutableStorage :<|> immutableStorageIndex :<|> immutableStorageIndexCorrupt :<|> immutableStorageIndexShares :<|> immutableStorageIndexShareNumber :<|> mutableStorageIndex :<|> mutableStorageIndexRTW :<|> mutableStorageIndexShareNumber :<|> mutableStorageIndexShareNumberShares :<|> mutableStorageIndexShareNumberCorrupt = client api

main :: IO ()
main = do
    print "hi"
    run

-- -- | URI scheme to use
-- data Scheme
--     = -- | http://
--       Http
--     | -- | https://
--       Https
--     deriving (Generic)

-- {- | Simple data type to represent the target of HTTP requests
--    for servant's automatically-generated clients.
-- -}
-- data BaseUrl = BaseUrl
--     { baseUrlScheme :: Scheme
--     -- ^ URI scheme to use
--     , baseUrlHost :: String
--     -- ^ host (eg "haskell.org")
--     , baseUrlPort :: Int
--     -- ^ port (eg 80)
--     , baseUrlPath :: String
--     -- ^ path (eg "/a/b/c")
--     }

getVersion :: ClientM Version
getVersion = do
    version

fixAccept :: Request -> Request
fixAccept req =
    req{requestHeaders = ("Authorization", "Tahoe-LAFS a2xwc2hmeTVqNmNyZzZnb3I0d2pyY2Fza3p0NzVncWQ=") : requestHeaders req}

run :: IO ()
run = do
    manager' <- newManager defaultManagerSettings
    res <- runClientM getVersion (mkClientEnv manager' (BaseUrl Http "localhost" 33337 ""))
    case res of
        Left err -> putStrLn $ "Error: " <> show err
        Right v -> do
            print v

-- queries :: ClientM (Position, HelloMessage, Email)
-- queries = do
--   pos <- position 10 10
--   message <- hello (Just "servant")
--   em  <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
--   return (pos, message, em)

-- run :: IO ()
-- run = do
--   manager' <- newManager defaultManagerSettings
--   res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
--   case res of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right (pos, message, em) -> do
--       print pos
--       print message
--       print em
