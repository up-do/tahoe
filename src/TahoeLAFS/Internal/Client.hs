{-# LANGUAGE OverloadedStrings #-}

{- | Implement the correct HTTPS client configuration for using Great Black
 Swamp.  This is necessary and correct for authenticating Great Black
 Swamp's self-authenticating URLs.
-}
module TahoeLAFS.Internal.Client where

import qualified Data.ByteString.Base64 as Base64

-- import qualified Data.CaseInsensitive as CI
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (ManagerSettings, Request (requestHeaders), managerModifyRequest)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (Header)

-- import qualified Network.HTTP.Client.TLS as TLS

{- | Create a ManagerSettings suitable for use with Great Black Swamp client
 requests.
-}
mkGBSManagerSettings ::
    -- | The secret capability identifying the storage service to access.
    T.Text ->
    -- | The settings.
    ManagerSettings
mkGBSManagerSettings swissnum =
    (mkManagerSettings gbsTLSSettings sockSettings)
        { managerModifyRequest = addAuthorization swissnum
        }
  where
    sockSettings = Nothing

{- | The TLSSettings suitable for use with Great Black Swamp client requests.
 These ensure we can authenticate the server before using it.
-}
gbsTLSSettings :: TLSSettings
gbsTLSSettings = TLSSettingsSimple True True True

-- Add the necessary authorization header.  Since this is used with
-- `managerModifyRequest`, it may be called more than once per request so it
-- needs to take care not to double up headers.
-- https://github.com/snoyberg/http-client/issues/350
addAuthorization :: Applicative f => T.Text -> Request -> f Request
addAuthorization swissnum req =
    pure
        req
            { requestHeaders = addHeader authz . requestHeaders $ req
            }
  where
    enc = Base64.encode . T.encodeUtf8
    authz = ("Authorization", "Tahoe-LAFS " <> enc swissnum)

    addHeader :: Header -> [Header] -> [Header]
    addHeader (name, value) [] = [(name, value)]
    addHeader (name, value) (o@(name', value') : xs)
        | name == name' = (o : xs)
        | otherwise = o : addHeader (name, value) xs

addAuthorizationPrint :: T.Text -> Request -> IO Request
addAuthorizationPrint swissnum req = do
    print "Before"
    print req
    print "--------"
    r <- addAuthorization swissnum req
    print "After"
    print r
    print "--------"
    pure r
