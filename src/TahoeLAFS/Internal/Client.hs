{- | Implement the correct HTTPS client configuration for using Great Black
 Swamp.  This is necessary and correct for authenticating Great Black
 Swamp's self-authenticating URLs.
-}
module TahoeLAFS.Internal.Client where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (ManagerSettings, Request (requestHeaders), managerModifyRequest)
import Network.HTTP.Client.TLS (mkManagerSettings)

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
        { managerModifyRequest = fixAccept swissnum
        }
  where
    sockSettings = Nothing

{- | The TLSSettings suitable for use with Great Black Swamp client requests.
 These ensure we can authenticate the server before using it.
-}
gbsTLSSettings :: TLSSettings
gbsTLSSettings = TLSSettingsSimple True True True

-- Add the necessary authorization header.
fixAccept :: Applicative f => T.Text -> Request -> f Request
fixAccept swissnum req =
    pure req{requestHeaders = ("Authorization", "Tahoe-LAFS " <> enc swissnum) : requestHeaders req}
  where
    enc = Base64.encode . T.encodeUtf8

fixAcceptPrint :: T.Text -> Request -> IO Request
fixAcceptPrint swissnum req = do
    print req
    fixAccept swissnum req
