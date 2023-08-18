{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import Control.Exception (try)
import qualified Data.ByteString as B
import Data.Default.Class (Default (def))
import Data.List (intercalate)
import Data.X509 (decodeSignedCertificate)
import Network.Connection (connectionClose)
import Network.HTTP.Client (defaultRequest, managerModifyRequest, managerTlsConnection, requestHeaders)
import Network.Socket (
    AddrInfo,
    AddrInfoFlag (AI_NUMERICHOST, AI_NUMERICSERV),
    SockAddr (SockAddrInet, SockAddrInet6, SockAddrUnix),
    SocketType (Stream),
    addrAddress,
    addrFamily,
    addrFlags,
    addrProtocol,
    addrSocketType,
    bind,
    close',
    defaultHints,
    getAddrInfo,
    hostAddress6ToTuple,
    hostAddressToTuple,
    openSocket,
    socket,
 )
import qualified Network.TLS as TLS
import Numeric (showHex)
import TahoeLAFS.Internal.Client (mkGBSManagerSettings)
import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
 )
import Text.Printf (printf)

spec :: Spec
spec = do
    describe "mkGBSManagerSettings" $ do
        describe "Authorization header" $ do
            it "includes the Tahoe-LAFS realm and encoded swissnum" $ do
                modified <- managerModifyRequest settings request
                requestHeaders modified
                    -- Should contain the base64 encoding of the swissnum
                    `shouldContain` [("authorization", "Tahoe-LAFS c3dpc3NudW0=")]
            it "does not duplicate the header" $ do
                modified <- managerModifyRequest settings request
                modified' <- managerModifyRequest settings modified
                let authorizations = filter (("authorization" ==) . fst) (requestHeaders modified')
                length authorizations `shouldBe` 1

        describe "TLS connections" $ do
            certificate <- fmap decodeSignedCertificate <$> B.readFile "certificate"
            it "makes a connection to a server using the correct certificate" $ do
                withTlsServer certificate $ \addr ctx -> do
                    connect <- managerTlsConnection (mkGBSManagerSettings (spkiHash certificate) "swissnum")
                    let (host, port) = addrToHostPort addr

                    connection <- connect "unused" host port
                    -- Assume that if we got a connection, TLS succeeded.  Is it safe?  I don't know.
                    connectionClose connection
            it "refuses to make a connection to a server not using the correct certificate" $ do
                withTlsServer certificate $ \addr ctx -> do
                    connect <- managerTlsConnection (mkGBSManagerSettings "wrong spki hash" "swissnum")
                    let (host, port) = addrToHostPort addr
                    try (connect "unused" host port) `shouldBe` Left "something"
  where
    settings = mkGBSManagerSettings "swissnum"
    request = defaultRequest

spkiHash _ = error "xxx implement spkiHash"

addrToHostPort (SockAddrInet port host) = (printf "%d.%d.%d.%d" $ hostAddressToTuple host, fromIntegral port)
addrToHostPort (SockAddrInet6 port _flow host _scope) = (printf "%x:%x:%x:%x:%x:%x" $ hostAddress6ToTuple host, fromIntegral port)
addrToHostPort (SockAddrUnix _path) = error "Cannot do TLS over a Unix socket"

-- XXX get a Credential here and then use it to set up the TLS.Context
-- ServerParams -> serverShared (Shared) -> sharedCredentials -> Credentials ([Credential]) -> (CertificateChain, PrivKey)
-- ServerParams -> serverHooks (ServerHooks) -> onServerNameIndication -> return Credentials ([Credential]) -> (CertificateChain, PrivKey)
withTlsServer :: TLS.Credentials -> (SockAddr -> TLS.Context -> IO a) -> IO a
withTlsServer serverCredentials app = do
    -- XXX safely clean up
    addr : _ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
    sock <- openSocket addr
    bind sock (addrAddress addr)
    ctx <- TLS.contextNew sock serverParams
    r <- app (addrAddress addr) ctx
    TLS.bye ctx
    close' sock
    pure r
  where
    hints =
        defaultHints
            { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV]
            , addrSocketType = Stream
            }
    serverParams =
        def
            { TLS.serverShared =
                def
                    { TLS.sharedCredentials = serverCredentials
                    }
            }
