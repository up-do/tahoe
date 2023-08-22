{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception, SomeException, throwIO, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class (Default (def))
import Data.List (intercalate)
import Data.X509 (CertificateChain (..), getSigned, signedObject)
import Debug.Trace
import Network.HTTP.Client (
    defaultRequest,
    managerModifyRequest,
    managerTlsConnection,
    newManager,
    parseRequest,
    requestHeaders,
    withConnection,
 )
import Network.HTTP.Client.Internal (Connection (connectionRead))
import qualified Network.Simple.TCP.TLS as SimpleTLS
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
    getSocketName,
    hostAddress6ToTuple,
    hostAddressToTuple,
    listen,
    openSocket,
    socket,
 )
import qualified Network.TLS as TLS
import Network.TLS.Extra.Cipher (ciphersuite_default)
import Numeric (showHex)
import TahoeLAFS.Internal.Client (
    SPKIHash (SPKIHash),
    mkGBSManagerSettings,
    spkiBytes,
    spkiHash,
 )
import Test.Hspec (
    Spec,
    anyIOException,
    describe,
    expectationFailure,
    it,
    runIO,
    shouldBe,
    shouldContain,
    shouldReturn,
    shouldThrow,
 )
import Text.Printf (printf)
import Vectors (SPKICase (..), loadSPKITestVector)

-- Paths to pre-generated test data - an RSA private key and associated
-- self-signed certificate.
privateKeyPath = "test/data/private-key.pem"
certificatePath = "test/data/certificate.pem"

spkiTestVectorPath = "test/data/spki-hash-test-vectors.yaml"

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

        describe "SPKI Fingerprints" $ do
            vectorE <- runIO $ loadSPKITestVector <$> B.readFile spkiTestVectorPath
            case vectorE of
                Left loadErr ->
                    it "is broken" $ expectationFailure $ "could not load test vectors: " <> show loadErr
                Right vector -> do
                    describe "spkiBytes" $ do
                        it "agrees with the test vectors" $ do
                            flip mapM_ vector $ \(SPKICase{spkiExpected, spkiCertificate}) -> do
                                spkiBytes spkiCertificate `shouldBe` spkiExpected

                    describe "spkiHash" $ do
                        it "agrees with the test vectors" $ do
                            flip mapM_ vector $ \(SPKICase{spkiExpectedHash, spkiCertificate}) -> do
                                spkiHash spkiCertificate `shouldBe` spkiExpectedHash

        describe "TLS connections" $ do
            credentialE <- runIO $ TLS.credentialLoadX509 certificatePath privateKeyPath
            case credentialE of
                Left loadErr ->
                    it "is broken" $ expectationFailure "could not load pre-generated certificate"
                Right credential -> do
                    let CertificateChain [signedExactCert] = fst credential
                        requiredHash = spkiHash . signedObject . getSigned $ signedExactCert
                    it "makes a connection to a server using the correct certificate" $ do
                        withTlsServer (TLS.Credentials [credential]) "Hello!" expectServerSuccess $ \serverAddr -> do
                            let (host, port) = addrToHostPort serverAddr
                            manager <- newManager (mkGBSManagerSettings requiredHash "swissnum")
                            req <- parseRequest $ printf "https://%s:%d/" host port
                            withConnection req manager $ \clientConn -> do
                                connectionRead clientConn `shouldReturn` "Hello!"

                    it "refuses to make a connection to a server not using the correct certificate" $ do
                        withTlsServer (TLS.Credentials [credential]) "Hello!" expectServerFailure $ \serverAddr -> do
                            let (host, port) = addrToHostPort serverAddr
                            manager <- newManager (mkGBSManagerSettings (SPKIHash "wrong spki hash") "swissnum")
                            req <- parseRequest $ printf "https://%s:%d/" host port
                            withConnection req manager connectionRead
                                `shouldThrow` (\(TLS.HandshakeFailed _) -> True)
  where
    settings = mkGBSManagerSettings (SPKIHash "just any hash") "swissnum"
    request = defaultRequest

    expectServerSuccess = id
    expectServerFailure server = do
        result <- try server
        case result of
            Left (e :: SomeException) -> pure ()
            Right r -> throwIO $ ExpectedFailure ("Expect the server to fail but it succeed with " <> show r)

newtype ExpectedFailure = ExpectedFailure String deriving (Eq, Ord, Show)
instance Exception ExpectedFailure

addrToHostPort :: SockAddr -> (String, Int)
addrToHostPort (SockAddrInet port host) = (uncurry4 (printf "%d.%d.%d.%d") (hostAddressToTuple host), fromIntegral port)
addrToHostPort (SockAddrInet6 port _flow host _scope) = (uncurry8 (printf "%x:%x:%x:%x:%x:%x:%x:%x") (hostAddress6ToTuple host), fromIntegral port)
addrToHostPort (SockAddrUnix _path) = error "Cannot do TLS over a Unix socket"

-- XXX get a Credential here and then use it to set up the TLS.Context
-- ServerParams -> serverShared (Shared) -> sharedCredentials -> Credentials ([Credential]) -> (CertificateChain, PrivKey)
-- ServerParams -> serverHooks (ServerHooks) -> onServerNameIndication -> return Credentials ([Credential]) -> (CertificateChain, PrivKey)
withTlsServer :: TLS.Credentials -> LB.ByteString -> (IO () -> IO ()) -> (SockAddr -> IO a) -> IO a
withTlsServer serverCredentials expectedBytes runServer clientApp = do
    -- XXX safely clean up
    bindAddr : _ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
    sock <- openSocket bindAddr
    bind sock (addrAddress bindAddr)
    listen sock 1
    boundAddr <- getSocketName sock

    -- The server socket is bound and listening so it is safe to initiate a
    -- connection now.  We'll get to handling the TLS connection next.
    client <- Async.async (clientApp boundAddr)

    -- Tests cover success and failure codepaths.  Let them make whatever
    -- assertion they want about the server result.
    () <- runServer $ SimpleTLS.accept serverParams sock serverApp

    -- Let the client finish
    Async.wait client
  where
    -- Serve a connection to the TLS server.
    serverApp (ctx, addr) = TLS.sendData ctx expectedBytes

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
            , TLS.serverSupported =
                def
                    { TLS.supportedCiphers = ciphersuite_default
                    }
            }

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 z (a, b, c, d) = z a b c d

uncurry8 :: (a -> b -> c -> d -> e -> f -> g -> h -> i) -> (a, b, c, d, e, f, g, h) -> i
uncurry8 z (a, b, c, d, e, f, g, h) = z a b c d e f g h
