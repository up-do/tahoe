{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import qualified Control.Concurrent.Async as Async
import Control.Exception (Exception, SomeException, throwIO, try)
import Control.Monad (forM_)
import Data.Bifunctor (Bifunctor (bimap))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Default.Class (Default (def))
import Data.Either (isRight)
import qualified Data.Text as T
import Data.X509 (CertificateChain (..), getSigned, signedObject)
import GHC.IO (unsafePerformIO)
import Network.HTTP.Client (
    defaultRequest,
    managerModifyRequest,
    newManager,
    parseRequest,
    requestHeaders,
    withConnection,
 )
import Network.HTTP.Client.Internal (Connection (connectionRead))
import qualified Network.Simple.TCP.TLS as SimpleTLS
import Network.Socket (
    AddrInfoFlag (AI_NUMERICHOST, AI_NUMERICSERV),
    Family (AF_INET),
    SockAddr (SockAddrInet, SockAddrInet6, SockAddrUnix),
    Socket,
    SocketType (Stream),
    addrAddress,
    addrFlags,
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
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.WarpTLS (
    runTLSSocket,
    tlsSettings,
 )
import Servant.Client (ClientError (ConnectionError))
import TahoeLAFS.Internal.Client (
    SPKIHash (SPKIHash),
    mkGBSManagerSettings,
    spkiBytes,
    spkiFingerprint,
 )
import TahoeLAFS.Storage.Backend.Memory (memoryBackend)
import TahoeLAFS.Storage.Client (NURL (NURLv1, nurlv1Address), runGBS, version)
import qualified TahoeLAFS.Storage.Server as Server
import Test.Hspec (
    Spec,
    describe,
    expectationFailure,
    it,
    runIO,
    shouldBe,
    shouldContain,
    shouldReturn,
    shouldSatisfy,
    shouldThrow,
 )
import Text.Printf (printf)
import Vectors (SPKICase (..), loadSPKITestVector)

-- Paths to pre-generated test data - an RSA private key and associated
-- self-signed certificate.
privateKeyPath :: FilePath
privateKeyPath = "test/data/private-key.pem"
certificatePath :: FilePath
certificatePath = "test/data/certificate.pem"

spkiTestVectorPath :: FilePath
spkiTestVectorPath = "test/data/spki-hash-test-vectors.yaml"

credentialE :: Either String TLS.Credential
{-# NOINLINE credentialE #-}
credentialE = unsafePerformIO $ TLS.credentialLoadX509 certificatePath privateKeyPath

credential :: TLS.Credential
credential = either (error . ("Failed to load test credentials: " <>) . show) id credentialE

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
                    it "test suite bug" $ expectationFailure $ "could not load test vectors: " <> show loadErr
                Right vector -> do
                    describe "spkiBytes" $ do
                        it "agrees with the test vectors" $ do
                            forM_ vector $ \(SPKICase{spkiExpected, spkiCertificate}) -> do
                                spkiBytes spkiCertificate `shouldBe` spkiExpected

                    describe "spkiFingerprint" $ do
                        it "agrees with the test vectors" $ do
                            forM_ vector $ \(SPKICase{spkiExpectedHash, spkiCertificate}) -> do
                                spkiFingerprint spkiCertificate `shouldBe` spkiExpectedHash

        describe "TLS connections" $ do
            let CertificateChain [signedExactCert] = fst credential
                requiredHash = spkiFingerprint . signedObject . getSigned $ signedExactCert
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

        describe "runGBS" $ do
            let swissnum = "hello world"
            let CertificateChain [signedExactCert] = fst credential
            let certificate = signedObject . getSigned $ signedExactCert
            let nurl = NURLv1 (spkiFingerprint certificate) ("127.0.0.1", 0) swissnum
            it "returns Left on connection errors" $ do
                result <- runGBS nurl version
                result
                    `shouldSatisfy` ( \case
                                        Left (ConnectionError _) -> True
                                        _ -> False
                                    )

            it "returns Right on success" $ do
                backend <- memoryBackend
                ver <- withServerSocket $ \sock -> do
                    Async.withAsync (runTLSSocket (tlsSettings certificatePath privateKeyPath) defaultSettings sock (Server.app backend)) $
                        const $ do
                            addr <- getSocketName sock
                            runGBS nurl{nurlv1Address = bimap T.unpack fromIntegral $ addrToHostPort addr} version
                ver `shouldSatisfy` isRight
  where
    settings = mkGBSManagerSettings (SPKIHash "just any hash") "swissnum"
    request = defaultRequest

    expectServerSuccess = id
    expectServerFailure server = do
        result <- try server
        case result of
            Left (_ :: SomeException) -> pure ()
            Right r -> throwIO $ ExpectedFailure ("Expect the server to fail but it succeed with " <> show r)

withServerSocket :: (Socket -> IO a) -> IO a
withServerSocket action = do
    sock <- socket AF_INET Stream 0
    listen sock 1
    r <- action sock
    close' sock
    pure r

newtype ExpectedFailure = ExpectedFailure String deriving (Eq, Ord, Show)
instance Exception ExpectedFailure

addrToHostPort :: SockAddr -> (T.Text, Int)
addrToHostPort (SockAddrInet port host) = (T.pack $ uncurry4 (printf "%d.%d.%d.%d") (hostAddressToTuple host), fromIntegral port)
addrToHostPort (SockAddrInet6 port _flow host _scope) = (T.pack $ uncurry8 (printf "%x:%x:%x:%x:%x:%x:%x:%x") (hostAddress6ToTuple host), fromIntegral port)
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
    serverApp (ctx, _) = TLS.sendData ctx expectedBytes

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
