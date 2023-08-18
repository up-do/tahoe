{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import Control.Exception (try)
import Network.HTTP.Client (defaultRequest, managerModifyRequest, requestHeaders)
import Network.Socket (AI_NUMERICHOST, AI_NUMERICSERV, AddrInfo, addrAddress, addrFamily, addrProtocol, addrSocketType, bind, getAddrInfo, openSocket, socket)
import TahoeLAFS.Internal.Client (mkGBSManagerSettings)

import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
 )

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
                port <- runTlsServer certificate
                connect <- managerTlsConnection (mkGBSManagerSettings (spkiHash certificate) "swissnum")
                connection <- connect "unused" "127.0.0.1" port
                -- Assume that if we got a connection, TLS succeeded.  Is it safe?  I don't know.
                connectionClose connection
            it "refuses to make a connection to a server not using the correct certificate" $ do
                port <- runTlsServer certificate
                connect <- managerTlsConnection (mkGBSManagerSettings "wrong spki hash" "swissnum")
                try (connect "unused" "127.0.0.1" port) `shouldBe` Left "something"
  where
    settings = mkGBSManagerSettings "swissnum"
    request = defaultRequest

-- XXX get a Credential here and then use it to set up the TLS.Context
-- ServerParams -> serverShared (Shared) -> sharedCredentials -> Credentials ([Credential]) -> (CertificateChain, PrivKey)
-- ServerParams -> serverHooks (ServerHooks) -> onServerNameIndication -> return Credentials ([Credential]) -> (CertificateChain, PrivKey)
withTlsServer :: [Credential] -> (TLS.Context -> IO a) -> IO a
withTlsServer serverCredentials app = do
    -- XXX safely clean up
    addr : _ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
    sock <- openSocket addr
    bind sock (addrAddress addr)
    TLS.contextNew sock serverParams
    app sock
    close' sock
  where
    hints = defaultHints{addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream}
    serverParams =
        def
            { serverShared =
                def
                    { sharedCredentials = serverCredentials
                    }
            }
