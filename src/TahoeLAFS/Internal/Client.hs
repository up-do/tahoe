{-# LANGUAGE OverloadedStrings #-}

{- | Implement the correct HTTPS client configuration for using Great Black
 Swamp.  This is necessary and correct for authenticating Great Black
 Swamp's self-authenticating URLs.
-}
module TahoeLAFS.Internal.Client where

import qualified Data.ByteString.Base64 as Base64

import Crypto.Hash (Digest, hash)
import Crypto.Hash.Algorithms (SHA256)
import Data.ASN1.BinaryEncoding (DER (DER))
import Data.ASN1.Encoding (encodeASN1')
import Data.ASN1.Types (ASN1Object (fromASN1, toASN1))
import Data.ByteArray (convert)
import qualified Data.ByteString as B
import Data.Default.Class (Default (def))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.X509 (Certificate (Certificate, certPubKey, certSignatureAlg), CertificateChain (CertificateChain), PubKey, SignatureALG, Signed (signedObject), SignedExact (getSigned))
import Data.X509.CertificateStore (CertificateStore)
import Data.X509.Validation (FailedReason (AuthorityTooDeep, EmptyChain, InvalidSignature), ServiceID, SignatureFailure (SignaturePubkeyMismatch), SignatureVerification (SignatureFailed, SignaturePass), verifySignedSignature)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client (ManagerSettings, Request (requestHeaders), managerModifyRequest)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.HTTP.Types (Header)
import Network.TLS (ClientHooks (onServerCertificate), ClientParams (..), Supported (..), ValidationCache)
import Network.TLS.Extra.Cipher (ciphersuite_default)

newtype SPKIHash = SPKIHash B.ByteString deriving (Eq, Ord)

instance Show SPKIHash where
    show (SPKIHash bs) = "SPKIHash " <> T.unpack (T.decodeLatin1 (Base64.encode bs))

{- | Create a ManagerSettings suitable for use with Great Black Swamp client
 requests.
-}
mkGBSManagerSettings ::
    -- | The SPKI hash of the certificate of the storage service to access.
    SPKIHash ->
    -- | The secret capability identifying the storage service to access.
    T.Text ->
    -- | The settings.
    ManagerSettings
mkGBSManagerSettings requiredHash swissnum =
    (mkManagerSettings (gbsTLSSettings requiredHash) sockSettings)
        { managerModifyRequest = addAuthorization swissnum
        }
  where
    sockSettings = Nothing

{- | The TLSSettings suitable for use with Great Black Swamp client requests.
 These ensure we can authenticate the server before using it.
-}
gbsTLSSettings :: SPKIHash -> TLSSettings
gbsTLSSettings requiredHash =
    TLSSettings
        ( ClientParams
            { clientUseMaxFragmentLength = Nothing
            , clientServerIdentification = ("", "")
            , clientUseServerNameIndication = True
            , clientWantSessionResume = Nothing
            , clientShared = def
            , clientHooks =
                def
                    { onServerCertificate = validateGBSCertificate requiredHash
                    }
            , clientSupported = def{supportedCiphers = ciphersuite_default}
            , clientDebug = def
            , clientEarlyData = Nothing
            }
        )

{- | Determine the validity of an x509 certificate presented during a TLS
 handshake for a GBS connection.

 The certificate is considered valid if its signature can be validated and
 the sha256 hash of its SPKI fields match the expected value.

 If not exactly one certificate is presented then validation fails.
-}
validateGBSCertificate :: SPKIHash -> CertificateStore -> ValidationCache -> ServiceID -> CertificateChain -> IO [FailedReason]
validateGBSCertificate _ _ _ _ (CertificateChain []) = pure [EmptyChain]
validateGBSCertificate requiredSPKIHash _ _ _ (CertificateChain [signedExactCert]) =
    -- Nothing is valid unless the signature on the certificate is valid
    -- so do that first.
    case verifySignedSignature signedExactCert pubKey of
        SignatureFailed failure -> pure [InvalidSignature failure]
        SignaturePass -> do
            -- The certificates SubjectPublicKeyInfo must match the hash we
            -- expect, too.
            if spkiHash cert == requiredSPKIHash
                then pure []
                else do
                    print $ "Expected SPKI hash: " <> show requiredSPKIHash
                    print $ "Got SPKI hash" <> show (spkiHash cert)
                    pure [InvalidSignature SignaturePubkeyMismatch]
  where
    pubKey = certPubKey cert
    cert = signedObject . getSigned $ signedExactCert
validateGBSCertificate _ _ _ _ _ = pure [AuthorityTooDeep]

data SubjectPublicKeyInfo = SubjectPublicKeyInfo
    { subjectPublicKeyInfoAlgorithm :: SignatureALG
    , subjectPublicKeyInfoPublicKey :: PubKey
    }
    deriving (Eq, Show)

instance ASN1Object SubjectPublicKeyInfo where
    toASN1 (SubjectPublicKeyInfo{subjectPublicKeyInfoAlgorithm, subjectPublicKeyInfoPublicKey}) =
        toASN1 subjectPublicKeyInfoAlgorithm <> toASN1 subjectPublicKeyInfoPublicKey

    fromASN1 asn1s = do
        (subjectPublicKeyInfoAlgorithm, theRest) <- fromASN1 asn1s
        (subjectPublicKeyInfoPublicKey, unused) <- fromASN1 theRest
        pure (SubjectPublicKeyInfo{..}, unused)

spkiHash :: Certificate -> SPKIHash
spkiHash = SPKIHash . sha256 . spkiBytes

spki :: Certificate -> SubjectPublicKeyInfo
spki (Certificate{certSignatureAlg, certPubKey}) = SubjectPublicKeyInfo certSignatureAlg certPubKey

spkiBytes :: Certificate -> B.ByteString
spkiBytes = encodeASN1' DER . flip toASN1 [] . spki

sha256 :: B.ByteString -> B.ByteString
sha256 = convert . (hash :: B.ByteString -> Digest SHA256)

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
        | name == name' = o : xs
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
