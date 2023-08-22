module Vectors where

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.X509 (Certificate, getSigned, signedObject)
import Data.X509.Memory (readSignedObjectFromMemory)
import Data.Yaml (FromJSON (..), ParseException, decodeEither', withObject, (.:))
import TahoeLAFS.Internal.Client (SPKIHash (SPKIHash))

-- | A single case of expected SPKI fingerprint calculation behavior.
data SPKICase = SPKICase
    { -- | The expected bytes representation of the SPKI information.
      spkiExpected :: B.ByteString
    , -- | The expected SPKI Fingerprint.
      spkiExpectedHash :: SPKIHash
    , -- | The certificate to operate on for this case.
      spkiCertificate :: Certificate
    }
    deriving (Eq, Show)

-- | A single possibly-successfully loaded SPKI fingerprint test case.
data SPKICase' = SPKICase'
    { spkiExpected' :: Either T.Text B.ByteString
    , spkiExpectedHash' :: Either T.Text B.ByteString
    , spkiCertificates' :: [Certificate]
    }
    deriving (Eq, Show)

instance FromJSON SPKICase' where
    parseJSON = withObject "SPKICase" $ \o ->
        SPKICase'
            <$> (Base64.decodeBase64 . T.encodeUtf8 <$> o .: "expected-spki")
            <*> (Base64URL.decodeBase64 . T.encodeUtf8 <$> o .: "expected-hash")
            <*> (fmap (signedObject . getSigned) . readSignedObjectFromMemory . T.encodeUtf8 <$> o .: "certificate")

-- | Some possibly-successfully loaded SPKI fingerprint test cases.
newtype SPKITestVector = SPKITestVector
    { spkiTestVector :: [SPKICase']
    }
    deriving (Eq, Show)

instance FromJSON SPKITestVector where
    parseJSON = withObject "SPKITestVector" $ \o -> SPKITestVector <$> o .: "vector"

-- | Attempt to load the SPKI Fingerprint test cases.
loadSPKITestVector ::
    -- | The YAML-serialized representation of the test cases.
    B.ByteString ->
    -- | The cases or an error if something went wrong.
    Either LoadError [SPKICase]
loadSPKITestVector = either (Left . YamlParseError) (traverse toSPKICase . spkiTestVector) . decodeEither'

-- | Convert a possibly-successfully loaded SPKI fingerprint test case to a canonical form.
toSPKICase :: SPKICase' -> Either LoadError SPKICase
toSPKICase (SPKICase' (Right expected) (Right hash) [cert]) = Right $ SPKICase expected (SPKIHash hash) cert
toSPKICase (SPKICase' (Left err) _ _) = Left $ TestVectorDataError $ "Error loading expected field: " <> T.pack (show err)
toSPKICase (SPKICase' _ (Left err) _) = Left $ TestVectorDataError $ "Error loading hash field: " <> T.pack (show err)
toSPKICase (SPKICase' _ _ certs) = Left $ TestVectorDataError $ "Error loading certs field: " <> T.pack (show certs)

-- | Represent an error that was encountered while trying to load the test data.
data LoadError = YamlParseError ParseException | TestVectorDataError T.Text deriving (Show)
