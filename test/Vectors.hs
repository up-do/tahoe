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

data SPKICase = SPKICase
    { spkiExpected :: B.ByteString
    , spkiExpectedHash :: SPKIHash
    , spkiCertificate :: Certificate
    }
    deriving (Eq, Show)

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

newtype SPKITestVector = SPKITestVector
    { spkiTestVector :: [SPKICase']
    }
    deriving (Eq, Show)

instance FromJSON SPKITestVector where
    parseJSON = withObject "SPKITestVector" $ \o -> SPKITestVector <$> o .: "vector"

loadSPKITestVector :: B.ByteString -> Either LoadError [SPKICase]
loadSPKITestVector = either (Left . YamlParseError) (traverse toSPKICase . spkiTestVector) . decodeEither'

toSPKICase (SPKICase' (Right expected) (Right hash) [cert]) = Right $ SPKICase expected (SPKIHash hash) cert
toSPKICase (SPKICase' (Left err) _ _) = Left $ TestVectorDataError $ "Error loading expected field: " <> T.pack (show err)
toSPKICase (SPKICase' _ (Left err) _) = Left $ TestVectorDataError $ "Error loading hash field: " <> T.pack (show err)
toSPKICase (SPKICase' _ _ certs) = Left $ TestVectorDataError $ "Error loading certs field: " <> T.pack (show certs)

data LoadError = YamlParseError ParseException | TestVectorDataError T.Text deriving (Show)
