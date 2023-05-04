{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module TahoeLAFS.Internal.ServantUtil (
    CBOR,
) where

import Network.HTTP.Media (
    (//),
 )

import Data.ByteString (
    ByteString,
 )

import Data.ByteString.Base64 (
    decode,
    encode,
 )

import Data.Text.Encoding (
    decodeUtf8,
    encodeUtf8,
 )

import Servant (
    Accept (..),
    MimeRender (..),
    MimeUnrender (..),
 )

import qualified Codec.Serialise as S
import Data.Aeson (
    FromJSON (parseJSON),
    ToJSON (toJSON),
    withText,
 )
import Data.Aeson.Types (
    Value (String),
 )

data CBOR

instance Accept CBOR where
    -- https://tools.ietf.org/html/rfc7049#section-7.3
    contentType _ = "application" // "cbor"

instance S.Serialise a => MimeRender CBOR a where
    mimeRender _ = S.serialise

instance S.Serialise a => MimeUnrender CBOR a where
    mimeUnrender _ bytes = Right $ S.deserialise bytes

instance ToJSON ByteString where
    toJSON bs = String $ decodeUtf8 $ encode bs

instance FromJSON ByteString where
    parseJSON =
        withText
            "String"
            ( \bs ->
                case decode $ encodeUtf8 bs of
                    Left err -> fail ("Base64 decoding failed: " ++ err)
                    Right bytes -> return bytes
            )
