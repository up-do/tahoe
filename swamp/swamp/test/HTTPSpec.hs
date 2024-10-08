{-# LANGUAGE OverloadedStrings #-}

module HTTPSpec (
    spec,
) where

import Data.Aeson (
    encode,
 )
import Data.Aeson.Types (
    Value (Array, Number, String),
 )
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import Network.HTTP.Types.Method (
    methodGet,
    methodPatch,
    methodPost,
 )
import TahoeLAFS.Storage.Backend.Null (
    NullBackend (NullBackend),
 )
import TahoeLAFS.Storage.Server (
    app,
 )
import Test.Hspec (
    Spec,
    describe,
    it,
 )
import Test.Hspec.Wai (
    matchBody,
    matchHeaders,
    request,
    shouldRespondWith,
    with,
    (<:>),
 )
import Test.Hspec.Wai.Matcher (
    bodyEquals,
 )
import Prelude hiding (
    replicate,
 )

-- WaiSession changed incompatibly between hspec-wai 0.9.2 and 0.11.1.  We
-- would like to work with both so just skip the explicit type signature here
-- (and below).  ghc can figure it out.
--
-- getJSON :: ByteString -> WaiSession st SResponse
getJSON path =
    request
        methodGet
        path
        [("Accept", "application/json")]
        ""

-- postJSON :: ByteString -> L.ByteString -> WaiSession st SResponse
postJSON path =
    request
        methodPost
        path
        [("Content-Type", "application/json"), ("Accept", "application/json")]

-- putShare :: ByteString -> Int64 -> WaiSession st SResponse
patchShare path size =
    request
        methodPatch
        path
        [("Content-Type", "application/octet-stream"), ("Accept", "application/json")]
        (L.replicate size 0xdd)

allocateBucketsJSON :: L.ByteString
allocateBucketsJSON =
    encode $
        Map.fromList
            [ ("renew-secret" :: String, String "abcdefgh")
            , ("cancel-secret" :: String, String "ijklmnop")
            , ("share-numbers" :: String, Array (Vector.fromList [Number 1, Number 3, Number 5]))
            , ("allocated-size" :: String, Number 512)
            ]

allocateResultJSON :: L.ByteString
allocateResultJSON =
    encode $
        Map.fromList
            [ ("already-have" :: String, Array Vector.empty)
            , ("allocated" :: String, Array Vector.empty)
            ]

corruptionJSON :: L.ByteString
corruptionJSON =
    encode $
        Map.fromList
            [ ("reason" :: String, "foo and bar" :: String)
            ]

sharesResultJSON :: L.ByteString
-- Simple enough I won't go through Aeson here
sharesResultJSON = "[]"

spec :: Spec
spec = with (return $ app NullBackend) $
    describe "v1" $ do
        describe "GET /storage/v1/version" $ do
            it "responds with OK" $
                getJSON "/storage/v1/version" `shouldRespondWith` 200

        describe "POST /storage/v1/immutable/abcdefgh" $ do
            it "responds with CREATED" $
                postJSON
                    "/storage/v1/immutable/abcdefgh"
                    allocateBucketsJSON
                    `shouldRespondWith` 201
                        { -- TODO: ;charset=utf-8 is just an artifact of Servant, would be
                          -- nice to turn it off and not assert it here.
                          matchHeaders = ["Content-Type" <:> "application/json;charset=utf-8"]
                        , matchBody = bodyEquals allocateResultJSON
                        }

        describe "PATCH /storage/v1/immutable/abcdefgh/1" $ do
            it "responds with CREATED" $
                patchShare "/storage/v1/immutable/abcdefgh/1" 512 `shouldRespondWith` 201

        describe "POST /storage/v1/immutable/abcdefgh/1/corrupt" $ do
            it "responds with OK" $
                postJSON
                    "/storage/v1/immutable/abcdefgh/1/corrupt"
                    corruptionJSON
                    `shouldRespondWith` 200

        describe "GET /storage/v1/immutable/abcdefgh/shares" $ do
            it "responds with OK and a JSON list" $
                getJSON "/storage/v1/immutable/abcdefgh/shares"
                    `shouldRespondWith` 200
                        { matchBody = bodyEquals sharesResultJSON
                        }

        describe "GET /storage/v1/immutable/abcdefgh/1" $ do
            it "responds with OK and an application/octet-stream of the share" $ do
                let req =
                        request
                            methodGet
                            "/storage/v1/immutable/abcdefgh/1"
                            [("Accept", "application/octet-stream")]
                            ""
                req
                    `shouldRespondWith` 200
                        { matchBody = bodyEquals ""
                        }
