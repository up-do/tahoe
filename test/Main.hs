{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Amazonka (runResourceT)
import qualified Amazonka as AWS
import qualified Amazonka.S3 as S3
import Amazonka.S3.Lens (delete_objects, listObjectsResponse_contents, object_key)
import qualified Amazonka.S3.Lens as S3
import Control.Lens (view, (.~), (?~), (^.), _Just)
import Control.Monad (
    guard,
    unless,
    void,
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Network.HTTP.Types as HTTP
import qualified System.IO as IO
import Tahoe.Storage.Backend (
    WriteVector (WriteVector),
 )
import Tahoe.Storage.Backend.S3 (
    S3Backend (..),
    applyWriteVectors,
    newS3Backend,
 )
import Tahoe.Storage.Testing.Spec (
    makeStorageSpec,
 )
import Test.Hspec (
    Spec,
    context,
    describe,
    hspec,
    it,
    parallel,
    shouldBe,
 )

main :: IO ()
main = hspec . parallel . describe "S3" $ spec

spec :: Spec
spec = do
    context "utilities" $ do
        describe "applyWriteVectors" $ do
            it "replaces existing bytes with new bytes" $ do
                applyWriteVectors "abc" [WriteVector 0 "x", WriteVector 1 "y", WriteVector 2 "z"] `shouldBe` "xyz"
            it "can extend the length of the result" $ do
                applyWriteVectors "abc" [WriteVector 3 "xyz"] `shouldBe` "abcxyz"
            it "fills gaps with NULs" $ do
                applyWriteVectors "abc" [WriteVector 5 "xyz"] `shouldBe` "abc\0\0xyz"
            it "applies writes left-to-right" $ do
                applyWriteVectors "abc" [WriteVector 0 "x", WriteVector 0 "y"] `shouldBe` "ybc"
            it "accepts partial overlaps" $ do
                applyWriteVectors "abc" [WriteVector 0 "xy", WriteVector 1 "zw"] `shouldBe` "xzw"

    context "backend" $ do
        describe "S3Backend" $ makeStorageSpec s3Backend cleanupS3

-- Delete all objects with a prefix matching this backend.  Leave the
-- bucket alone in case there are other objects unrelated to this bucket
-- in it.
cleanupS3 :: S3Backend -> IO ()
cleanupS3 (S3Backend{s3BackendEnv, s3BackendBucket, s3BackendPrefix}) = runResourceT $ do
    resp <- AWS.send s3BackendEnv (S3.listObjects_prefix ?~ s3BackendPrefix $ S3.newListObjects s3BackendBucket)
    let objectKeys = catMaybes . traverse (S3.newObjectIdentifier . (^. object_key) <$>) $ resp ^. listObjectsResponse_contents

    unless (null objectKeys) . void $
        AWS.send s3BackendEnv (S3.newDeleteObjects s3BackendBucket (delete_objects .~ objectKeys $ S3.newDelete))

s3Backend :: IO S3Backend
s3Backend = runResourceT $ do
    let setLocalEndpoint = AWS.setEndpoint False "127.0.0.1" 9000
        setAddressingStyle s = s{AWS.s3AddressingStyle = AWS.S3AddressingStylePath}

    logger <- AWS.newLogger AWS.Debug IO.stdout

    env <- AWS.newEnv AWS.discover
    let loggedEnv = env -- {AWS.logger = logger}
        pathEnv = AWS.overrideService setAddressingStyle loggedEnv
        localEnv = AWS.overrideService setLocalEndpoint pathEnv
        env' = localEnv
    bucket <- AWS.sendEither env' (S3.newCreateBucket name)
    case bucket of
        -- AWS accepts duplicate create as long as you are the creator of the
        -- existing bucket.  MinIO returns a 409 Conflict response in that
        -- case.
        Left (AWS.ServiceError err) -> if HTTP.statusCode (err ^. AWS.serviceError_status) == 409 then pure () else error (show err)
        Left err -> error (show err)
        Right _ -> do
            exists <- AWS.await env' S3.newBucketExists (S3.newHeadBucket name)
            guard (exists == AWS.AcceptSuccess)

    prefix <- unusedPrefixFrom env' name $ T.singleton <$> ['a' .. 'z']
    liftIO $ newS3Backend env' name prefix
  where
    name = S3.BucketName "45336944-25bf-4fb1-8e82-9ba2631bda67"

    unusedPrefixFrom _ _ [] = error "Could not find an unused prefix"
    unusedPrefixFrom env bucket (letter : more) = do
        contents <- (^. listObjectsResponse_contents . _Just) <$> AWS.send env (S3.newListObjects bucket)
        let keys = T.pack . show . view object_key <$> contents
        if any (prefix `T.isPrefixOf`) keys
            then unusedPrefixFrom env bucket more
            else do
                -- Create a placeholder so we will immediately consider this
                -- prefix used.  This is still race-y still we look before we
                -- leap but the alternative is screwing with a bunch of
                -- complex AWS locking configuration that I don't feel like
                -- doing, especially just for this test suite which will
                -- mostly not have anything to race against anyway.
                void $ AWS.send env (S3.newPutObject bucket (S3.ObjectKey prefix) (AWS.toBody ("placeholder" :: String)))

                -- Then just return the letter and let the backend put in a
                -- separator if it wants to.
                pure letter
      where
        prefix = letter <> "/"
