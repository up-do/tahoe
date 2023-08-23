{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

{- | Demonstrate the use of some GBS client APIs.

 Usage:

  client-test <storage-furl> <chk-read-cap> <share-num>
-}
module Main where

import Data.ByteString.Base32 (encodeBase32Unpadded)
import qualified Data.Text as T
import System.Environment (getArgs)
import Tahoe.CHK.Capability (
    CHK (CHKReader),
    Reader (Reader, verifier),
    Verifier (Verifier, storageIndex),
    pCapability,
 )
import TahoeLAFS.Storage.API (ShareNumber (..))
import TahoeLAFS.Storage.Client (
    getImmutableShareNumbers,
    parseNURL,
    readImmutableShare,
    runGBS,
    version,
 )
import Text.Megaparsec (parse)

main :: IO ()
main = do
    [storageNURLStr, capStr, shareNumStr] <- getArgs
    let Right (CHKReader Reader{verifier = Verifier{..}}) = parse pCapability "argv[2]" (T.pack capStr)
        nurlM = parseNURL . T.pack $ storageNURLStr

    case nurlM of
        Nothing ->
            print ("Failed to parse NURL" :: T.Text)
        Just nurl -> do
            result <- runGBS nurl $ do
                ver <- version
                sharez <- getImmutableShareNumbers storageIndexS
                chk <- readImmutableShare storageIndexS shareNum Nothing
                pure (ver, sharez, chk)

            case result of
                Left err -> print $ "Request error:  " <> show err
                Right (ver, sharez, chk) -> do
                    print $ "version: " <> show ver
                    print $ "share numbers: " <> show sharez
                    print $ "share bytes: " <> show chk
          where
            storageIndexS = T.unpack . T.toLower . encodeBase32Unpadded $ storageIndex
            shareNum = ShareNumber $ read shareNumStr
