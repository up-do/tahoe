{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Options.Applicative (
    Parser,
    ParserInfo,
    argument,
    auto,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    str,
    strOption,
    value,
    (<**>),
 )

import Data.ByteString.Base32 (
    decodeBase32Unpadded,
 )
import Tahoe.CHK.Types (
    Parameters (..),
    Required,
    Total,
 )

import Tahoe.Server (
    directoryStorageServer',
 )

import Tahoe.CHK.Upload (
    UploadResult (uploadResultReadCap, uploadResultShareMap),
    defaultParameters,
    filesystemUploadableRandomConvergence,
    filesystemUploadableWithConvergence,
    prettyFormatSharemap,
    store,
 )

import Tahoe.CHK.Capability (
    CHK (CHKReader),
    dangerRealShow,
 )

data Config = UploadConfig
    { uploadConfigPath :: FilePath
    , uploadConfigConvergence :: Maybe B.ByteString
    , uploadConfigTotalShares :: Total
    , uploadConfigRequiredShares :: Required
    }
    deriving (Show, Eq)

uploadConfig :: Parser Config
uploadConfig =
    UploadConfig
        <$> argument str (metavar "PATH")
        <*> optional
            ( strOption
                ( long "convergence-secret"
                    <> metavar "BASE32"
                    <> help "A convergence secret to use for deriving capabilities.  The equivalent of a random convergence secret is used if not given."
                )
            )
        <*> option
            auto
            ( long "shares-total"
                <> metavar "COUNT"
                <> help "The total number of shares into which the data will be encoded."
                <> value 10
            )
        <*> option
            auto
            ( long "shares-required"
                <> metavar "COUNT"
                <> help "The minimum number of shares required to re-assemble the original data."
                <> value 3
            )

opts :: ParserInfo Config
opts =
    info
        (uploadConfig <**> helper)
        ( fullDesc
            <> progDesc "Upload some data as an immutable object and report the capability."
            <> header "tahoe-lafs-encrypt-chk"
        )

main :: IO ()
main = do
    (UploadConfig path secret total required) <- execParser opts
    let params =
            defaultParameters
                { paramTotalShares = total
                , paramRequiredShares = required
                }
    uploadable <- case secret of
        Nothing -> filesystemUploadableRandomConvergence path params
        Just b32Secret ->
            case decodeBase32Unpadded b32Secret of
                Left _err -> error "base32 decoding convergence secret failed"
                Right bytesSecret ->
                    filesystemUploadableWithConvergence bytesSecret path params

    servers <- getServers
    result <- store servers uploadable
    report_upload result
  where
    getServers =
        mapM
            directoryStorageServer'
            [ "storage001"
            , "storage002"
            , "storage003"
            , "storage004"
            , "storage005"
            ]

    report_upload :: UploadResult -> IO ()
    report_upload result = do
        Text.putStrLn . prettyFormatSharemap . uploadResultShareMap $ result
        Text.putStrLn . Text.append "Read cap: " . dangerRealShow . CHKReader . uploadResultReadCap $ result
