{-# LANGUAGE FlexibleInstances #-}

module Tahoe.Directory.Internal.Capability where

import qualified Data.Text as T
import Data.Void (Void)
import qualified Tahoe.CHK.Capability as CHK
import Tahoe.Capability (ConfidentialShowable (..))
import qualified Tahoe.SDMF as SDMF

import Text.Megaparsec (Parsec, getInput, setInput)

{- | A wrapper around some other capability type which signals that the
 plaintext is an encoded list of files.
-}
newtype DirectoryCapability a = DirectoryCapability a deriving (Eq, Ord, Show)

instance ConfidentialShowable (DirectoryCapability CHK.Verifier) where
    confidentiallyShow (DirectoryCapability a) =
        T.replace "URI:CHK-Verifier:" "URI:DIR2-CHK-Verifier:" (CHK.dangerRealShow (CHK.CHKVerifier a))

instance ConfidentialShowable (DirectoryCapability CHK.Reader) where
    confidentiallyShow (DirectoryCapability a) =
        T.replace "URI:CHK:" "URI:DIR2-CHK:" (CHK.dangerRealShow (CHK.CHKReader a))

instance ConfidentialShowable (DirectoryCapability SDMF.Verifier) where
    confidentiallyShow (DirectoryCapability a) =
        T.replace "URI:SSK-Verifier:" "URI:DIR2-Verifier:" (confidentiallyShow a)

instance ConfidentialShowable (DirectoryCapability SDMF.Reader) where
    confidentiallyShow (DirectoryCapability a) =
        T.replace "URI:SSK-RO:" "URI:DIR2-RO:" (confidentiallyShow a)

instance ConfidentialShowable (DirectoryCapability SDMF.Writer) where
    confidentiallyShow (DirectoryCapability a) =
        T.replace "URI:SSK:" "URI:DIR2:" (confidentiallyShow a)

type Parser = Parsec Void T.Text

{- | Parse a CHK directory verifier capability.

 The implementation is a cheesy hack that does string substitution on the
 input before applying the original CHK verifier parser.
-}
pVerifyCHK :: Parser (DirectoryCapability CHK.Verifier)
pVerifyCHK = do
    s <- getInput
    setInput $ T.replace "URI:DIR2-CHK-Verifier:" "URI:CHK-Verifier:" s
    DirectoryCapability <$> CHK.pVerifier

{- | Parse a CHK directory reader capability.

 The implementation is a cheesy hack that does string substitution on the
 input before applying the original CHK reader parser.
-}
pReadCHK :: Parser (DirectoryCapability CHK.Reader)
pReadCHK = do
    s <- getInput
    setInput $ T.replace "URI:DIR2-CHK:" "URI:CHK:" s
    DirectoryCapability <$> CHK.pReader

{- | Parse an SDMF directory verifier capability.  As is the case for the other
 directory capability parsers, the implementation is cheesy.
-}
pVerifySDMF :: Parser (DirectoryCapability SDMF.Verifier)
pVerifySDMF = do
    s <- getInput
    setInput $ T.replace "URI:DIR2-Verifier:" "URI:SSK-Verifier:" s
    DirectoryCapability <$> SDMF.pVerifier

{- | Parse an SDMF directory reader capability.  As is the case for the other
 directory capability parsers, the implementation is cheesy.
-}
pReadSDMF :: Parser (DirectoryCapability SDMF.Reader)
pReadSDMF = do
    s <- getInput
    setInput $ T.replace "URI:DIR2-RO:" "URI:SSK-RO:" s
    DirectoryCapability <$> SDMF.pReader

{- | Parse an SDMF directory writer capability.  As is the case for the other
 directory capability parsers, the implementation is cheesy.
-}
pWriteSDMF :: Parser (DirectoryCapability SDMF.Writer)
pWriteSDMF = do
    s <- getInput
    setInput $ T.replace "URI:DIR2:" "URI:SSK:" s
    DirectoryCapability <$> SDMF.pWriter
