module Main where

import qualified Crypto.PubKey.RSA as RSA
import Data.Binary (encode)
import Data.ByteString.Base32 (encodeBase32Unpadded)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (stdin)
import Tahoe.Capability (confidentiallyShow)
import qualified Tahoe.SDMF as SDMF
import qualified Tahoe.SDMF.Keys as SDMF.Keys

main :: IO ()
main = do
    plaintext <- LB.hGetContents stdin
    keypair <- SDMF.Keys.KeyPair . snd <$> RSA.generate (2048 `div` 8) e
    Just iv <- SDMF.randomIV

    let ciphertext = SDMF.encrypt keypair iv plaintext
    (shares, writeCap) <- SDMF.encode keypair iv 1 3 5 ciphertext
    let shareBytes = encode <$> shares

    let si = SDMF.Keys.unStorageIndex . SDMF.verifierStorageIndex . SDMF.readerVerifier . SDMF.writerReader $ writeCap

    mapM_ (uncurry (writeShare si)) (zip [0 :: Int ..] shareBytes)
    T.putStrLn $ confidentiallyShow writeCap
  where
    e = 0x10001
    writeShare si shnum = LB.writeFile $ (T.unpack . T.toLower . encodeBase32Unpadded $ si) <> "." <> show shnum
