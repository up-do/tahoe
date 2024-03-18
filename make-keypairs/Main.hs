module Main where

import qualified Crypto.PubKey.RSA as RSA
import qualified Data.ByteString as B
import Tahoe.SDMF.Internal.Keys (signatureKeyToBytes)
import Tahoe.SDMF.Keys (Signature (..))

-- | The size of the keys to generate.
bits :: Int
bits = 2048

-- | The number of keys to generate.
count :: Int
count = 5

main :: IO ()
main = do
    mapM_ genKey [0 .. count - 1]

genKey :: Show a => a -> IO ()
genKey n = do
    print ("Generating RSA key..." :: String)
    (_, priv) <- RSA.generate (bits `div` 8) e
    print $ "Serializing key " <> show n
    let bytes = signatureKeyToBytes (Signature priv)
    print $ "Generated them (" <> show (B.length bytes) <> " bytes)"
    B.writeFile ("test/data/rsa-privkey-" <> show n <> ".der") bytes
    print ("Wrote them to the file." :: String)
  where
    e = 0x10001
