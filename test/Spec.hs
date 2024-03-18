module Main where

import Test.Tasty (TestTree, defaultMain, testGroup)

import System.IO (hSetEncoding, stderr, stdout, utf8)

import qualified Codec.FEC as FEC
import Control.Exception (evaluate)
import Control.Monad (void)
import qualified SpecCHK
import qualified SpecCrypto
import qualified SpecMerkle
import qualified SpecSSK
import qualified SpecServer
import qualified SpecUEB
import qualified SpecUpload
import qualified SpecUtil
import qualified SpecZFEC
import Test.Tasty.HUnit (assertBool, testCase)
import Vectors (loadTestVectorData)

tests :: [TestTree]
tests =
    [ SpecUpload.tests
    , SpecCrypto.tests
    , SpecMerkle.tests
    , SpecZFEC.tests
    , SpecUtil.tests
    , SpecUEB.tests
    , SpecServer.tests
    , SpecCHK.tests
    ]

failurePlaceholder :: String -> String -> TestTree
failurePlaceholder name msg = testCase name $ assertBool msg False

main :: IO ()
main = do
    -- Hedgehog writes some non-ASCII and the whole test process will die if
    -- it can't be encoded.  Increase the chances that all of the output can
    -- be encoded by forcing the use of UTF-8 (overriding the LANG-based
    -- choice normally made).
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    -- fec <= 0.1.1 has a bug under multithreaded usage where concurrent
    -- implicit initialization from different threads corrupts some of its
    -- internal state.  fec > 0.1.1 exposes an `initialize` function to allow
    -- this to be avoided but for now we can just do any initialization prior
    -- to multithreaded work to avoid this.
    --
    -- Use evaluate to force evaluation at this position in the IO, otherwise
    -- we have no idea when `FEC.fec ...` will actually cause initialization
    -- of the underlying lib.  Replace this with `FEC.initialize` when
    -- possible.
    void $ evaluate $ FEC.fec 2 3

    testVectors <- loadTestVectorData
    let testVectorsTree =
            either
                (failurePlaceholder "CHK" . ("Test vectors: " ++) . show)
                SpecCHK.testsFromVectors
                testVectors
    defaultMain $ testGroup "Tahoe Capabilities" [testGroup "CHK" $ testVectorsTree : tests, SpecSSK.tests]
