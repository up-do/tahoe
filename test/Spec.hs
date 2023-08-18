module Spec where

import Test.Hspec

import qualified CBORSpec as C
import qualified HTTPSpec as H
import qualified MiscSpec as M
import qualified SemanticSpec as S

spec :: Spec
spec = parallel $ do
    describe "HTTP" H.spec
    describe "Misc" M.spec
    describe "Semantic" S.spec
    describe "CBOR" C.spec
