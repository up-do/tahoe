{-# LANGUAGE OverloadedStrings #-}

module ClientSpec where

import Network.HTTP.Client (defaultRequest, managerModifyRequest, requestHeaders)
import TahoeLAFS.Internal.Client (mkGBSManagerSettings)

import Test.Hspec (
    Spec,
    describe,
    it,
    shouldBe,
    shouldContain,
 )

spec :: Spec
spec = do
    describe "mkGBSManagerSettings" $ do
        describe "Authorization header" $ do
            it "includes the Tahoe-LAFS realm and encoded swissnum" $ do
                modified <- managerModifyRequest settings request
                requestHeaders modified `shouldContain` [("authorization", "Tahoe-LAFS c3dpc3NudW0=")]
            it "does not duplicate the header" $ do
                modified <- managerModifyRequest settings request
                modified' <- managerModifyRequest settings modified
                let authorizations = filter (("authorization" ==) . fst) (requestHeaders modified')
                length authorizations `shouldBe` 1
  where
    settings = mkGBSManagerSettings "swissnum"
    request = defaultRequest
