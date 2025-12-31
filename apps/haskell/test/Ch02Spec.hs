module Ch02Spec (spec) where

import Test.Hspec
import Ch02.PasswordCracker

spec :: Spec
spec = describe "Ch02.PasswordCracker" $ do
    describe "getCryptoHash" $ do
        it "returns correct SHA-256 hash" $ do
            getCryptoHash "ab" `shouldBe` "fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603"

    describe "crackPassword" $ do
        it "finds password when it exists" $ do
            let targetHash = getCryptoHash "ab"
            crackPassword targetHash "ab" 2 `shouldBe` Just "ab"

        it "returns Nothing when password not found" $ do
            let targetHash = getCryptoHash "xyz"
            crackPassword targetHash "ab" 2 `shouldBe` Nothing

        it "returns Nothing for length zero" $ do
            let targetHash = getCryptoHash "ab"
            crackPassword targetHash "ab" 0 `shouldBe` Nothing
