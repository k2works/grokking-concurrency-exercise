module Ch05Spec (spec) where

import Test.Hspec
import Ch02.PasswordCracker (getCryptoHash)
import Ch05.PasswordCrackerParallel

spec :: Spec
spec = describe "Ch05.PasswordCrackerParallel" $ do
    describe "crackPasswordParallel" $ do
        it "finds password when it exists" $ do
            let targetHash = getCryptoHash "ba"
            result <- crackPasswordParallel targetHash "ab" 2
            result `shouldBe` Just "ba"

        it "returns Nothing when password not found" $ do
            let targetHash = getCryptoHash "xyz"
            result <- crackPasswordParallel targetHash "ab" 2
            result `shouldBe` Nothing

        it "returns Nothing for length zero" $ do
            let targetHash = getCryptoHash "ab"
            result <- crackPasswordParallel targetHash "ab" 0
            result `shouldBe` Nothing

        it "finds longer password" $ do
            let targetHash = getCryptoHash "cab"
            result <- crackPasswordParallel targetHash "abc" 3
            result `shouldBe` Just "cab"
