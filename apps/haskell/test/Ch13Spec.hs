module Ch13Spec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Ch13.WordCount

spec :: Spec
spec = describe "Ch13.WordCount" $ do
    describe "mapPhase" $ do
        it "converts text to word-count pairs" $ do
            let result = mapPhase "Hello World"
            result `shouldBe` [("hello", 1), ("world", 1)]

        it "returns empty list for empty string" $ do
            mapPhase "" `shouldBe` []

    describe "reducePhase" $ do
        it "aggregates word counts" $ do
            let pairs = [("hello", 1), ("world", 1), ("hello", 1)]
            let result = reducePhase pairs
            Map.lookup "hello" result `shouldBe` Just 2
            Map.lookup "world" result `shouldBe` Just 1

    describe "countWords" $ do
        it "counts words in multiple texts" $ do
            let texts = ["hello world", "hello haskell", "world of haskell"]
            let result = countWords texts
            Map.lookup "hello" result `shouldBe` Just 2
            Map.lookup "world" result `shouldBe` Just 2
            Map.lookup "haskell" result `shouldBe` Just 2
            Map.lookup "of" result `shouldBe` Just 1

        it "returns empty map for empty input" $ do
            countWords [] `shouldBe` Map.empty

    describe "countWordsParallel" $ do
        it "counts words in parallel" $ do
            let texts = ["hello world", "hello haskell", "world of haskell"]
            result <- countWordsParallel texts
            Map.lookup "hello" result `shouldBe` Just 2
            Map.lookup "world" result `shouldBe` Just 2
            Map.lookup "haskell" result `shouldBe` Just 2
            Map.lookup "of" result `shouldBe` Just 1

        it "handles large input" $ do
            let text = "the quick brown fox jumps over the lazy dog"
            let texts = replicate 100 text
            result <- countWordsParallel texts
            Map.lookup "the" result `shouldBe` Just 200
            Map.lookup "fox" result `shouldBe` Just 100
