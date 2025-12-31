module Ch07Spec (spec) where

import Test.Hspec
import qualified Data.Map.Strict as Map
import Ch07.VoteCounter
import Ch07.Pipeline

spec :: Spec
spec = do
    describe "Ch07.VoteCounter" $ do
        describe "countVotes" $ do
            it "returns empty map for empty input" $ do
                countVotes [] `shouldBe` Map.empty

            it "counts votes correctly" $ do
                let votes = ["A", "B", "A", "A", "B", "C"]
                let result = countVotes votes
                Map.lookup "A" result `shouldBe` Just 3
                Map.lookup "B" result `shouldBe` Just 2
                Map.lookup "C" result `shouldBe` Just 1

        describe "countVotesParallel" $ do
            it "counts votes correctly in parallel" $ do
                let votes = ["A", "B", "A", "A", "B", "C"]
                result <- countVotesParallel votes
                Map.lookup "A" result `shouldBe` Just 3
                Map.lookup "B" result `shouldBe` Just 2
                Map.lookup "C" result `shouldBe` Just 1

    describe "Ch07.Pipeline" $ do
        describe "Pipeline" $ do
            it "processes through all stages" $ do
                let pipeline = addStage (\x -> x - 3)
                             $ addStage (*2)
                             $ addStage (+1)
                             $ newPipeline
                -- (5 + 1) * 2 - 3 = 9
                runPipeline pipeline (5 :: Int) `shouldBe` 9

            it "processes multiple inputs" $ do
                let pipeline = addStage (*2) newPipeline
                runPipelineAll pipeline [1, 2, 3, 4, 5] `shouldBe` [2, 4, 6, 8, 10]

        describe "concurrentPipeline" $ do
            it "processes through concurrent stages" $ do
                let processors = [(+1), (*2)]
                result <- concurrentPipeline processors [1, 2, 3, 4, 5]
                -- (x + 1) * 2: [4, 6, 8, 10, 12]
                result `shouldBe` [4, 6, 8, 10, 12]
