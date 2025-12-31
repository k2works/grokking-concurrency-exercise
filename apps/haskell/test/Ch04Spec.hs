module Ch04Spec (spec) where

import Test.Hspec
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Ch04.ThreadBasics

spec :: Spec
spec = describe "Ch04.ThreadBasics" $ do
    describe "createThread" $ do
        it "creates and runs a thread" $ do
            counter <- newTVarIO (0 :: Int)
            _ <- createThread $ atomically $ modifyTVar' counter (+1)
            -- Wait a bit for the thread to complete
            threadDelay 10000
            result <- readTVarIO counter
            result `shouldBe` 1

    describe "runThreads" $ do
        it "runs multiple threads and waits for completion" $ do
            counter <- newTVarIO (0 :: Int)
            runThreads 5 $ \_ -> atomically $ modifyTVar' counter (+1)
            result <- readTVarIO counter
            result `shouldBe` 5

        it "passes correct index to each thread" $ do
            sumVar <- newTVarIO (0 :: Int)
            runThreads 4 $ \i -> atomically $ modifyTVar' sumVar (+i)
            result <- readTVarIO sumVar
            -- 0 + 1 + 2 + 3 = 6
            result `shouldBe` 6
