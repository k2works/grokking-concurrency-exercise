module Ch06Spec (spec) where

import Test.Hspec
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Ch06.GameLoop

spec :: Spec
spec = describe "Ch06.GameLoop" $ do
    describe "GameTask" $ do
        it "executes task action" $ do
            counter <- newTVarIO (0 :: Int)
            let task = GameTask "test" $ atomically $ modifyTVar' counter (+1)
            taskAction task
            result <- readTVarIO counter
            result `shouldBe` 1

    describe "ProcessorFreeEvent" $ do
        it "signals and waits correctly" $ do
            event <- newProcessorFreeEvent
            _ <- forkIO $ do
                threadDelay 10000
                signalEvent event
            waitEvent event
            -- If we get here, the event was signaled

    describe "gameLoop" $ do
        it "processes tasks for specified iterations" $ do
            counter <- newTVarIO (0 :: Int)
            let task1 = GameTask "task1" $ atomically $ modifyTVar' counter (+1)
            let task2 = GameTask "task2" $ atomically $ modifyTVar' counter (+10)
            gameLoop [task1, task2] 3
            result <- readTVarIO counter
            -- 3 iterations * (1 + 10) = 33
            result `shouldBe` 33

    describe "cooperativeGameLoop" $ do
        it "processes tasks and signals event" $ do
            counter <- newTVarIO (0 :: Int)
            event <- newProcessorFreeEvent
            let task = GameTask "task1" $ atomically $ modifyTVar' counter (+1)
            cooperativeGameLoop [task] event 5
            result <- readTVarIO counter
            result `shouldBe` 5
