module Ch06.GameLoop
    ( GameTask(..)
    , ProcessorFreeEvent
    , newProcessorFreeEvent
    , waitEvent
    , signalEvent
    , gameLoop
    , cooperativeGameLoop
    ) where

import Control.Concurrent.STM
import Control.Monad (forM_, replicateM_)

-- | A game task that can be processed
data GameTask = GameTask
    { taskName   :: String
    , taskAction :: IO ()
    }

-- | Event for signaling processor availability
newtype ProcessorFreeEvent = ProcessorFreeEvent (TVar Bool)

-- | Create a new processor free event
newProcessorFreeEvent :: IO ProcessorFreeEvent
newProcessorFreeEvent = ProcessorFreeEvent <$> newTVarIO False

-- | Wait for the event to be signaled
waitEvent :: ProcessorFreeEvent -> IO ()
waitEvent (ProcessorFreeEvent var) = atomically $ do
    ready <- readTVar var
    if ready
        then writeTVar var False
        else retry

-- | Signal the event
signalEvent :: ProcessorFreeEvent -> IO ()
signalEvent (ProcessorFreeEvent var) = atomically $ writeTVar var True

-- | Simple game loop that processes tasks
gameLoop :: [GameTask] -> Int -> IO ()
gameLoop tasks iterations =
    replicateM_ iterations $ forM_ tasks taskAction

-- | Cooperative game loop with event signaling
cooperativeGameLoop :: [GameTask] -> ProcessorFreeEvent -> Int -> IO ()
cooperativeGameLoop tasks event iterations =
    replicateM_ iterations $ forM_ tasks $ \task -> do
        taskAction task
        signalEvent event
