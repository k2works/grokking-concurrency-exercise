module Ch07.Pipeline
    ( Pipeline
    , newPipeline
    , addStage
    , runPipeline
    , runPipelineAll
    , concurrentPipeline
    ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad (forM, forever)
import Control.Exception (finally)

-- | A pipeline of processing stages
newtype Pipeline a = Pipeline [a -> a]

-- | Create a new empty pipeline
newPipeline :: Pipeline a
newPipeline = Pipeline []

-- | Add a stage to the pipeline
addStage :: (a -> a) -> Pipeline a -> Pipeline a
addStage f (Pipeline stages) = Pipeline (stages ++ [f])

-- | Run the pipeline on a single input
runPipeline :: Pipeline a -> a -> a
runPipeline (Pipeline stages) input = foldl (flip ($)) input stages

-- | Run the pipeline on multiple inputs
runPipelineAll :: Pipeline a -> [a] -> [a]
runPipelineAll pipeline = map (runPipeline pipeline)

-- | Concurrent pipeline using TQueues
concurrentPipeline :: [a -> a] -> [a] -> IO [a]
concurrentPipeline [] inputs = return inputs
concurrentPipeline processors inputs = do
    -- Create queues between stages
    let numQueues = length processors + 1
    queues <- forM [1..numQueues] $ \_ -> newTQueueIO

    let inputQueue = head queues
        outputQueue = last queues

    -- Feed input
    forkIO $ do
        mapM_ (atomically . writeTQueue inputQueue . Just) inputs
        atomically $ writeTQueue inputQueue Nothing

    -- Create processor threads
    let pairs = zip3 (init queues) (tail queues) processors
    mapM_ (\(inQ, outQ, proc) -> forkIO $ processStage inQ outQ proc) pairs

    -- Collect results
    collectResults outputQueue

processStage :: TQueue (Maybe a) -> TQueue (Maybe a) -> (a -> a) -> IO ()
processStage inQ outQ processor = go
  where
    go = do
        item <- atomically $ readTQueue inQ
        case item of
            Nothing -> atomically $ writeTQueue outQ Nothing
            Just x  -> do
                atomically $ writeTQueue outQ (Just (processor x))
                go

collectResults :: TQueue (Maybe a) -> IO [a]
collectResults queue = go []
  where
    go acc = do
        item <- atomically $ readTQueue queue
        case item of
            Nothing -> return (reverse acc)
            Just x  -> go (x : acc)
