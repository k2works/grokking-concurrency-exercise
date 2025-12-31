module Ch04.ThreadBasics
    ( createThread
    , runThreads
    ) where

import Control.Concurrent (forkIO, ThreadId)
import Control.Concurrent.Async (async, wait, Async)
import Control.Monad (forM, forM_)

-- | Create and start a new thread
createThread :: IO () -> IO ThreadId
createThread = forkIO

-- | Run multiple threads and wait for completion
runThreads :: Int -> (Int -> IO ()) -> IO ()
runThreads count action = do
    handles <- forM [0..count-1] $ \i -> async (action i)
    forM_ handles wait
