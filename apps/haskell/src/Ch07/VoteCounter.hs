module Ch07.VoteCounter
    ( countVotes
    , countVotesParallel
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

-- | Count votes sequentially
countVotes :: [String] -> Map String Int
countVotes = foldl' countOne Map.empty
  where
    countOne acc vote = Map.insertWith (+) vote 1 acc

-- | Count votes using Fork/Join pattern
countVotesParallel :: [String] -> IO (Map String Int)
countVotesParallel [] = return Map.empty
countVotesParallel votes = do
    -- Split into chunks and process in parallel
    let chunks = splitIntoChunks 4 votes
    results <- mapConcurrently (return . countVotes) chunks
    return $ foldl' (Map.unionWith (+)) Map.empty results

-- | Split a list into n chunks
splitIntoChunks :: Int -> [a] -> [[a]]
splitIntoChunks n xs =
    let len = length xs
        chunkSize = (len + n - 1) `div` n
    in go chunkSize xs
  where
    go _ [] = []
    go size ys = take size ys : go size (drop size ys)
