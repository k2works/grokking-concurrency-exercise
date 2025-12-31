module Ch13.WordCount
    ( mapPhase
    , reducePhase
    , countWords
    , countWordsParallel
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Char (toLower, isAlphaNum)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (foldl')

-- | Map: Convert text to (word, 1) pairs
mapPhase :: String -> [(String, Int)]
mapPhase text =
    [(map toLower word, 1) | word <- words text, not (null word)]

-- | Reduce: Aggregate word counts
reducePhase :: [(String, Int)] -> Map String Int
reducePhase = foldl' addPair Map.empty
  where
    addPair acc (word, count) = Map.insertWith (+) word count acc

-- | MapReduce: Count words in multiple texts (sequential)
countWords :: [String] -> Map String Int
countWords texts =
    let mapped = concatMap mapPhase texts
    in reducePhase mapped

-- | MapReduce: Count words using parallel execution
countWordsParallel :: [String] -> IO (Map String Int)
countWordsParallel [] = return Map.empty
countWordsParallel texts = do
    -- Map phase (parallel)
    mapped <- mapConcurrently (return . mapPhase) texts
    -- Reduce phase
    return $ reducePhase (concat mapped)
