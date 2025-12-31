{-# LANGUAGE OverloadedStrings #-}

module Ch05.PasswordCrackerParallel
    ( crackPasswordParallel
    ) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Maybe (listToMaybe, catMaybes)
import Ch02.PasswordCracker (getCryptoHash)

-- | Crack password using parallel execution
crackPasswordParallel :: String -> String -> Int -> IO (Maybe String)
crackPasswordParallel _ _ 0 = return Nothing
crackPasswordParallel cryptoHash alphabet len = do
    -- Parallelize on first character
    results <- mapConcurrently (tryFirstChar cryptoHash alphabet len) alphabet
    return $ listToMaybe (catMaybes results)

tryFirstChar :: String -> String -> Int -> Char -> IO (Maybe String)
tryFirstChar cryptoHash alphabet len firstChar =
    return $ crackRecursive cryptoHash alphabet [firstChar] (len - 1)

crackRecursive :: String -> String -> String -> Int -> Maybe String
crackRecursive cryptoHash alphabet prefix 0 =
    if getCryptoHash prefix == cryptoHash
        then Just prefix
        else Nothing
crackRecursive cryptoHash alphabet prefix remaining =
    foldr tryChar Nothing alphabet
  where
    tryChar c acc =
        case crackRecursive cryptoHash alphabet (prefix ++ [c]) (remaining - 1) of
            Just result -> Just result
            Nothing -> acc
