{-# LANGUAGE OverloadedStrings #-}

module Ch02.PasswordCracker
    ( getCryptoHash
    , crackPassword
    ) where

import Crypto.Hash.SHA256 (hash)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import qualified Data.ByteString.Lazy as BL

-- | Compute SHA-256 hash of a password
getCryptoHash :: String -> String
getCryptoHash password =
    let hashBytes = hash (C8.pack password) :: ByteString
        hexBytes = toLazyByteString (byteStringHex hashBytes)
    in C8.unpack (BL.toStrict hexBytes)

-- | Crack password by brute force (sequential)
crackPassword :: String -> String -> Int -> Maybe String
crackPassword _ _ 0 = Nothing
crackPassword cryptoHash alphabet len =
    crackRecursive cryptoHash alphabet "" len

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
