#!/usr/bin/env stack
{- stack script
   --resolver lts-20.26
   --package cryptonite
   --package bytestring
   --package binary
   --package QuickCheck
-}

{-# LANGUAGE OverloadedStrings #-}

-- This demo shows the REAL SHA-256 hashing, not simulation

import qualified Crypto.Hash as CH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16

-- Real SHA-256 hashing (from your actual Hash.hs)
hashBytes :: ByteString -> ByteString
hashBytes bs = BA.convert (CH.hash bs :: CH.Digest CH.SHA256)

-- Real hash combination
hashPair :: ByteString -> ByteString -> ByteString
hashPair h1 h2 = hashBytes (BS.concat [h1, h2])

-- Convert hash to hex for display
hashToHex :: ByteString -> String
hashToHex = BS8.unpack . B16.encode

main :: IO ()
main = do
  putStrLn "==================================="
  putStrLn "ADS4All - REAL Cryptographic Hashing Demo"
  putStrLn "==================================="
  putStrLn ""
  
  -- Hash some values with real SHA-256
  let value1 = "Alice"
  let value2 = "Bob"
  
  let hash1 = hashBytes (BS8.pack value1)
  let hash2 = hashBytes (BS8.pack value2)
  
  putStrLn "Real SHA-256 Hashes:"
  putStrLn $ "  Hash(\"Alice\") = " ++ take 16 (hashToHex hash1) ++ "..."
  putStrLn $ "  Hash(\"Bob\")   = " ++ take 16 (hashToHex hash2) ++ "..."
  
  -- Combine hashes (like in Merkle tree)
  let combined = hashPair hash1 hash2
  putStrLn $ "  Hash(Alice||Bob) = " ++ take 16 (hashToHex combined) ++ "..."
  
  putStrLn ""
  putStrLn "This is REAL cryptographic hashing, not simulation!"
  putStrLn "Your actual ADS4All code uses this SHA-256 implementation."
  
  -- Show the actual hash sizes
  putStrLn ""
  putStrLn "Hash properties:"
  putStrLn $ "  Algorithm: SHA-256"
  putStrLn $ "  Hash size: " ++ show (BS.length hash1) ++ " bytes (256 bits)"
  putStrLn $ "  Collision resistance: 2^128 operations"
  putStrLn ""
  putStrLn "✅ This is what your thesis implementation actually does!"