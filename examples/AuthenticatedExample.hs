#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified ADS4All.BST as BST
import qualified ADS4All.Hash as Hash
import qualified ADS4All.Merkle.Binary as Merkle
import ADS4All.Core
import ADS4All.Monad

import Control.Monad
import System.CPUTime
import Text.Printf

-- Performance testing for authenticated data structures
main :: IO ()
main = do
  putStrLn "ADS4All Authenticated Example"
  putStrLn "============================="
  
  -- Test BST operations
  putStrLn "\nBinary Search Tree Performance:"
  testBSTPerformance [100, 500, 1000, 2000]
  
  -- Test Merkle Tree operations
  putStrLn "\nMerkle Tree Performance:"
  testMerklePerformance [100, 500, 1000, 2000]
  
  putStrLn "\nAll tests completed!"

-- Test BST performance at different sizes
testBSTPerformance :: [Int] -> IO ()
testBSTPerformance sizes = forM_ sizes $ \n -> do
  let values = [1..n]
  
  -- Measure insertion time
  start <- getCPUTime
  let tree = foldl (flip BST.insert) BST.empty values
  end <- getCPUTime
  let insertTime = fromIntegral (end - start) / (10^12)
  
  -- Measure lookup time
  start2 <- getCPUTime
  let _ = map (`BST.lookup` tree) values
  end2 <- getCPUTime
  let lookupTime = fromIntegral (end2 - start2) / (10^12)
  
  -- Calculate hash
  let treeHash = Hash.shallow tree
  let hashStr = take 16 (Hash.hashToHex treeHash) ++ "..."
  
  printf "  Size %4d: Insert %.3f ms, Lookup %.3f ms, Hash %s\n" 
         n (insertTime * 1000) (lookupTime * 1000) hashStr

-- Test Merkle Tree performance
testMerklePerformance :: [Int] -> IO ()
testMerklePerformance sizes = forM_ sizes $ \n -> do
  let values = [1..n]
  
  -- Measure creation time
  start <- getCPUTime
  let tree = Merkle.fromList values
  end <- getCPUTime
  let createTime = fromIntegral (end - start) / (10^12)
  
  -- Measure root hash computation
  start2 <- getCPUTime
  let rootHash = Merkle.rootHash tree
  end2 <- getCPUTime  
  let hashTime = fromIntegral (end2 - start2) / (10^12)
  
  let hashStr = take 16 (Hash.hashToHex rootHash) ++ "..."
  
  printf "  Size %4d: Create %.3f ms, Hash %.3f ms, Root %s\n"
         n (createTime * 1000) (hashTime * 1000) hashStr