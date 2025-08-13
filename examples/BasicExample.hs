#!/usr/bin/env stack
-- stack --resolver lts-20.26 script

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified ADS4All.BST as BST
import qualified ADS4All.Hash as Hash
import ADS4All.Core

-- Simple example demonstrating ADS4All usage
main :: IO ()
main = do
  putStrLn "ADS4All Basic Example"
  putStrLn "===================="
  
  -- Create a simple BST
  let tree = BST.insert 5 $ BST.insert 3 $ BST.insert 7 $ BST.empty
  putStrLn $ "Created BST with elements [3, 5, 7]"
  putStrLn $ "BST size: " ++ show (BST.size tree)
  putStrLn $ "BST height: " ++ show (BST.height tree)
  putStrLn $ "In-order traversal: " ++ show (BST.inorder tree)
  
  -- Test lookup
  case BST.lookup 5 tree of
    Just val -> putStrLn $ "Found value: " ++ show val
    Nothing -> putStrLn "Value not found"
    
  -- Hash the tree
  let treeHash = Hash.shallow tree
  putStrLn $ "Tree hash: " ++ Hash.hashToHex treeHash
  
  putStrLn "Example completed successfully!"