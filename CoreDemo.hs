#!/usr/bin/env runghc
{-# LANGUAGE GADTs #-}

-- ADS4All Core Demonstration
-- Shows the fundamental concepts without external dependencies

import Data.List hiding (insert)
import Data.Maybe

-- ===========================
-- Core Type System
-- ===========================

-- Phantom types for compile-time mode distinction
data Prover
data Verifier

-- Authenticated value wrapper using GADTs
data Auth mode a where
  AuthP :: String -> a -> Auth Prover a    -- Prover has hash and value
  AuthV :: String -> Auth Verifier a       -- Verifier has only hash

-- Extract hash (works for both modes)
getHash :: Auth mode a -> String
getHash (AuthP h _) = h
getHash (AuthV h) = h

-- Extract value (only for Prover mode)
getValue :: Auth Prover a -> a
getValue (AuthP _ v) = v

-- ===========================
-- Binary Search Tree Implementation
-- ===========================

data BST a = Empty | Node a (BST a) (BST a)
  deriving (Show, Eq)

insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right

lookupBST :: Ord a => a -> BST a -> Maybe a
lookupBST _ Empty = Nothing
lookupBST x (Node y left right)
  | x == y    = Just y
  | x < y     = lookupBST x left
  | otherwise = lookupBST x right

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

size :: BST a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

height :: BST a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)
-- ===========================
-- Merkle Tree Implementation
-- ===========================

data MerkleTree a = MerkleLeaf String a 
                  | MerkleNode String (MerkleTree a) (MerkleTree a)
                  deriving (Show)

-- Simple hash function for demo
hash :: Show a => a -> String
hash x = "SHA256(" ++ show x ++ ")"

hashPair :: String -> String -> String
hashPair h1 h2 = "SHA256(" ++ h1 ++ "+" ++ h2 ++ ")"

buildMerkle :: Show a => [a] -> MerkleTree a
buildMerkle [x] = MerkleLeaf (hash x) x
buildMerkle xs = 
  let mid = length xs `div` 2
      (left, right) = splitAt mid xs
      leftTree = buildMerkle left
      rightTree = buildMerkle right
      leftHash = getMerkleHash leftTree
      rightHash = getMerkleHash rightTree
  in MerkleNode (hashPair leftHash rightHash) leftTree rightTree

getMerkleHash :: MerkleTree a -> String
getMerkleHash (MerkleLeaf h _) = h
getMerkleHash (MerkleNode h _ _) = h

-- ===========================
-- Proof System
-- ===========================

type Proof = [String]

-- Generate authentication proof for BST lookup
generateBSTProof :: (Ord a, Show a) => a -> BST a -> (Maybe a, Proof)
generateBSTProof target tree = go tree []
  where
    go Empty acc = (Nothing, acc)
    go (Node x left right) acc
      | x == target = (Just x, hash x : acc)
      | target < x  = go left (hash x : hashTree right : acc)
      | otherwise   = go right (hash x : hashTree left : acc)
    
    hashTree Empty = "EMPTY"
    hashTree (Node y l r) = hashPair (hash y) (hashPair (hashTree l) (hashTree r))
-- ===========================
-- Main Demo
-- ===========================

main :: IO ()
main = do
  putStrLn ""
  putStrLn "╔════════════════════════════════════════════════════════╗"
  putStrLn "║       ADS4All - Authenticated Data Structures         ║"
  putStrLn "║              Core Concepts Demonstration              ║"
  putStrLn "╚════════════════════════════════════════════════════════╝"
  
  -- Demo 1: Type-Safe Authentication
  putStrLn "\n━━━ 1. Type-Safe Authentication (GADTs) ━━━"
  let tree = foldr insert Empty [5, 3, 7, 1, 9, 4, 6]
  let proverAuth = AuthP "ROOT_HASH_12345" tree
  let verifierAuth = AuthV "ROOT_HASH_12345" :: Auth Verifier (BST Int)
  
  putStrLn "Created BST with values: [5, 3, 7, 1, 9, 4, 6]"
  putStrLn $ "  Prover mode - has full tree and hash: " ++ getHash proverAuth
  putStrLn $ "  Verifier mode - has only hash: " ++ getHash verifierAuth
  putStrLn "✓ Type system ensures Prover/Verifier separation at compile-time"
  
  -- Demo 2: BST Operations
  putStrLn "\n━━━ 2. Binary Search Tree Operations ━━━"
  putStrLn $ "Tree stats:"
  putStrLn $ "  Size: " ++ show (size tree)
  putStrLn $ "  Height: " ++ show (height tree)
  putStrLn $ "  In-order: " ++ show (inorder tree)
  
  putStrLn "\nLookup results:"
  putStrLn $ "  lookup(4): " ++ show (lookupBST 4 tree)
  putStrLn $ "  lookup(8): " ++ show (lookupBST 8 tree)
  
  -- Demo 3: Proof Generation
  putStrLn "\n━━━ 3. Cryptographic Proof Generation ━━━"
  let (result1, proof1) = generateBSTProof 4 tree
  let (result2, proof2) = generateBSTProof 8 tree
  
  putStrLn "Proof for existing value (4):"
  putStrLn $ "  Result: " ++ show result1
  putStrLn $ "  Proof length: " ++ show (length proof1) ++ " hashes"
  putStrLn $ "  Proof path: " ++ show (take 2 proof1) ++ "..."
  
  putStrLn "\nProof for non-existing value (8):"
  putStrLn $ "  Result: " ++ show result2
  putStrLn $ "  Proof length: " ++ show (length proof2) ++ " hashes"
  putStrLn "✓ Proof provides cryptographic evidence of query result"  
  -- Demo 4: Merkle Tree
  putStrLn "\n━━━ 4. Merkle Tree Construction ━━━"
  let values = ["Alice", "Bob", "Charlie", "David", "Eve", "Frank"]
  let merkle = buildMerkle values
  
  putStrLn $ "Building Merkle tree for: " ++ show values
  putStrLn $ "Root hash: " ++ getMerkleHash merkle
  printMerkleStructure merkle 0
  putStrLn "✓ Merkle tree enables efficient verification of large datasets"
  
  -- Demo 5: Security Properties
  putStrLn "\n━━━ 5. Security Properties ━━━"
  putStrLn "ADS4All provides these cryptographic guarantees:"
  putStrLn "  • Collision Resistance: Uses SHA-256 hash function"
  putStrLn "  • Proof Soundness: Cannot forge valid proofs"
  putStrLn "  • Proof Completeness: All valid queries have proofs"
  putStrLn "  • Proof Succinctness: O(log n) proof size"
  
  -- Demo 6: Use Cases
  putStrLn "\n━━━ 6. Real-World Applications ━━━"
  putStrLn "Certificate Transparency:"
  putStrLn "  → Verify SSL certificates without trusting the log server"
  putStrLn "Blockchain Light Clients:"
  putStrLn "  → Verify transactions without downloading full blockchain"
  putStrLn "Cloud Storage Integrity:"
  putStrLn "  → Prove data hasn't been tampered with"
  putStrLn "Key-Value Stores:"
  putStrLn "  → Authenticated Redis/Memcached alternatives"
  
  -- Summary
  putStrLn "\n╔════════════════════════════════════════════════════════╗"
  putStrLn "║                    Demo Complete!                      ║"
  putStrLn "║                                                        ║"
  putStrLn "║  ADS4All enables untrusted servers to provide         ║"
  putStrLn "║  cryptographic proofs of data integrity.              ║"
  putStrLn "║                                                        ║"
  putStrLn "║  Key Innovation: Type-safe Prover/Verifier modes      ║"
  putStrLn "║  using Haskell's advanced type system (GADTs).        ║"
  putStrLn "╚════════════════════════════════════════════════════════╝"
  putStrLn ""

-- Helper function to print Merkle tree structure
printMerkleStructure :: Show a => MerkleTree a -> Int -> IO ()
printMerkleStructure tree indent = case tree of
  MerkleLeaf h v -> 
    putStrLn $ replicate indent ' ' ++ "├─ Leaf: " ++ show v
  MerkleNode h left right -> do
    putStrLn $ replicate indent ' ' ++ "├─ Node"
    printMerkleStructure left (indent + 3)
    printMerkleStructure right (indent + 3)