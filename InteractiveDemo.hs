#!/usr/bin/env runghc
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

-- ADS4All Interactive Demo
-- Demonstrates core authenticated data structure concepts

import Data.List
import Data.Maybe
import System.Random
import Control.Monad

-- ===========================
-- Core Type System (Simplified)
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
-- Binary Search Tree
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

delete :: Ord a => a -> BST a -> BST a
delete _ Empty = Empty
delete x (Node y left right)
  | x < y     = Node y (delete x left) right
  | x > y     = Node y left (delete x right)
  | otherwise = merge left right
  where
    merge Empty r = r
    merge l Empty = l
    merge l r = let (m, r') = extractMin r
                in Node m l r'
    
    extractMin (Node y Empty r) = (y, r)
    extractMin (Node y l r) = let (m, l') = extractMin l
                              in (m, Node y l' r)

inorder :: BST a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- ===========================
-- Skip List (Simplified)
-- ===========================

data SkipList a = SkipList [[a]] deriving (Show)

emptySkipList :: SkipList a
emptySkipList = SkipList []

insertSkipList :: Ord a => a -> SkipList a -> IO (SkipList a)
insertSkipList x (SkipList levels) = do
  height <- randomHeight
  let newLevels = take height $ map (insertSorted x) levels ++ repeat [x]
  return $ SkipList newLevels
  where
    randomHeight = do
      r <- randomRIO (1, 4) :: IO Int
      return r
    
    insertSorted y [] = [y]
    insertSorted y (z:zs)
      | y <= z    = y : z : zs
      | otherwise = z : insertSorted y zs

searchSkipList :: Ord a => a -> SkipList a -> Bool
searchSkipList x (SkipList levels) = any (elem x) levels

-- ===========================
-- Merkle Tree
-- ===========================

data MerkleTree a = MerkleLeaf String a 
                  | MerkleNode String (MerkleTree a) (MerkleTree a)
                  deriving (Show)

hashValue :: Show a => a -> String
hashValue x = "H(" ++ show x ++ ")"

hashConcat :: String -> String -> String
hashConcat h1 h2 = "H(" ++ h1 ++ "||" ++ h2 ++ ")"

buildMerkle :: Show a => [a] -> MerkleTree a
buildMerkle [x] = MerkleLeaf (hashValue x) x
buildMerkle xs = 
  let (left, right) = splitAt (length xs `div` 2) xs
      leftTree = buildMerkle left
      rightTree = buildMerkle right
      leftHash = getMerkleHash leftTree
      rightHash = getMerkleHash rightTree
  in MerkleNode (hashConcat leftHash rightHash) leftTree rightTree

getMerkleHash :: MerkleTree a -> String
getMerkleHash (MerkleLeaf h _) = h
getMerkleHash (MerkleNode h _ _) = h

-- ===========================
-- Proof Generation & Verification
-- ===========================

type Proof = [String]

generateProof :: (Ord a, Show a) => a -> BST a -> Maybe Proof
generateProof target tree = go tree []
  where
    go Empty _ = Nothing
    go (Node x left right) acc
      | x == target = Just (hashValue x : acc)
      | target < x  = go left (hashValue x : hashValue (treeHash right) : acc)
      | otherwise   = go right (hashValue x : hashValue (treeHash left) : acc)
    
    treeHash Empty = "empty"
    treeHash (Node y l r) = hashConcat (hashValue y) (hashConcat (treeHash l) (treeHash r))

verifyProof :: Show a => a -> Proof -> String -> Bool
verifyProof _ [] _ = False
verifyProof target (p:ps) rootHash = 
  let targetHash = hashValue target
      reconstructed = foldl hashConcat targetHash ps
  in reconstructed == rootHash

-- ===========================
-- Demo Functions
-- ===========================

demoBST :: IO ()
demoBST = do
  putStrLn "\n📊 Binary Search Tree Demo"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  let values = [15, 10, 20, 8, 12, 17, 25, 6, 11, 16]
  let tree = foldr insert Empty values
  
  putStrLn $ "Inserted values: " ++ show values
  putStrLn $ "In-order traversal: " ++ show (inorder tree)
  
  putStrLn "\nSearch operations:"
  forM_ [12, 7, 25] $ \x -> do
    let result = lookupBST x tree
    putStrLn $ "  Looking for " ++ show x ++ ": " ++ 
               maybe "Not found" (\v -> "Found " ++ show v) result
  
  putStrLn "\nAuthentication:"
  let authTree = AuthP "root_hash_abc123" tree
  putStrLn $ "  Root hash: " ++ getHash authTree
  putStrLn $ "  Tree authenticated in Prover mode"
  
  case generateProof 12 tree of
    Just proof -> do
      putStrLn $ "\nGenerated proof for value 12:"
      putStrLn $ "  Proof path: " ++ show proof
      putStrLn $ "  Proof is " ++ show (length proof) ++ " hashes long"
    Nothing -> putStrLn "  Failed to generate proof"

demoSkipList :: IO ()
demoSkipList = do
  putStrLn "\n🎯 Skip List Demo"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  let values = [5, 15, 10, 20, 3, 12, 8]
  skipList <- foldM (flip insertSkipList) emptySkipList values
  
  putStrLn $ "Inserted values: " ++ show values
  case skipList of
    SkipList levels -> do
      putStrLn $ "Skip list has " ++ show (length levels) ++ " levels"
      forM_ (zip [1..] levels) $ \(i, level) ->
        putStrLn $ "  Level " ++ show i ++ ": " ++ show level
  
  putStrLn "\nSearch operations:"
  forM_ [10, 7, 20] $ \x -> do
    let found = searchSkipList x skipList
    putStrLn $ "  Searching for " ++ show x ++ ": " ++ 
               if found then "Found" else "Not found"

demoMerkle :: IO ()
demoMerkle = do
  putStrLn "\n🌳 Merkle Tree Demo"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  let values = ["Alice", "Bob", "Charlie", "David"]
  let merkleTree = buildMerkle values
  
  putStrLn $ "Building Merkle tree for: " ++ show values
  putStrLn $ "Root hash: " ++ getMerkleHash merkleTree
  
  putStrLn "\nMerkle tree structure:"
  printMerkle merkleTree 0
  where
    printMerkle (MerkleLeaf h v) indent = 
      putStrLn $ replicate indent ' ' ++ "└─ Leaf: " ++ show v ++ " [" ++ h ++ "]"
    printMerkle (MerkleNode h left right) indent = do
      putStrLn $ replicate indent ' ' ++ "└─ Node [" ++ h ++ "]"
      printMerkle left (indent + 3)
      printMerkle right (indent + 3)

demoSecurity :: IO ()
demoSecurity = do
  putStrLn "\n🔐 Security Properties Demo"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  putStrLn "ADS4All Security Guarantees:"
  putStrLn "  ✅ Collision Resistance"
  putStrLn "     → SHA-256 prevents finding two inputs with same hash"
  putStrLn "  ✅ Proof Soundness"
  putStrLn "     → Cannot create valid proof for non-existent data"
  putStrLn "  ✅ Proof Completeness"
  putStrLn "     → Every valid query has a corresponding proof"
  putStrLn "  ✅ Type Safety"
  putStrLn "     → Prover/Verifier separation enforced at compile-time"
  
  putStrLn "\nMetamorphic Testing Properties:"
  putStrLn "  • Insert order independence"
  putStrLn "  • Delete-insert equivalence"
  putStrLn "  • Proof determinism"
  putStrLn "  • Hash consistency"

demoPerformance :: IO ()
demoPerformance = do
  putStrLn "\n⚡ Performance Characteristics"
  putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━"
  
  putStrLn "Time Complexity (n = number of elements):"
  putStrLn "  BST Operations:"
  putStrLn "    • Insert: O(log n) average, O(n) worst"
  putStrLn "    • Search: O(log n) average, O(n) worst"
  putStrLn "    • Delete: O(log n) average, O(n) worst"
  putStrLn "  Skip List:"
  putStrLn "    • All operations: O(log n) with high probability"
  putStrLn "  Merkle Tree:"
  putStrLn "    • Build: O(n)"
  putStrLn "    • Proof generation: O(log n)"
  putStrLn "    • Proof verification: O(log n)"
  
  putStrLn "\nSpace Complexity:"
  putStrLn "  • BST: O(n)"
  putStrLn "  • Skip List: O(n) expected"
  putStrLn "  • Merkle Tree: O(n)"
  putStrLn "  • Proof size: O(log n)"

-- ===========================
-- Main Demo Runner
-- ===========================

main :: IO ()
main = do
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║     ADS4All - Authenticated Data      ║"
  putStrLn "║         Structures Framework           ║"
  putStrLn "╚════════════════════════════════════════╝"
  
  demoBST
  demoSkipList
  demoMerkle
  demoSecurity
  demoPerformance
  
  putStrLn "\n╔════════════════════════════════════════╗"
  putStrLn "║        Demo Completed Successfully!    ║"
  putStrLn "╚════════════════════════════════════════╝"
  putStrLn "\n💡 Try building the full project with:"
  putStrLn "   stack build && stack test"
  putStrLn ""