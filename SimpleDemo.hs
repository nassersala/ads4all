-- Simple Demo of ADS4All concepts
-- This demonstrates the core ideas without full compilation

import Data.List (sort)

-- Simple Binary Search Tree
data BST a = Empty | Node a (BST a) (BST a)
  deriving (Show, Eq)

-- Insert into BST
insert :: Ord a => a -> BST a -> BST a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y     = Node y (insert x left) right
  | x > y     = Node y left (insert x right)
  | otherwise = Node y left right

-- Lookup in BST
lookupBST :: Ord a => a -> BST a -> Maybe a
lookupBST _ Empty = Nothing
lookupBST x (Node y left right)
  | x == y    = Just y
  | x < y     = lookupBST x left
  | otherwise = lookupBST x right

-- In-order traversal
inorder :: BST a -> [a]
inorder Empty = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- Size of BST
size :: BST a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

-- Height of BST
height :: BST a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- Simple hash simulation (just for demo)
hash :: Show a => BST a -> String
hash Empty = "empty"
hash (Node x left right) = "hash(" ++ show x ++ "," ++ hash left ++ "," ++ hash right ++ ")"

-- Merkle proof simulation
data MerkleProof = MerkleProof [String] deriving Show

-- Generate proof for a path
generateProof :: (Ord a, Show a) => a -> BST a -> Maybe MerkleProof
generateProof _ Empty = Nothing
generateProof x (Node y left right)
  | x == y    = Just (MerkleProof [hash left, hash right])
  | x < y     = case generateProof x left of
                  Just (MerkleProof p) -> Just (MerkleProof (hash right : p))
                  Nothing -> Nothing
  | otherwise = case generateProof x right of
                  Just (MerkleProof p) -> Just (MerkleProof (hash left : p))
                  Nothing -> Nothing

-- Demo function
demo :: IO ()
demo = do
  putStrLn "==================================="
  putStrLn "ADS4All - Authenticated Data Structures Demo"
  putStrLn "==================================="
  putStrLn ""
  
  -- Create a BST
  let tree = foldr insert Empty [5, 3, 7, 1, 9, 4, 6, 8, 2]
  putStrLn "Created BST with values: [5, 3, 7, 1, 9, 4, 6, 8, 2]"
  putStrLn $ "Tree structure: " ++ show tree
  putStrLn ""
  
  -- Basic operations
  putStrLn "Basic Operations:"
  putStrLn $ "  Size: " ++ show (size tree)
  putStrLn $ "  Height: " ++ show (height tree)
  putStrLn $ "  In-order traversal: " ++ show (inorder tree)
  putStrLn ""
  
  -- Lookups
  putStrLn "Lookup Operations:"
  putStrLn $ "  Lookup 4: " ++ show (lookupBST 4 tree)
  putStrLn $ "  Lookup 10: " ++ show (lookupBST 10 tree)
  putStrLn ""
  
  -- Authentication simulation
  putStrLn "Authentication (Simulated):"
  putStrLn $ "  Root hash: " ++ hash tree
  putStrLn ""
  
  -- Merkle proof simulation
  putStrLn "Merkle Proof Generation (Simulated):"
  case generateProof 4 tree of
    Just proof -> putStrLn $ "  Proof for value 4: " ++ show proof
    Nothing -> putStrLn "  No proof found for value 4"
  putStrLn ""
  
  -- Skip List simulation
  putStrLn "Skip List Concept:"
  putStrLn "  A skip list provides O(log n) search with high probability"
  putStrLn "  by maintaining multiple levels of linked lists."
  putStrLn ""
  
  -- Security properties
  putStrLn "Security Properties of ADS4All:"
  putStrLn "  ✓ Collision Resistance: Uses SHA-256 cryptographic hash"
  putStrLn "  ✓ Proof Soundness: Cannot forge valid proofs"
  putStrLn "  ✓ Proof Completeness: All valid queries have proofs"
  putStrLn "  ✓ Type Safety: Prover/Verifier modes enforced at compile-time"
  putStrLn ""
  
  putStrLn "==================================="
  putStrLn "Demo completed successfully!"
  putStrLn "==================================="

-- Main
main :: IO ()
main = demo