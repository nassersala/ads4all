{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ADS4All.Merkle.Binary
  ( -- * Binary Merkle Tree Data Types
    MerkleTree(..)
  , MerkleNode(..)
  , MerkleProof(..)
  , ProofElement(..)
    -- * Basic Operations
  , empty
  , singleton
  , fromList
  , insert
  , lookup
  , delete
  , size
  , height
  , leaves
    -- * Merkle Operations
  , rootHash
  , generateProof
  , verifyProof
  , updateProof
    -- * Authenticated Operations
  , AuthMerkleTree
  , authMerkleTree
  , insertAuth
  , lookupAuth
  , deleteAuth
    -- * Utilities
  , isValidTree
  , recomputeHashes
  , balanceTree
  ) where

import Prelude hiding (lookup)
import GHC.Generics
import Data.Binary
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Vector as V

import ADS4All.Core
import ADS4All.Hash
import ADS4All.Monad

-- | Binary Merkle Tree node
data MerkleNode a = MerkleLeaf
                  | MerkleNode Hash (MerkleTree a) (MerkleTree a)
                  deriving (Eq, Show, Generic, Binary)

-- | Binary Merkle Tree
data MerkleTree a = MerkleTree
  { treeRoot :: MerkleNode a
  , treeData :: [a]  -- Original data for reconstruction
  } deriving (Eq, Show, Generic, Binary)

-- | Proof element in Merkle proof
data ProofElement = LeftSibling Hash | RightSibling Hash
  deriving (Eq, Show, Generic, Binary)

-- | Merkle proof for membership/non-membership
data MerkleProof = MerkleProof
  { proofPath     :: [ProofElement]  -- Path from leaf to root
  , proofRootHash :: Hash            -- Expected root hash
  } deriving (Eq, Show, Generic, Binary)

-- | Authenticated Merkle Tree type
type AuthMerkleTree mode a = Auth mode (MerkleTree a)

-- | Create empty Merkle tree
empty :: MerkleTree a
empty = MerkleTree MerkleLeaf []

-- | Create singleton Merkle tree
singleton :: Shallow a => a -> MerkleTree a
singleton x = fromList [x]

-- | Build Merkle tree from list of data
fromList :: Shallow a => [a] -> MerkleTree a
fromList [] = empty
fromList xs = MerkleTree (buildTree xs) xs
  where
    buildTree :: Shallow a => [a] -> MerkleNode a
    buildTree [] = MerkleLeaf
    buildTree [x] = MerkleNode (shallow x) (MerkleTree MerkleLeaf [x]) (MerkleTree MerkleLeaf [])
    buildTree ys = 
      let mid = length ys `div` 2
          (left, right) = splitAt mid ys
          leftTree = MerkleTree (buildTree left) left
          rightTree = MerkleTree (buildTree right) right
          leftHash = rootHash leftTree
          rightHash = rootHash rightTree
          combinedHash = combineHashes [leftHash, rightHash]
      in MerkleNode combinedHash leftTree rightTree

-- | Insert element into Merkle tree
insert :: Shallow a => a -> MerkleTree a -> MerkleTree a
insert x (MerkleTree _ xs) = fromList (x : xs)

-- | Lookup element in Merkle tree  
lookup :: Eq a => a -> MerkleTree a -> Maybe a
lookup x (MerkleTree _ xs) = if x `elem` xs then Just x else Nothing

-- | Delete element from Merkle tree
delete :: (Eq a, Shallow a) => a -> MerkleTree a -> MerkleTree a
delete x (MerkleTree _ xs) = fromList (filter (/= x) xs)

-- | Get size of Merkle tree
size :: MerkleTree a -> Int
size (MerkleTree _ xs) = length xs

-- | Get height of Merkle tree
height :: MerkleTree a -> Int
height (MerkleTree root _) = nodeHeight root
  where
    nodeHeight MerkleLeaf = 0
    nodeHeight (MerkleNode _ left right) = 1 + max (height left) (height right)

-- | Get all leaves of Merkle tree
leaves :: MerkleTree a -> [a]
leaves (MerkleTree _ xs) = xs

-- | Get root hash of Merkle tree
rootHash :: Shallow a => MerkleTree a -> Hash
rootHash (MerkleTree MerkleLeaf _) = hashBytes "EmptyTree"
rootHash (MerkleTree (MerkleNode h _ _) _) = h

-- | Generate Merkle proof for an element
generateProof :: (Eq a, Shallow a) => a -> MerkleTree a -> Maybe MerkleProof
generateProof target tree@(MerkleTree root _) = do
  path <- findPath target root []
  return $ MerkleProof path (rootHash tree)
  where
    findPath :: (Eq a, Shallow a) => a -> MerkleNode a -> [ProofElement] -> Maybe [ProofElement]
    findPath _ MerkleLeaf _ = Nothing
    findPath x (MerkleNode _ left right) acc = 
      case (containsElement x left, containsElement x right) of
        (True, _) -> findPath x (treeRoot left) (RightSibling (rootHash right) : acc)
        (_, True) -> findPath x (treeRoot right) (LeftSibling (rootHash left) : acc)
        _ -> Nothing
    
    containsElement :: Eq a => a -> MerkleTree a -> Bool
    containsElement x (MerkleTree _ xs) = x `elem` xs

-- | Verify Merkle proof
verifyProof :: Shallow a => a -> MerkleProof -> Bool
verifyProof element (MerkleProof path expectedRoot) = 
  reconstructRoot element path == expectedRoot
  where
    reconstructRoot :: Shallow a => a -> [ProofElement] -> Hash
    reconstructRoot x [] = shallow x
    reconstructRoot x (LeftSibling siblingHash : rest) =
      let currentHash = reconstructRoot x rest
      in combineHashes [siblingHash, currentHash]
    reconstructRoot x (RightSibling siblingHash : rest) =
      let currentHash = reconstructRoot x rest  
      in combineHashes [currentHash, siblingHash]

-- | Update proof after tree modification
updateProof :: (Eq a, Shallow a) => a -> MerkleTree a -> Maybe MerkleProof
updateProof = generateProof

-- | Check if tree structure is valid (hashes match)
isValidTree :: Shallow a => MerkleTree a -> Bool
isValidTree tree = validateNode (treeRoot tree)
  where
    validateNode :: Shallow a => MerkleNode a -> Bool
    validateNode MerkleLeaf = True
    validateNode (MerkleNode h left right) =
      let expectedHash = combineHashes [rootHash left, rootHash right]
      in h == expectedHash && validateNode (treeRoot left) && validateNode (treeRoot right)

-- | Recompute all hashes in tree
recomputeHashes :: Shallow a => MerkleTree a -> MerkleTree a
recomputeHashes (MerkleTree _ xs) = fromList xs

-- | Balance the tree (rebuild from scratch)
balanceTree :: Shallow a => MerkleTree a -> MerkleTree a
balanceTree = recomputeHashes

-- | Compute hash of Merkle Tree (shallow projection)
instance Shallow a => Shallow (MerkleTree a) where
  shallow = rootHash

-- | Make MerkleTree an authenticated data structure
instance Authenticated MerkleTree where
  type AuthT MerkleTree = MerkleTree
  
  authP tree = AuthP (shallow tree) tree
  authV tree = AuthV (shallow tree)
  
  unauthP (AuthP h tree) = (tree, h)
  unauthP _ = error "Invalid auth value for Prover"
  
  unauthV (AuthV h) expectedHash = h == expectedHash
  unauthV _ _ = False

-- | Authenticate a MerkleTree
authMerkleTree :: Shallow a => MerkleTree a -> ADS mode (AuthMerkleTree mode a)
authMerkleTree = authM

-- | Authenticated insert
insertAuth :: (Shallow a) 
           => a 
           -> AuthMerkleTree mode a 
           -> ADS mode (AuthMerkleTree mode a)
insertAuth x authTree = do
  mode <- getMode (Proxy :: Proxy mode)
  case mode of
    ProverMode -> case authTree of
      AuthP _ tree -> do
        let newTree = insert x tree
        appendProof (shallow tree)
        return $ AuthP (shallow newTree) newTree
      _ -> throwError "Invalid auth tree for Prover"
    VerifierMode -> case authTree of
      AuthV h -> do
        -- Verifier updates hash based on proof
        newHash <- consumeProof
        return $ AuthV newHash
      _ -> throwError "Invalid auth tree for Verifier"

-- | Authenticated lookup with proof generation
lookupAuth :: forall mode a. (Eq a, Shallow a) 
           => a 
           -> MerkleTree a 
           -> ADS mode (Maybe a, Maybe MerkleProof)
lookupAuth target tree = do
  let result = lookup target tree
  proof <- case result of
    Nothing -> return Nothing
    Just _ -> return $ generateProof target tree
  return (result, proof)

-- | Authenticated delete
deleteAuth :: (Eq a, Shallow a) 
           => a 
           -> AuthMerkleTree mode a 
           -> ADS mode (AuthMerkleTree mode a)
deleteAuth x authTree = do
  mode <- getMode (Proxy :: Proxy mode)
  case mode of
    ProverMode -> case authTree of
      AuthP _ tree -> do
        let newTree = delete x tree
        appendProof (shallow tree)
        return $ AuthP (shallow newTree) newTree
      _ -> throwError "Invalid auth tree for Prover"
    VerifierMode -> case authTree of
      AuthV h -> do
        -- Verifier updates hash based on proof
        newHash <- consumeProof
        return $ AuthV newHash
      _ -> throwError "Invalid auth tree for Verifier"