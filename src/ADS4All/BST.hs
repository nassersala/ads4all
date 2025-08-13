{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ADS4All.BST
  ( -- * BST Data Type
    BST(..)
  , Path(..)
  , Direction(..)
    -- * Basic Operations
  , empty
  , singleton
  , insert
  , lookup
  , delete
  , size
  , height
  , inorder
    -- * Authenticated Operations
  , AuthBST
  , authBST
  , lookupAuth
  , insertAuth
  , deleteAuth
  , verifyPath
    -- * Proof Generation
  , ProofPath
  , generateProof
  , verifyProof
  ) where

import Prelude hiding (lookup)
import GHC.Generics
import Data.Binary
import Control.Monad.State
import Data.Maybe (isJust)

import ADS4All.Core
import ADS4All.Hash
import ADS4All.Monad

-- | Binary Search Tree data structure
data BST a = Leaf              -- Empty tree
           | Node (BST a) a (BST a)  -- Node with left subtree, value, right subtree
           deriving (Eq, Show, Generic, Binary)

-- | Direction in tree traversal
data Direction = L | R
  deriving (Eq, Show, Generic, Binary)

-- | Path through the tree
type Path = [Direction]

-- | Proof path includes sibling hashes at each level
type ProofPath = [(Direction, Hash)]

-- | Authenticated BST type
type AuthBST mode a = Auth mode (BST a)

-- | Empty tree
empty :: BST a
empty = Leaf

-- | Create a singleton tree
singleton :: a -> BST a
singleton x = Node Leaf x Leaf

-- | Insert a value into the BST
insert :: Ord a => a -> BST a -> BST a
insert x Leaf = singleton x
insert x (Node l v r)
  | x < v     = Node (insert x l) v r
  | x > v     = Node l v (insert x r)
  | otherwise = Node l x r  -- Replace if equal

-- | Lookup a value in the BST
lookup :: Ord a => a -> BST a -> Maybe a
lookup _ Leaf = Nothing
lookup x (Node l v r)
  | x < v     = lookup x l
  | x > v     = lookup x r
  | otherwise = Just v

-- | Delete a value from the BST
delete :: Ord a => a -> BST a -> BST a
delete _ Leaf = Leaf
delete x (Node l v r)
  | x < v     = Node (delete x l) v r
  | x > v     = Node l v (delete x r)
  | otherwise = deleteNode l r
  where
    deleteNode Leaf r' = r'
    deleteNode l' Leaf = l'
    deleteNode l' r' = Node l' minVal (delete minVal r')
      where minVal = findMin r'
            findMin (Node Leaf v' _) = v'
            findMin (Node l'' _ _) = findMin l''
            findMin Leaf = error "impossible: findMin on Leaf"

-- | Get the size of the tree
size :: BST a -> Int
size Leaf = 0
size (Node l _ r) = 1 + size l + size r

-- | Get the height of the tree
height :: BST a -> Int
height Leaf = 0
height (Node l _ r) = 1 + max (height l) (height r)

-- | In-order traversal
inorder :: BST a -> [a]
inorder Leaf = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r

-- | Compute hash of a BST (shallow projection)
instance Shallow a => Shallow (BST a) where
  shallow Leaf = hashBytes "Leaf"
  shallow (Node l v r) = combineHashes 
    [ hashBytes "Node"
    , shallow l
    , shallow v
    , shallow r
    ]

-- | Make BST an authenticated data structure
instance Authenticated BST where
  type AuthT BST = BST
  
  authP tree = AuthP (shallow tree) tree
  authV tree = AuthV (shallow tree)
  
  unauthP (AuthP h tree) = (tree, h)
  unauthP _ = error "Invalid auth value for Prover"
  
  unauthV (AuthV h) expectedHash = h == expectedHash
  unauthV _ _ = False

-- | Authenticate a BST
authBST :: Shallow a => BST a -> ADS mode (AuthBST mode a)
authBST = authM

-- | Authenticated lookup with proof generation
lookupAuth :: forall mode a. (Ord a, Shallow a) 
           => a 
           -> BST a 
           -> ADS mode (Maybe a, ProofPath)
lookupAuth target tree = go tree []
  where
    go :: BST a -> ProofPath -> ADS mode (Maybe a, ProofPath)
    go Leaf path = return (Nothing, reverse path)
    go (Node l v r) path
      | target < v = do
          let siblingHash = shallow r
          go l ((L, siblingHash) : path)
      | target > v = do
          let siblingHash = shallow l
          go r ((R, siblingHash) : path)
      | otherwise = return (Just v, reverse path)

-- | Authenticated insert
insertAuth :: (Ord a, Shallow a) 
           => a 
           -> AuthBST mode a 
           -> ADS mode (AuthBST mode a)
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

-- | Authenticated delete
deleteAuth :: (Ord a, Shallow a) 
           => a 
           -> AuthBST mode a 
           -> ADS mode (AuthBST mode a)
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

-- | Generate a proof for a lookup operation
generateProof :: (Ord a, Shallow a) => a -> BST a -> ProofPath
generateProof target = go []
  where
    go path Leaf = reverse path
    go path (Node l v r)
      | target < v = go ((L, shallow r) : path) l
      | target > v = go ((R, shallow l) : path) r
      | otherwise = reverse path

-- | Verify a proof for a lookup operation
verifyProof :: Shallow a => a -> Hash -> ProofPath -> Bool
verifyProof value rootHash path = reconstructHash value path == rootHash
  where
    reconstructHash :: Shallow a => a -> ProofPath -> Hash
    reconstructHash v [] = shallow v
    reconstructHash v ((dir, siblingHash):rest) =
      let currentHash = reconstructHash v rest
      in case dir of
           L -> combineHashes [currentHash, siblingHash]
           R -> combineHashes [siblingHash, currentHash]

-- | Verify a path through the tree
verifyPath :: Shallow a => BST a -> Path -> a -> Bool
verifyPath tree path value = go tree path
  where
    go Leaf [] = False
    go (Node _ v _) [] = shallow v == shallow value
    go (Node l _ _) (L:ps) = go l ps
    go (Node _ _ r) (R:ps) = go r ps
    go _ _ = False
