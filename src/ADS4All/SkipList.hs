{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ADS4All.SkipList
  ( -- * Skip List Data Type
    SkipList(..)
  , Node(..)
  , Level
    -- * Basic Operations
  , empty
  , singleton
  , insert
  , lookup
  , delete
  , size
  , height
  , toList
    -- * Authenticated Operations
  , AuthSkipList
  , authSkipList
  , lookupAuth
  , insertAuth
  , deleteAuth
    -- * Proof Types
  , SkipProof(..)
  , LevelProof
  , generateLookupProof
  , verifyLookupProof
    -- * Utilities
  , maxLevel
  , randomLevel
  ) where

import Prelude hiding (lookup)
import GHC.Generics
import Data.Binary
import Control.Monad.State
import System.Random
import Data.Maybe (isJust)
import qualified Data.Vector as V

import ADS4All.Core
import ADS4All.Hash
import ADS4All.Monad

-- | Skip list level (0 is bottom level)
type Level = Int

-- | Maximum level for skip list (log n performance guarantee)
maxLevel :: Int
maxLevel = 16

-- | Node in the skip list with multiple forward pointers
data Node a = Node 
  { nodeValue :: a
  , nodeLevel :: Level
  , forward   :: V.Vector (Maybe (Node a))
  } deriving (Generic, Binary)

instance Eq a => Eq (Node a) where
  n1 == n2 = nodeValue n1 == nodeValue n2

instance Show a => Show (Node a) where
  show (Node v l _) = "Node(" ++ show v ++ ",L" ++ show l ++ ")"

-- | Skip List data structure
data SkipList a = SkipList
  { header     :: Node a          -- Header node (sentinel)
  , currentLevel :: Level         -- Current maximum level
  , listSize   :: Int             -- Number of elements
  } deriving (Generic, Binary)

instance Show a => Show (SkipList a) where
  show sl = "SkipList(size=" ++ show (listSize sl) ++ 
           ",level=" ++ show (currentLevel sl) ++ ")"

-- | Proof for skip list operations
data SkipProof a = SkipProof
  { searchPath  :: [LevelProof a]  -- Path taken during search
  , targetValue :: Maybe a         -- Value found (if any)
  } deriving (Generic, Binary, Show)

-- | Proof for each level during search
type LevelProof a = (Level, Hash, Maybe Hash)  -- (level, current_hash, next_hash)

-- | Authenticated Skip List type
type AuthSkipList mode a = Auth mode (SkipList a)

-- | Create empty skip list
empty :: Ord a => SkipList a
empty = SkipList
  { header = Node 
      { nodeValue = error "Header node has no value"
      , nodeLevel = maxLevel
      , forward = V.replicate (maxLevel + 1) Nothing
      }
  , currentLevel = 0
  , listSize = 0
  }

-- | Create singleton skip list
singleton :: (Ord a, RandomGen g) => a -> State g (SkipList a)
singleton x = do
  level <- randomLevel
  let node = Node x level (V.replicate (level + 1) Nothing)
  return $ SkipList
    { header = Node 
        { nodeValue = error "Header node has no value"
        , nodeLevel = maxLevel
        , forward = V.generate (maxLevel + 1) $ \i -> 
            if i <= level then Just node else Nothing
        }
    , currentLevel = level
    , listSize = 1
    }

-- | Generate random level for new node (geometric distribution)
randomLevel :: RandomGen g => State g Level
randomLevel = do
  level <- go 0
  return $ min level maxLevel
  where
    go :: RandomGen g => Level -> State g Level
    go l = do
      coin <- state $ randomR (0, 1 :: Int)
      if coin == 1 && l < maxLevel
        then go (l + 1)
        else return l

-- | Insert a value into the skip list
insert :: (Ord a, RandomGen g) => a -> SkipList a -> State g (SkipList a)
insert x sl@(SkipList h curLevel sz) = do
  let update = V.replicate (maxLevel + 1) Nothing
  
  -- Find insertion point
  let (updateVector, found) = findInsertionPoint x sl update
  
  if found
    then return sl  -- Value already exists
    else do
      level <- randomLevel
      let newLevel = max curLevel level
      
      -- Create new node
      let newNode = Node x level (V.replicate (level + 1) Nothing)
      
      -- Update forward pointers
      let updatedNode = updateForwardPointers newNode updateVector level
      let updatedHeader = updateHeaderPointers h updateVector newLevel level newNode
      
      return $ SkipList updatedHeader newLevel (sz + 1)

-- | Find insertion point and return update vector
findInsertionPoint :: Ord a => a -> SkipList a -> V.Vector (Maybe (Node a)) 
                   -> (V.Vector (Maybe (Node a)), Bool)
findInsertionPoint x (SkipList h curLevel _) update = go h curLevel update False
  where
    go current level upd found
      | level < 0 = (upd, found)
      | otherwise = 
          case V.unsafeIndex (forward current) level of
            Nothing -> go current (level - 1) upd found
            Just next -> 
              if nodeValue next < x
                then go next level upd found
                else if nodeValue next == x
                     then (V.unsafeUpd upd [(level, Just current)], True)
                     else go current (level - 1) 
                            (V.unsafeUpd upd [(level, Just current)]) found

-- | Update forward pointers for new node
updateForwardPointers :: Node a -> V.Vector (Maybe (Node a)) -> Level -> Node a
updateForwardPointers newNode updateVector level =
  newNode { forward = V.generate (level + 1) $ \i ->
    case V.unsafeIndex updateVector i of
      Nothing -> Nothing
      Just updateNode -> V.unsafeIndex (forward updateNode) i
  }

-- | Update header forward pointers
updateHeaderPointers :: Node a -> V.Vector (Maybe (Node a)) -> Level -> Level -> Node a -> Node a
updateHeaderPointers h updateVector newLevel level newNode =
  h { forward = V.generate (maxLevel + 1) $ \i ->
    if i <= level
      then case V.unsafeIndex updateVector i of
             Nothing -> if i <= newLevel then Just newNode else Nothing
             Just updateNode -> 
               let oldForward = V.unsafeIndex (forward updateNode) i
               in V.unsafeIndex (forward updateNode) i
      else V.unsafeIndex (forward h) i
  }

-- | Lookup a value in the skip list
lookup :: Ord a => a -> SkipList a -> Maybe a
lookup x (SkipList h curLevel _) = go h curLevel
  where
    go current level
      | level < 0 = Nothing
      | otherwise = 
          case V.unsafeIndex (forward current) level of
            Nothing -> go current (level - 1)
            Just next -> 
              if nodeValue next < x
                then go next level
                else if nodeValue next == x
                     then Just (nodeValue next)
                     else go current (level - 1)

-- | Delete a value from the skip list
delete :: Ord a => a -> SkipList a -> SkipList a
delete x sl@(SkipList h curLevel sz) = 
  let update = V.replicate (maxLevel + 1) Nothing
      (updateVector, toDelete) = findNodeToDelete x sl update
  in case toDelete of
       Nothing -> sl  -- Value not found
       Just node -> 
         let updatedHeader = deleteNodePointers h updateVector node
             newLevel = findNewLevel updatedHeader curLevel
         in SkipList updatedHeader newLevel (sz - 1)

-- | Find node to delete
findNodeToDelete :: Ord a => a -> SkipList a -> V.Vector (Maybe (Node a)) 
                 -> (V.Vector (Maybe (Node a)), Maybe (Node a))
findNodeToDelete x (SkipList h curLevel _) update = go h curLevel update
  where
    go current level upd
      | level < 0 = 
          case V.unsafeIndex (forward current) 0 of
            Just next | nodeValue next == x -> (upd, Just next)
            _ -> (upd, Nothing)
      | otherwise = 
          case V.unsafeIndex (forward current) level of
            Nothing -> go current (level - 1) upd
            Just next -> 
              if nodeValue next < x
                then go next level upd
                else go current (level - 1) 
                       (V.unsafeUpd upd [(level, Just current)])

-- | Delete node by updating pointers
deleteNodePointers :: Node a -> V.Vector (Maybe (Node a)) -> Node a -> Node a
deleteNodePointers h updateVector nodeToDelete =
  h { forward = V.generate (maxLevel + 1) $ \i ->
    case V.unsafeIndex updateVector i of
      Nothing -> V.unsafeIndex (forward h) i
      Just updateNode -> V.unsafeIndex (forward nodeToDelete) i
  }

-- | Find new maximum level after deletion
findNewLevel :: Node a -> Level -> Level
findNewLevel h curLevel = go curLevel
  where
    go level
      | level <= 0 = 0
      | case V.unsafeIndex (forward h) level of
          Nothing -> go (level - 1)
          Just _ -> level

-- | Get size of skip list
size :: SkipList a -> Int
size = listSize

-- | Get height (maximum level) of skip list
height :: SkipList a -> Int
height = currentLevel

-- | Convert skip list to sorted list
toList :: SkipList a -> [a]
toList (SkipList h _ _) = go (V.unsafeIndex (forward h) 0)
  where
    go Nothing = []
    go (Just node) = nodeValue node : go (V.unsafeIndex (forward node) 0)

-- | Compute hash of a Skip List (shallow projection)
instance Shallow a => Shallow (SkipList a) where
  shallow sl = combineHashes 
    [ hashBytes "SkipList"
    , hash (listSize sl)
    , hash (currentLevel sl)
    , shallowNodeList (toList sl)
    ]
    where
      shallowNodeList [] = hashBytes "EmptyList"
      shallowNodeList xs = hashList (map shallow xs)

-- | Make SkipList an authenticated data structure
instance Authenticated SkipList where
  type AuthT SkipList = SkipList
  
  authP sl = AuthP (shallow sl) sl
  authV sl = AuthV (shallow sl)
  
  unauthP (AuthP h sl) = (sl, h)
  unauthP _ = error "Invalid auth value for Prover"
  
  unauthV (AuthV h) expectedHash = h == expectedHash
  unauthV _ _ = False

-- | Authenticate a SkipList
authSkipList :: Shallow a => SkipList a -> ADS mode (AuthSkipList mode a)
authSkipList = authM

-- | Authenticated lookup with proof generation
lookupAuth :: forall mode a. (Ord a, Shallow a) 
           => a 
           -> SkipList a 
           -> ADS mode (Maybe a, SkipProof a)
lookupAuth target sl@(SkipList h curLevel _) = do
  proof <- generateSearchProof target sl
  result <- return $ lookup target sl
  return (result, proof)

-- | Generate proof for search operation
generateSearchProof :: Shallow a => a -> SkipList a -> ADS mode (SkipProof a)
generateSearchProof target (SkipList h curLevel _) = do
  path <- go h curLevel []
  return $ SkipProof path (lookup target (SkipList h curLevel 0))
  where
    go current level acc
      | level < 0 = return (reverse acc)
      | otherwise = do
          let currentHash = hashNode current
          case V.unsafeIndex (forward current) level of
            Nothing -> do
              let levelProof = (level, currentHash, Nothing)
              go current (level - 1) (levelProof : acc)
            Just next -> do
              let nextHash = hashNode next
              if nodeValue next < target
                then go next level ((level, currentHash, Just nextHash) : acc)
                else go current (level - 1) ((level, currentHash, Just nextHash) : acc)

-- | Hash a node for proof generation
hashNode :: Shallow a => Node a -> Hash
hashNode (Node v l _) = combineHashes [shallow v, hash l]

-- | Generate lookup proof
generateLookupProof :: (Ord a, Shallow a) => a -> SkipList a -> SkipProof a
generateLookupProof = error "Not implemented yet"

-- | Verify lookup proof
verifyLookupProof :: Shallow a => a -> Hash -> SkipProof a -> Bool
verifyLookupProof = error "Not implemented yet"

-- | Authenticated insert
insertAuth :: (Ord a, Shallow a, RandomGen g) 
           => a 
           -> AuthSkipList mode a 
           -> StateT g (ADS mode) (AuthSkipList mode a)
insertAuth x authSL = do
  mode <- lift $ getMode (Proxy :: Proxy mode)
  case mode of
    ProverMode -> case authSL of
      AuthP _ sl -> do
        newSL <- insert x sl
        lift $ appendProof (shallow sl)
        return $ AuthP (shallow newSL) newSL
      _ -> lift $ throwError "Invalid auth skiplist for Prover"
    VerifierMode -> case authSL of
      AuthV h -> do
        -- Verifier updates hash based on proof
        newHash <- lift consumeProof
        return $ AuthV newHash
      _ -> lift $ throwError "Invalid auth skiplist for Verifier"

-- | Authenticated delete
deleteAuth :: (Ord a, Shallow a) 
           => a 
           -> AuthSkipList mode a 
           -> ADS mode (AuthSkipList mode a)
deleteAuth x authSL = do
  mode <- getMode (Proxy :: Proxy mode)
  case mode of
    ProverMode -> case authSL of
      AuthP _ sl -> do
        let newSL = delete x sl
        appendProof (shallow sl)
        return $ AuthP (shallow newSL) newSL
      _ -> throwError "Invalid auth skiplist for Prover"
    VerifierMode -> case authSL of
      AuthV h -> do
        -- Verifier updates hash based on proof
        newHash <- consumeProof
        return $ AuthV newHash
      _ -> throwError "Invalid auth skiplist for Verifier"