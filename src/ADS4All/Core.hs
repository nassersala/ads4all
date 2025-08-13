{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module ADS4All.Core
  ( -- * Core Types
    Mode(..)
  , Prover
  , Verifier
  , Auth(..)
  , ProofObject
  , Hash
    -- * Type Classes
  , Authenticated(..)
  , Shallow(..)
    -- * Core Operations
  , authProver
  , authVerifier
  , unauthProver
  , unauthVerifier
    -- * Utilities
  , getAuthHash
  , getAuthValue
  , isProverMode
  , isVerifierMode
  ) where

import Data.ByteString (ByteString)
import Data.Kind (Type)
import Data.Proxy

-- | Phantom types to distinguish between Prover and Verifier modes at compile time
data Prover
data Verifier

-- | Runtime representation of the mode (for when we need it)
data Mode = ProverMode | VerifierMode
  deriving (Eq, Show)

-- | Type alias for cryptographic hashes
type Hash = ByteString

-- | Proof object is a list of hashes that constitute the authentication proof
type ProofObject = [Hash]

-- | Authenticated value wrapper using GADTs for type-safe mode distinction
-- In Prover mode, we have both the hash and the actual value
-- In Verifier mode, we only have the hash
data Auth :: Type -> Type -> Type where
  -- | Prover has both hash and value
  AuthP :: Hash -> a -> Auth Prover a
  -- | Verifier only has hash
  AuthV :: Hash -> Auth Verifier a

-- | Extract hash from authenticated value (works for both modes)
getAuthHash :: Auth mode a -> Hash
getAuthHash (AuthP h _) = h
getAuthHash (AuthV h)   = h

-- | Extract value from authenticated value (only for Prover mode)
getAuthValue :: Auth Prover a -> a
getAuthValue (AuthP _ v) = v

-- | Type-level check for Prover mode
isProverMode :: Proxy Prover -> Bool
isProverMode _ = True

-- | Type-level check for Verifier mode  
isVerifierMode :: Proxy Verifier -> Bool
isVerifierMode _ = True

-- | Type class for computing shallow projections
-- Shallow projection hashes data up to, but not past, nested authenticated values
class Shallow a where
  shallow :: a -> Hash

-- | Type class for authenticated data structures
-- This is the core abstraction that all authenticated data structures must implement
class Authenticated t where
  -- | Associated type for the authenticated version of the structure
  type AuthT t :: Type -> Type
  
  -- | Authenticate a value (Prover mode)
  -- Takes a value and returns an authenticated wrapper containing both hash and value
  authP :: Shallow a => t a -> Auth Prover (t a)
  
  -- | Authenticate a value (Verifier mode)  
  -- Takes a value and returns only the hash
  authV :: Shallow a => t a -> Auth Verifier (t a)
  
  -- | Unauthenticate a value (Prover mode)
  -- Extracts the value and adds hash to proof object
  unauthP :: Auth Prover (t a) -> (t a, Hash)
  
  -- | Unauthenticate a value (Verifier mode)
  -- Verifies the hash matches and returns success/failure
  unauthV :: Auth Verifier (t a) -> Hash -> Bool

-- | Convenience function for Prover authentication
authProver :: (Authenticated t, Shallow a) => t a -> Auth Prover (t a)
authProver = authP

-- | Convenience function for Verifier authentication
authVerifier :: (Authenticated t, Shallow a) => t a -> Auth Verifier (t a)
authVerifier = authV

-- | Convenience function for Prover unauthentication
unauthProver :: Authenticated t => Auth Prover (t a) -> (t a, Hash)
unauthProver = unauthP

-- | Convenience function for Verifier unauthentication  
unauthVerifier :: Authenticated t => Auth Verifier (t a) -> Hash -> Bool
unauthVerifier = unauthV