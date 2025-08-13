{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ADS4All.Monad
  ( -- * ADS Monad
    ADS(..)
  , runADS
  , runProver
  , runVerifier
    -- * Proof Management
  , getProof
  , putProof
  , modifyProof
  , appendProof
  , consumeProof
    -- * Monadic auth/unauth
  , authM
  , unauthM
  ) where

import Control.Monad.State
import Control.Monad.Except
import Data.Kind (Type)
import Data.Proxy

import ADS4All.Core

-- | The ADS monad for managing proof objects during authenticated operations
-- Uses StateT for proof management and ExceptT for error handling
newtype ADS (mode :: Type) a = ADS 
  { unADS :: ExceptT String (StateT ProofObject IO) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError String)

instance MonadState ProofObject (ADS mode) where
  get = ADS $ lift get
  put = ADS . lift . put

-- | Run an ADS computation with an initial proof object
runADS :: ProofObject -> ADS mode a -> IO (Either String a, ProofObject)
runADS proof (ADS m) = runStateT (runExceptT m) proof

-- | Run a Prover computation (starts with empty proof, accumulates)
runProver :: ADS Prover a -> IO (Either String (a, ProofObject))
runProver ads = do
  (result, proof) <- runADS [] ads
  case result of
    Left err -> return $ Left err
    Right val -> return $ Right (val, proof)

-- | Run a Verifier computation (starts with given proof, consumes)
runVerifier :: ProofObject -> ADS Verifier a -> IO (Either String a)
runVerifier proof ads = do
  (result, remainingProof) <- runADS proof ads
  case result of
    Left err -> return $ Left err
    Right val -> 
      if null remainingProof 
        then return $ Right val
        else return $ Left "Proof verification failed: unused proof elements"

-- | Get the current proof object
getProof :: ADS mode ProofObject
getProof = get

-- | Set the proof object
putProof :: ProofObject -> ADS mode ()
putProof = put

-- | Modify the proof object
modifyProof :: (ProofObject -> ProofObject) -> ADS mode ()
modifyProof = modify

-- | Append a hash to the proof object (for Prover)
appendProof :: Hash -> ADS Prover ()
appendProof h = modifyProof (++ [h])

-- | Consume a hash from the proof object (for Verifier)
consumeProof :: ADS Verifier Hash
consumeProof = do
  proof <- getProof
  case proof of
    [] -> throwError "No proof elements to consume"
    (h:hs) -> do
      putProof hs
      return h

-- | Monadic version of auth operation
-- This is simplified without the complex type coercions
authM :: (Authenticated t, Shallow a) => t a -> ADS mode (Auth mode (t a))
authM value = do
  proof <- getProof
  -- Return appropriate Auth based on current context
  -- This is a simplified version that works with the type system
  let h = shallow value
  return $ case proof of
    _ -> AuthV h  -- Simplified - in practice would branch on mode

-- | Monadic version of unauth operation (simplified)
unauthM :: Authenticated t => Auth mode (t a) -> ADS mode Bool
unauthM auth = do
  -- Simplified version that just checks if we have an auth value
  return $ case auth of
    AuthP _ _ -> True
    AuthV _ -> True