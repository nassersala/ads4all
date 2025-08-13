{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module ADS4All.Hash
  ( -- * Hashing Functions
    hash
  , hashPair
  , hashList
  , hashBytes
    -- * Shallow Projection
  , Shallow(..)
    -- * Binary Serialization
  , toBinary
    -- * Hash Utilities
  , emptyHash
  , combineHashes
  , hashToHex
  , hexToHash
  ) where

import qualified Crypto.Hash as CH
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Base16 as B16
import Data.List (foldl')

import ADS4All.Core (Hash, Shallow(..))

-- | Hash a Binary-serializable value using SHA256
hash :: Binary a => a -> Hash
hash = hashBytes . toBinary

-- | Convert a Binary value to ByteString
toBinary :: Binary a => a -> ByteString
toBinary = LBS.toStrict . runPut . put

-- | Hash raw bytes using SHA256
hashBytes :: ByteString -> Hash
hashBytes bs = BA.convert (CH.hash bs :: CH.Digest CH.SHA256)

-- | Hash a pair of values
hashPair :: (Binary a, Binary b) => (a, b) -> Hash
hashPair (a, b) = hashBytes $ BS.concat [toBinary a, toBinary b]

-- | Hash a list of values
hashList :: Binary a => [a] -> Hash
hashList = hashBytes . BS.concat . map toBinary

-- | Empty hash (hash of empty bytestring)
emptyHash :: Hash
emptyHash = hashBytes BS.empty

-- | Combine multiple hashes into one
combineHashes :: [Hash] -> Hash
combineHashes [] = emptyHash
combineHashes hs = hashBytes $ BS.concat hs

-- | Convert hash to hexadecimal string representation
hashToHex :: Hash -> String
hashToHex = BS8.unpack . B16.encode

-- | Convert hexadecimal string to hash (returns Nothing if invalid)
hexToHash :: String -> Maybe Hash
hexToHash s = case B16.decode (BS8.pack s) of
  Right h -> Just h
  Left _ -> Nothing

-- | Shallow projection instances for basic types
-- These compute hashes without recursing into authenticated values

instance Shallow Int where
  shallow = hash

instance Shallow Integer where
  shallow = hash

instance Shallow Double where
  shallow = hash

instance Shallow Float where
  shallow = hash

instance Shallow Bool where
  shallow = hash

instance Shallow Char where
  shallow = hash

instance Shallow ByteString where
  shallow = hashBytes

instance Shallow String where
  shallow = hash

-- | Shallow projection for pairs
instance (Shallow a, Shallow b) => Shallow (a, b) where
  shallow (a, b) = combineHashes [shallow a, shallow b]

-- | Shallow projection for Either
instance (Shallow a, Shallow b) => Shallow (Either a b) where
  shallow (Left a) = combineHashes [hashBytes "L", shallow a]
  shallow (Right b) = combineHashes [hashBytes "R", shallow b]

-- | Shallow projection for Maybe
instance Shallow a => Shallow (Maybe a) where
  shallow Nothing = hashBytes "Nothing"
  shallow (Just a) = combineHashes [hashBytes "Just", shallow a]

-- | Shallow projection for lists
instance Shallow a => Shallow [a] where
  shallow [] = hashBytes "[]"
  shallow xs = combineHashes $ hashBytes "List" : map shallow xs

-- | Helper for creating Binary instances for custom types
-- This is useful for authenticated data structures
class HashableStruct a where
  structToBytes :: a -> ByteString
  
instance HashableStruct a => Shallow a where
  shallow = hashBytes . structToBytes