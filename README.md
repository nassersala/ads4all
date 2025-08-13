# ADS4All

**Authenticated Data Structures for Haskell**

## what is it?

ADS4All is a Haskell library that adds cryptographic authentication to data structures. You get data plus mathematical proof the data is correct.

## usage

```haskell
import ADS4All.Core
import ADS4All.BST

main = do
  -- Build a BST
  let tree = foldr insert Empty [5, 3, 7, 1, 9]
  
  -- Prover has the data
  let authTree = authProver tree
  
  -- Generate proof for a query  
  let (result, proof) = runProver $ lookup 3 authTree
  
  -- Verifier checks with just hash + proof
  let verified = runVerifier proof $ lookup 3 treeHash
```

## the key idea

Traditional client-server:
```haskell
balance = server.getBalance()  -- hope it's right?
```

With ADS4All:
```haskell
(balance, proof) = server.getBalance()
verified = checkProof rootHash balance proof  -- know it's right!
```

## Features

* Type-safe prover/verifier separation using GADTs
* BSTs, Skip Lists, and Merkle Trees implementations
* SHA-256 cryptographic hashing
* O(log n) proof size
* Property-based testing with QuickCheck
* Generic abstractions via type classes

## Installation

clone the repo:
```bash
git clone https://github.com/yourusername/ADS4All.git
cd ADS4All
```

build with stack:
```bash
stack build
stack test  # run tests
stack bench # run benchmarks
```

or with cabal:
```bash
cabal build
cabal test
```

To Run Demos:
```bash
runghc SimpleDemo.hs     # Basic demo
runghc CoreDemo.hs       # Full features
runghc PerformanceDemo.hs # Performance analysis
```



## quick example

Certificate transparency log without trusting the server:

```haskell
-- Server maintains authenticated tree of certificates
certTree :: Auth Prover (BST Certificate)

-- Client asks: "is this cert in the log?"
queryCert :: CertID -> (Maybe Certificate, Proof)
queryCert id = runProver $ lookup id certTree

-- Client verifies with root hash (32 bytes) + proof
-- No need to download entire log
```

## how it works

1. **Authenticated types**: `Auth Prover a` has data, `Auth Verifier a` has hashes
2. **Shallow projection**: Hash data structures incrementally  
3. **Proof generation**: Build path of hashes during operations
4. **Verification**: Reconstruct root hash from proof

## Available data structures

```haskell
-- Binary Search Trees
BST.insert :: Ord a => a -> BST a -> BST a
BST.lookup :: Ord a => a -> BST a -> Maybe a
BST.delete :: Ord a => a -> BST a -> BST a

-- Skip Lists  
SkipList.insert :: Ord a => a -> SkipList a -> SkipList a
SkipList.search :: Ord a => a -> SkipList a -> Bool
SkipList.delete :: Ord a => a -> SkipList a -> SkipList a

-- Merkle Trees
Merkle.build :: [a] -> MerkleTree a
Merkle.getProof :: Int -> MerkleTree a -> Proof
Merkle.verify :: Proof -> Hash -> Bool
```

## testing your proofs

```haskell
-- Property: authenticated lookup = regular lookup
prop_lookup_correct :: Int -> BST Int -> Bool
prop_lookup_correct x tree = 
  let (result, proof) = runProver $ lookup x (authProver tree)
      verified = runVerifier proof $ lookup x (authVerifier tree)
  in result == lookup x tree && verified

-- Property: proof size is logarithmic
prop_proof_size :: BST Int -> Bool  
prop_proof_size tree = 
  proofSize <= 2 * ceiling (logBase 2 (fromIntegral (size tree)))
```

## applications

* Certificate transparency logs
* Blockchain light clients
* Outsourced database verification
* Tamper-proof audit logs
* Cloud storage integrity

## performance

For n elements:
* Proof generation: O(log n)
* Proof size: O(log n) hashes
* Verification: O(log n)
* Storage: O(n) for tree

Example: 1 million elements = ~20 hash proof (640 bytes)

## current limitations

* BST has no balancing (can degrade to O(n))
* No persistent storage integration
* No network protocol included

## contributing

PRs welcome!

```bash
git checkout -b my-feature
# make changes
stack test  # make sure tests pass
git push origin my-feature
```

## related work

* Miller et al. "Authenticated Data Structures, Generically" (POPL 2014)
* Merkle trees (1979)
* Certificate Transparency (RFC 6962)

## Crafted By:

Nasser Ali Alzahrani as part of my PhD thesis.

## License

Not sure yet. need to talk to the uni first.
