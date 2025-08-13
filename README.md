# ADS4All: Authenticated Data Structures for All

A Haskell library providing type-safe, verified authenticated data structures with compile-time guarantees and comprehensive property-based testing.

## Overview

ADS4All is a novel framework for building authenticated data structures (ADS) in Haskell. It provides:

- **Type-safe authentication**: GADTs ensure prover/verifier modes are correctly distinguished at compile-time
- **Generic abstractions**: Core type classes that work with any data structure
- **Verified implementations**: Property-based testing with QuickCheck ensures correctness
- **Security properties**: Metamorphic testing verifies cryptographic security properties
- **Multiple data structures**: Binary Search Trees, Skip Lists, and Merkle Trees
- **Performance benchmarking**: Comprehensive benchmarks using Criterion

## Project Structure

```
ADS4All/
├── src/
│   └── ADS4All/
│       ├── Core.hs           # Core types and type classes
│       ├── Hash.hs           # Cryptographic hash utilities
│       ├── Proofs.hs         # Proof generation and verification
│       ├── Monad.hs          # Monadic interface for ADS operations
│       ├── Security.hs       # Security analysis utilities
│       ├── BST.hs            # Binary Search Tree implementation
│       ├── SkipList.hs       # Skip List implementation
│       └── Merkle/
│           └── Binary.hs     # Binary Merkle Tree implementation
├── test/
│   ├── Spec.hs               # Main test suite
│   └── SecurityProperties.hs # Security property tests
├── examples/
│   ├── BasicExample.hs       # Basic usage examples
│   ├── AuthenticatedExample.hs # Authentication workflow examples
│   └── SecurityDemo.hs       # Security features demonstration
├── benchmarks/
│   └── Main.hs               # Performance benchmarks
├── ads4all.cabal             # Cabal package description
├── package.yaml              # Hpack package description
├── stack.yaml                # Stack configuration
└── README.md                 # This file
```

## Installation

### Using Stack (Recommended)

```bash
# Clone the repository
git clone https://github.com/yourusername/ADS4All.git
cd ADS4All

# Build the project
stack build

# Run tests
stack test

# Run benchmarks
stack bench

# Install globally
stack install
```

### Using Cabal

```bash
# Clone the repository
git clone https://github.com/yourusername/ADS4All.git
cd ADS4All

# Build the project
cabal build

# Run tests
cabal test

# Run benchmarks
cabal bench

# Install
cabal install
```

## Quick Start

### Basic Usage

```haskell
import ADS4All.Core
import ADS4All.BST
import ADS4All.Hash

-- Create an authenticated BST
let tree = insert 5 "five" $ insert 3 "three" $ insert 7 "seven" Empty

-- Authenticate in prover mode
let authTree = authProver tree

-- Generate proof for a query
let (result, proof) = runProver $ lookup 3 authTree

-- Verify in verifier mode
let verified = runVerifier proof $ lookup 3 authTreeHash
```

### Examples

See the `examples/` directory for complete working examples:

- `BasicExample.hs`: Simple usage patterns
- `AuthenticatedExample.hs`: Full authentication workflows
- `SecurityDemo.hs`: Security features and guarantees

## Core Concepts

### Authenticated Data Structures

Authenticated Data Structures (ADS) allow untrusted servers to answer queries on behalf of trusted data owners while providing cryptographic proofs of correctness.

### Type-Safe Modes

The library uses GADTs to distinguish between Prover and Verifier modes at compile-time:

```haskell
data Auth :: Type -> Type -> Type where
  AuthP :: Hash -> a -> Auth Prover a    -- Prover has hash and value
  AuthV :: Hash -> Auth Verifier a       -- Verifier has only hash
```

### Shallow Projections

The `Shallow` type class computes hashes up to but not including nested authenticated values, enabling efficient incremental hashing.

### Property-Based Testing

Comprehensive QuickCheck properties ensure:
- Correctness of operations
- Proof validity
- Security properties
- Performance characteristics

## Testing

The test suite includes:

- **Functional correctness**: Verifies all operations work correctly
- **Authentication properties**: Ensures proofs are valid and complete
- **Security properties**: Metamorphic testing for cryptographic guarantees
- **Performance regression**: Tracks performance characteristics

Run tests with:
```bash
stack test
# or
cabal test
```

## Benchmarking

Performance benchmarks compare authenticated vs non-authenticated operations:

```bash
stack bench
# or
cabal bench
```

Benchmarks measure:
- Insert operations
- Lookup operations
- Proof generation
- Proof verification
- Memory usage

## Documentation

Generate Haddock documentation:

```bash
stack haddock
# or
cabal haddock
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## Research

This library is based on research in authenticated data structures and verified programming. Key contributions include:

- Type-safe authentication using GADTs
- Generic abstractions via type classes
- Comprehensive property-based testing
- Metamorphic testing for security properties

## License

This project is licensed under the BSD-3-Clause License. See LICENSE file for details.

## Authors

- Nasser Altubaga

## Acknowledgments

This work was completed as part of a Master's thesis on authenticated data structures.

## Contact

For questions, issues, or collaboration opportunities, please open an issue on GitHub.

---

*Built with Haskell, verified with QuickCheck, secured with cryptography.*