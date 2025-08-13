import Criterion.Main
import Control.DeepSeq
import System.Random

import ADS4All.Core
import ADS4All.Hash
import ADS4All.BST
import ADS4All.Merkle.Binary

-- Generate test data
genTestData :: Int -> [Int]
genTestData n = take n $ randomRs (1, 10000) (mkStdGen 42)

-- Benchmark BST operations
benchBST :: Int -> Benchmark
benchBST n = bgroup ("BST " ++ show n) 
  [ bench "insert" $ nf (foldl (flip insert) empty) (genTestData n)
  , bench "lookup" $ nf (map (`lookup` testTree)) (genTestData n)
  ]
  where
    testTree = foldl (flip insert) empty (genTestData n)

-- Benchmark Merkle Tree operations  
benchMerkle :: Int -> Benchmark
benchMerkle n = bgroup ("Merkle " ++ show n)
  [ bench "fromList" $ nf fromList (genTestData n)
  , bench "rootHash" $ nf rootHash (fromList (genTestData n))
  ]

main :: IO ()
main = defaultMain
  [ bgroup "Hash Operations"
    [ bench "hash int" $ nf hash (42 :: Int)
    , bench "hash string" $ nf hash "hello world"
    , bench "combineHashes" $ nf combineHashes (replicate 100 (hash (42 :: Int)))
    ]
  , benchBST 100
  , benchBST 1000
  , benchMerkle 100
  , benchMerkle 1000
  ]