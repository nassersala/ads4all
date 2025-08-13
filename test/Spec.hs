import Test.Hspec
import Test.QuickCheck

import ADS4All.Core
import ADS4All.Hash
import ADS4All.BST
import ADS4All.Merkle.Binary

main :: IO ()
main = hspec $ do
  describe "ADS4All.Core" $ do
    it "should compile without errors" $ do
      True `shouldBe` True

  describe "ADS4All.Hash" $ do
    it "should hash basic values" $ do
      let h1 = hash (42 :: Int)
      let h2 = hash (42 :: Int)
      h1 `shouldBe` h2

  describe "ADS4All.BST" $ do
    it "should create empty BST" $ do
      let tree = empty :: BST Int
      size tree `shouldBe` 0

  describe "ADS4All.Merkle.Binary" $ do
    it "should create empty Merkle tree" $ do
      let tree = empty :: MerkleTree Int
      size tree `shouldBe` 0