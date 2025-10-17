module Spec where

import Test.Hspec
import Lib 

main :: IO ()
main = hspec $ do
  describe "Lib.add" $ do
    it "adds two numbers correctly" $ do
      add 2 3 `shouldBe` 5
      add (-1) 1 `shouldBe` 0
      add 0 0 `shouldBe` 0

    it "is commutative" $ do
      add 1 2 `shouldBe` add 2 1
      add (-3) 5 `shouldBe` add 5 (-3)
