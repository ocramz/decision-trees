module Main where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import Numeric.Classification.DecisionTrees 


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Numeric.Classification.DecisionTrees" $ do
    it "The entropy of a uniform distribution is == 1" $ 
      entropy ds0 `shouldBe` Just 1.0
  -- describe "Lib" $ do
  --   it "works" $ do
  --     True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x


ds0 :: Dataset Int [Double]
ds0 = fromList [(1, [2,3,4,5]), (2, [1,2,3,5]) ]
