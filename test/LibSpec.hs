module Main where

import Test.Hspec
-- import Test.Hspec.QuickCheck

import Numeric.Classification.DecisionTrees


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  -- describe "Numeric.Classification.DecisionTrees" $ do
  --   it "The (regularized) entropy of a uniform distribution is == 1" $ 
  --     entropyR dsUniform `shouldBe` 1.0
  --   it "The (regularized) entropy of a degenerate distribution is := 0" $ 
  --     entropyR dsEmpty `shouldBe` 0.0      
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x


-- dsUniform, dsEmpty :: Dataset Int [Double]
-- dsUniform = fromList [(1, [2,3,4,5]), (2, [1,2,3,5]) ]

-- dsEmpty = fromList []
