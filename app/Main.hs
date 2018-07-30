-- {-# language DeriveGeneric #-}
module Main where

import Numeric.Classification
import Numeric.Datasets.Iris (Iris(..), IrisClass(..), iris)
-- import GHC.Generics
import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Vector as V
import qualified Data.IntMap as IM
import Control.Monad (void)

import Options.Applicative

-- main = print "hello!"

irisKV :: (MonadThrow m, Applicative f) => Iris -> m (IrisClass, f (V Double))
irisKV (Iris sl sw pl pw lab) = do
  iv <- mkV 4 $ V.fromList [sl, sw, pl, pw]
  pure (lab, pure iv)

irisDataset :: MonadThrow m => m (Dataset IrisClass [V Double])
irisDataset = do
  ivs <- traverse irisKV iris 
  pure $ fromListWith (++) ivs  

irisLabels :: FeatureLabels
irisLabels = FeatureLabels $ IM.fromList $ zip [0..] ["Sepal length", "Sepal width", "Petal length", "Petal width"]


main :: IO ()
main = do
  cliopts@(CLIOptions minls maxtd xmin xmax bindx) <-
    execParser $ info (cliOptions <**> helper) (fullDesc <> header "Iris")
  ivs <- traverse irisKV iris 
  let ivDs = fromListWith (++) ivs
      tjs = [(j, t) | j <- [0..3], t <- [xmin, bindx .. xmax]]
      opts = TOptions maxtd minls LessThan
  -- print ivDs
  -- print $ uniques round ivDs
      tr = growTree opts tjs ivDs
  -- print tr
  putStrLn ""
  putStrLn $ show cliopts
  putStrLn $ drawDecisionTree irisLabels opts $ tr --  entropyR <$> tr -- $ void tr



-- * Optparse

data CLIOptions a = CLIOptions {
    optMinLeafSize :: !Int
  , optMaxTreeDepth :: !Int
  , optFeatureMin :: a
  , optFeatureMax :: a
  , optBinSize :: a } deriving (Eq, Show)

cliOptions :: Parser (CLIOptions Double)
cliOptions = CLIOptions <$>
  option auto (
  long "min-leaf-size"
  <> short 's'
  <> help "Minimum leaf size"
  <> showDefault
  <> value 5
  <> metavar "INT" ) <*>
  option auto (
  long "max-tree-depth"
  <> short 'd'
  <> help "Maximum tree depth"
  <> showDefault
  <> value 5
  <> metavar "INT" ) <*>
  option auto (
  long "feature-min"
  <> short 'a'
  <> help "Minimum feature value"
  <> showDefault
  <> value 0
  <> metavar "DOUBLE" ) <*>
  option auto (
  long "feature-max"
  <> short 'b'
  <> help "Maximum feature value"
  <> showDefault
  <> value 10
  <> metavar "DOUBLE" ) <*>  
  option auto (
  long "bin-size"
  <> short 'x'
  <> help "Bin size"
  <> showDefault
  <> value 0.2
  <> metavar "DOUBLE" )


-- sample :: Parser Sample
-- sample = Sample
--       <$> strOption
--           ( long "hello"
--          <> metavar "TARGET"
--          <> help "Target for the greeting" )
--       <*> switch
--           ( long "quiet"
--          <> short 'q'
--          <> help "Whether to be quiet" )
--       <*> option auto
--           ( long "enthusiasm"
--          <> help "How enthusiastically to greet"
--          <> showDefault
--          <> value 1
--          <> metavar "INT" )
