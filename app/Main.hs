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

-- treeOptions :: TOptions
-- treeOptions = TOptions 10 5 LessThan

-- tjs = [(j, t) | j <- [0..4], t <- [0, 0.2 .. 10]]

main :: IO ()
main = do
  (CLIOptions minls maxtd bindx) <-
    execParser $ info (cliOptions <**> helper) (fullDesc <> header "Iris")
  ivs <- traverse irisKV iris 
  let ivDs = fromListWith (++) ivs
      tjs = [(j, t) | j <- [0..3], t <- [0, bindx .. 10]]
      treeOptions = TOptions minls maxtd LessThan
  -- print ivDs
  -- print $ uniques round ivDs
      tr = growTree treeOptions tjs ivDs
  -- print tr
  print $ void tr



-- * Optparse

data CLIOptions a = CLIOptions {
    optMinLeafSize :: !Int
  , optMaxTreeDepth :: !Int
  , optBinSize :: a } deriving (Eq, Show)

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
  <> value 10
  <> metavar "INT" ) <*>
  option auto (
  long "bin-size"
  <> short 'x'
  <> help "Bin size"
  <> showDefault
  <> value 0.2
  <> metavar "DOUBLE"
              )


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
