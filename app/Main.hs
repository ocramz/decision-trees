{-# language DeriveGeneric #-}
module Main where

import Numeric.Classification

import Numeric.Datasets.Iris (Iris(..), IrisClass(..), iris)

import GHC.Generics

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Vector as V

irisV :: MonadThrow m => Iris -> m (V Double)
irisV (Iris sl sw pl pw iclass) = mkV 5 $ V.fromList [sl, sw, pl, pw, iclassd] where
  iclassd = fromIntegral (fromEnum iclass)


main = do --print $ head iris
  iv <- traverse irisV iris -- irisV $ head iris
  print iv


