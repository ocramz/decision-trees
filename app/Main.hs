{-# language DeriveGeneric #-}
module Main where

import Numeric.Classification

import Numeric.Datasets.Iris (Iris(..), IrisClass(..), iris)

import GHC.Generics

import Control.Monad.Catch (MonadThrow(..))
import qualified Data.Vector as V

irisKV :: (MonadThrow m, Applicative f) => Iris -> m (IrisClass, f (V Double))
irisKV (Iris sl sw pl pw lab) = do
  iv <- mkV 4 $ V.fromList [sl, sw, pl, pw]
  pure (lab, pure iv)

-- main = print "hello!"

main = do --print $ head iris
  ivs <- traverse irisKV iris -- irisV $ head iris
  let ivDs = fromListWith (++) ivs
  print ivDs


