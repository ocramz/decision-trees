{-# language TypeFamilies #-}
module Numeric.Classification.Utils where

-- import qualified Data.Foldable as F (maximumBy, foldl', toList)
import qualified Data.Set as S
import qualified Data.IntMap.Strict as IM

import System.Random.MWC
import Control.Monad.Primitive

import Control.Monad (foldM, replicateM)
import Data.Maybe (maybeToList)

-- import Control.Monad.Catch (MonadThrow(..))
-- import Numeric.Classification.Exceptions

-- | Sample with replacement
resample :: PrimMonad m => Int -> IM.IntMap b -> Gen (PrimState m) -> m [b]
resample nsamples im gen = do
  ixs <- replicateM nsamples (uniformR (0, n - 1) gen)
  pure $ lookups im ixs
  where
    n = IM.size im

-- | Sample without replacement : return a list of at most M unique random samples from an indexed map of size N : O(N)
sample :: PrimMonad m => Int -> IM.IntMap b -> Gen (PrimState m) -> m [b]
sample nsamples im gen = do
  ixs <- S.toList <$> sampleUniques nsamples gen n
  pure $ lookups im ixs
  where
    n = IM.size im

lookups :: (Monoid (t b), Traversable t) => IM.IntMap b -> t IM.Key -> t b
lookups im ixs = mconcat . maybeToList $ traverse (`IM.lookup` im) ixs




-- sampleNoReplace iml nsamples gen
--   | nsamples > n = pure $ throwM $ DimMismatchE "sampleIM" n nsamples
--   | otherwise = do


-- | Sample without replacement : choose a set S of M unique random samples from a population of size N
sampleUniques :: PrimMonad m =>
                 Int   -- ^ # of unique numbers to sample (M)
              -> Gen (PrimState m)
              -> Int   -- ^ Population size (N)
              -> m (S.Set Int)  
sampleUniques nsamples gen n = foldM sample1 S.empty [p .. n] where
  p = n - nsamples + 1
  sample1 s j = do
    t <- uniformR (0, j) gen
    let set' =
          if not (S.member t s)
          then
            S.insert t s
          else
            S.insert j s
    return set'

-- stest n ntot = withSystemRandom . asGenIO $ \g -> do
--   let set = S.fromList [0..ntot - 1]
--   sampleUniques set n g




-- * Playground

class Foldable f => Indexed f where
  type Ix f :: *
  ix :: Ix f -> f a -> Maybe a

instance Indexed [] where
  type Ix [] = Int
  ix = indexSafe

instance Indexed IM.IntMap where
  type Ix IM.IntMap = IM.Key
  ix = IM.lookup

indexSafe :: Int -> [a] -> Maybe a
indexSafe i ll | i < length ll = Just $ ll !! i
               | otherwise = Nothing
