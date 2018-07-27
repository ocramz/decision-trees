module Numeric.InformationTheory where

import Control.Monad.Catch (MonadThrow(..))

import Data.Dataset
import Numeric.Classification.Exceptions


-- | Entropy of a Dataset
--
-- Entropy is defined as: sum (p_i * log_2 p_i)
-- where p_i = |{ x | x has Label i}|/|Dataset|
--
-- NB: returns Nothing if any class contains 0 points (i.e. if the integration support contains any 0-measure subsets)
entropy :: (Foldable t, Ord h, Floating h, MonadThrow m) => Dataset k (t a) -> m h
entropy = entropy_ . probClasses

entropy_ :: (Traversable t, Ord a, Floating a, MonadThrow m) => t a -> m a
entropy_ ps = negate . sum <$> traverse entropyD ps where
  -- entropyD :: (Ord a, Floating a) => a -> Maybe a
  entropyD p | p > 0 = pure ( p * logBase 2 p)
             | otherwise = throwM $ ZeroProbabilityE "Zero probability bin"

-- | Differential entropy has a singularity at 0 but converges slowly to 0 for small positive values.
entropyR :: (Foldable t, Ord h, Floating h) => Dataset k (t a) -> h
entropyR = entropyR_ . probClasses

entropyR_ :: (Foldable t, Functor t, Ord c, Floating c) => t c -> c
entropyR_ ps = negate . sum $ entropyReg <$> ps where
  entropyReg p | p > 0 =  p * logBase 2 p
               | otherwise = 0

-- | Gini index (expected error rate)
gini :: (Foldable t, Floating c) => Dataset k (t a) -> c
gini = gini_ . probClasses

gini_ :: (Foldable t, Functor t, Floating c) => t c -> c
gini_ ps = 1 - sum ((**2) <$> ps)
