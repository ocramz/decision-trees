{-# language DeriveFunctor #-}
module Numeric.Classification.DecisionTrees where

import Prelude hiding (lookup)
import qualified Data.Map as M

import Data.Function (on)

-- | Computes the entropy of a Dataset
--
-- the Entropy is defined as: sum (p_i * log_2 p_i)
-- where p_i = |{ x | x has Label i}|/|Dataset|

newtype Dataset k a = Dataset { unDataset :: M.Map k a } deriving (Eq, Show, Functor)

instance Foldable (Dataset k) where
  foldMap f (Dataset dm) = foldMap f dm

instance Traversable (Dataset k) where
  traverse f (Dataset dm) = Dataset <$> traverse f dm

empty :: Dataset k a
empty = Dataset M.empty

insert :: Ord k => k -> a -> Dataset k a -> Dataset k a
insert k ls (Dataset ds) = Dataset $ M.insert k ls ds

fromList :: Ord k => [(k, a)] -> Dataset k a
fromList ld = Dataset $ M.fromList ld

lookup :: Ord k => k -> Dataset k a -> Maybe a
lookup k (Dataset ds) = M.lookup k ds

-- | Size of the dataset
size :: Foldable t => Dataset k (t a) -> Int
size (Dataset ds) = M.foldl' (\acc l -> acc + length l) 0 ds

-- | Size of the individual classes (Nothing if the class has no datapoints)
sizeClasses :: (Foldable t, Num n) => Dataset k (t a) -> M.Map k n
sizeClasses (Dataset ds) = (fromIntegral . length) <$> ds

probClasses :: (Fractional b, Foldable t) => Dataset k (t a) -> M.Map k b
probClasses ds = (\n -> n / fromIntegral (size ds)) <$> sizeClasses ds

entropy :: (Traversable t, Ord a, Floating a) => t a -> Maybe a
entropy ps = negate . sum <$> traverse entropy1 ps where
  entropy1 :: (Ord a, Floating a) => a -> Maybe a
  entropy1 p | p > 0 = Just ( p * logBase 2 p)
             | otherwise = Nothing



