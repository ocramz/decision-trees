{-# language DeriveFunctor, DeriveFoldable #-}
module Data.Histogram where

import qualified Data.Foldable as F
import Data.Monoid (Sum(..))
import qualified Data.IntMap as IM
import Control.Arrow ((&&&))

-- | x `msub` x == mempty
class Monoid a => Group a where
  msub :: a -> a -> a
instance Num a => Group (Sum a) where
  (Sum a) `msub` (Sum b) = Sum (a - b)

-- | Histogram 

data Count a = Count { getCount :: !(Sum Int), getCountItems :: a } deriving (Eq, Show, Functor, Foldable)
instance Semigroup a => Semigroup (Count a) where
  (Count n xs) <> (Count m ys) = Count (n <> m) (xs <> ys)
instance Monoid a => Monoid (Count a) where
  mempty = Count (Sum 0) mempty
instance Group a => Group (Count a) where
  (Count n xs) `msub` (Count m ys) = Count (n `msub` m) (xs `msub` ys)

mkCount1 :: a -> Count [a]
mkCount1 x = Count 1 [x]

-- | Populate a histogram 
fillHistogram :: Foldable t => (a -> IM.Key) -> t a -> IM.IntMap (Count [a])
fillHistogram kf xs =  
  IM.fromListWith (<>) $ map (kf &&& mkCount1) $ F.toList xs

--  Some binning/quantization function
quantize :: (Ord a, Num a) => a -> IM.Key
quantize x | x < 0 = 0
           | x >= 0 && x < 5 = 1
           | otherwise = 2

getNElems :: (Foldable t, Num n) => t (Count a) -> n
getNElems = fromIntegral . getSum . foldMap getCount

-- | Compute a distribution from a Histogram 
mkDistrib :: (Functor t, Fractional b, Foldable t) => t (Count a) -> t b
mkDistrib xs = fmap ((/ n) . fromIntegral . getSum . getCount) xs where
  n = getNElems xs

getBins :: Functor t => t (Count a) -> t a 
getBins = fmap getCountItems 

-- 

-- newtype Histogram f a = Histogram {
--   getHistogram :: IM.IntMap (Count (f a))
--   } deriving (Eq, Show, Functor)
