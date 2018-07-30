{-# language DeriveFunctor, DeriveFoldable #-}
module Data.Histogram (Histogram , mkHistogram, getHistogram, normalize, entropy) where

import qualified Data.Foldable as F
import Data.Monoid (Sum(..))
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import Control.Arrow ((&&&))


data Histogram k a = Histogram {
    binFunction :: a -> k
  , unHistogram :: M.Map k (Count [a]) }

instance (Eq k, Eq a) => Eq (Histogram k a) where
  h1 == h2 = unHistogram h1 == unHistogram h2

instance (Show k, Show a) => Show (Histogram k a) where
  show h = show (unHistogram h)

-- | Populate a Histogram given a quantization function and a Foldable of data
mkHistogram :: (Ord k, Foldable t) => (a -> k) -> t a -> Histogram k a
mkHistogram kf = addToHistogram (empty kf)   

-- | Bin counts for a Histogram
getHistogram :: Histogram k a -> M.Map k Int
getHistogram h = getSum . getCount <$> unHistogram h

getNElems :: Histogram k a -> Int
getNElems = sum . getHistogram 

-- | Compute a distribution from a Histogram
normalize :: Fractional p => Histogram k a -> M.Map k p
normalize hh = f <$> ns
  where
  ns = fromIntegral <$> getHistogram hh
  n = fromIntegral $ getNElems hh
  f x = x / n

entropy :: Floating h => Histogram k a -> h
entropy h = sum $ fmap (\p -> p * logBase 2 p) ps
  where
    ps = normalize h

getBins :: Functor t => t (Count a) -> t a 
getBins = fmap getCountItems 

-- * A packaged Histogram type

-- | To construct a histogram we only need a quantization function (i.e. that decides in which bin does an element fall into)
empty :: (a -> k) -> Histogram k a
empty kf = Histogram kf M.empty

addToHistogram :: (Ord k, Foldable t) => Histogram k a -> t a -> Histogram k a
addToHistogram h0 xs = Histogram kf (M.union hm0 hm1) where
  (Histogram kf hm0) = h0
  hm1 = M.fromListWith (<>) $ map (kf &&& mkCount1) $ F.toList xs





-- | Count monoid
data Count a = Count { getCount :: !(Sum Int), getCountItems :: a } deriving (Eq, Show, Functor, Foldable)
instance Semigroup a => Semigroup (Count a) where
  (Count n xs) <> (Count m ys) = Count (n <> m) (xs <> ys)
instance Monoid a => Monoid (Count a) where
  mempty = Count (Sum 0) mempty

mkCount1 :: a -> Count [a]
mkCount1 x = Count 1 [x]
