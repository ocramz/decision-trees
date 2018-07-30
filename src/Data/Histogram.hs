{-# language DeriveFunctor, DeriveFoldable #-}
module Data.Histogram (Histogram , mkHistogram, getHistogram) where

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


mkHistogram :: (Ord k, Foldable t) => (a -> k) -> t a -> Histogram k a
mkHistogram kf = addToHistogram (empty kf)   

getHistogram :: Histogram k a -> M.Map k Int
getHistogram h = getSum . getCount <$> unHistogram h

getNElems :: (Foldable t, Num n) => t (Count a) -> n
getNElems = fromIntegral . getSum . foldMap getCount

-- | Compute a distribution from a Histogram 
mkDistrib :: (Functor t, Foldable t, Fractional p) => t (Count a) -> t p
mkDistrib xs = fmap ((/ n) . fromIntegral . getSum . getCount) xs where
  n = getNElems xs

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






-- TODO probably we can split this in two stages and compute the entropy only from a normalized histogram 
entropy :: Floating h => Histogram k a -> h
entropy (Histogram _ mm) = sum plp where
  plp = fmap (\p -> p * logBase 2 p) ps
  ps = fmap ((/ n) . fromIntegral . getSum . getCount) mm 
  n = getNElems mm



-- | Count monoid
data Count a = Count { getCount :: !(Sum Int), getCountItems :: a } deriving (Eq, Show, Functor, Foldable)
instance Semigroup a => Semigroup (Count a) where
  (Count n xs) <> (Count m ys) = Count (n <> m) (xs <> ys)
instance Monoid a => Monoid (Count a) where
  mempty = Count (Sum 0) mempty

mkCount1 :: a -> Count [a]
mkCount1 x = Count 1 [x]
