{-# language TypeFamilies, DeriveFunctor #-}
module Numeric.Classification.DecisionTrees where

import Control.Arrow ((&&&))
import Prelude hiding (lookup)
import qualified Prelude as P (filter)
import Data.Maybe (isJust)

import qualified Data.Foldable as F
import Data.Bifunctor (Bifunctor(..), first, second)

import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

import Data.Function (on)
import Data.Ord (comparing)

import Data.Typeable
import Control.Monad.Catch (MonadThrow(..))
import Control.Exception (Exception(..))

import qualified Data.Vector.Unboxed as VU


-- | Labeled dataset represented as a 'Map'. The map keys are the class labels
newtype Dataset k a = Dataset { unDataset :: M.Map k a } deriving (Eq, Show, Functor)

instance Foldable (Dataset k) where
  foldMap f (Dataset dm) = foldMap f dm

instance Traversable (Dataset k) where
  traverse f (Dataset dm) = Dataset <$> traverse f dm

empty :: Dataset k a
empty = Dataset M.empty

insert :: Ord k => k -> a -> Dataset k a -> Dataset k a
insert k ls (Dataset ds) = Dataset $ M.insert k ls ds

filterF :: Functor f => (a -> Bool) -> f [a] -> f [a]
filterF p ds = P.filter p <$> ds

fromList :: Ord k => [(k, a)] -> Dataset k a
fromList ld = Dataset $ M.fromList ld

toList :: Dataset k a -> [(k, a)]
toList (Dataset ds) = M.toList ds

-- lookup :: Ord k => k -> Dataset k a -> Maybe a
-- lookup k (Dataset ds) = M.lookup k ds

-- | Size of the dataset
size :: Foldable t => Dataset k (t a) -> Int
size (Dataset ds) = M.foldl' (\acc l -> acc + length l) 0 ds

-- | Number of items in each class
sizeClasses :: (Foldable t, Num n) => Dataset k (t a) -> M.Map k n
sizeClasses (Dataset ds) = (fromIntegral . length) <$> ds

-- | Empirical class probabilities i.e. for each k, number of items in class k / total number of items
probClasses :: (Fractional b, Foldable t) => Dataset k (t a) -> M.Map k b
probClasses ds = (\n -> n / fromIntegral (size ds)) <$> sizeClasses ds

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




-- | Split decision: find feature value that maximizes the entropy drop (i.e the information gain, or KL divergence)

-- Dataset k (t a) -> (a -> Bool)

-- | Tabulate the information gain for a number of decision thresholds and return a decision function corresponding to the threshold that yields the maximum information gain.
--
-- The decision thresholds can be obtained from the 'uniques' function
maxInfoGainSplit :: (Foldable f, Functor f) =>
                 f t -- ^ Decision thresholds
              -> (t -> a -> Bool) -- ^ Comparison function
              -> Dataset k [a]
              -> (a -> Bool)  -- ^ Decision function
maxInfoGainSplit vals decision ds = decision thr
  where
    (thr, _) = F.maximumBy (comparing snd) $ tabulateSplitsR vals decision ds

-- bisect p mi ma x0 = undefined where
--   (pmi, pma) = (p mi, p ma)


tabulateSplitsR :: (Functor f, Ord ig, Floating ig) =>
                   f t  -- ^ Decision thresholds 
                -> (t -> a -> Bool) -- ^ Decision function
                -> Dataset k [a] 
                -> f (t, ig)  -- ^ (Threshold, information gain)
tabulateSplitsR vals decision ds = mf <$> vals where
  mf x = (x, infoGainR (decision x) ds)


-- | Information gain due to a dataset split (regularized, H(0) := 0)
infoGainR :: (Ord h, Floating h) => (a -> Bool) -> Dataset k [a] -> h
infoGainR p ds = h0 - (pl * hl + pr * hr) where
    (dsl, pl, dsr, pr) = splitDataset p ds
    (h0, hl, hr) = (entropyR ds, entropyR dsl, entropyR dsr)    

-- -- | Information gain due to a dataset split
-- infoGain :: (Ord h, Floating h) => (a -> Bool) -> Dataset k [a] -> Maybe h
-- infoGain p ds = do
--   h0 <- entropy ds
--   hl <- entropy dsl
--   hr <- entropy dsr
--   pure $ h0 - (pl * hl + pr * hr)
--   where
--     (dsl, pl, dsr, pr) = splitDataset p ds

class Datum d where
  type DKey d :: *
  type DData d :: *
  lookupAttribute :: d -> DKey d -> Maybe (DData d)

(!?) :: Datum d => d -> DKey d -> Maybe (DData d)
(!?) = lookupAttribute  

newtype DenseD a = DenseD { unDenseD :: VU.Vector a } deriving (Eq, Show)

fromListD :: VU.Unbox a => [a] -> DenseD a
fromListD ll = DenseD $ VU.fromList ll

instance VU.Unbox a => Datum (DenseD a) where
  type DKey (DenseD a) = Int
  type DData (DenseD a) = a
  lookupAttribute (DenseD v) i = v VU.!? i

newtype SparseD k a = SparseD { unSparseD :: M.Map k a } deriving (Eq, Show)

fromListS :: Ord k => [(k, a)] -> SparseD k a
fromListS ll = SparseD $ M.fromList ll

instance Ord k => Datum (SparseD k a) where
  type DKey (SparseD k a) = k 
  type DData (SparseD k a) = a
  lookupAttribute (SparseD mm) i = M.lookup i mm

-- | Return a 'Datum' decision function according to a Boolean function of one of its attributes
splitAttrP :: Datum d => (DData d -> Bool) -> DKey d -> (d -> Bool)
splitAttrP p k dat = maybe False p (dat !? k)
  
  
  
  

-- | helper function for 'infoGain' and 'infoGainR'
splitDataset :: Fractional d =>
                    (a -> Bool)
                 -> Dataset k [a]
                 -> (Dataset k [a], d, Dataset k [a], d)
splitDataset p ds = (dsl, pl, dsr, pr) where
  sz = fromIntegral . size 
  (dsl, dsr) = partition p ds
  (s0, sl, sr) = (sz ds, sz dsl, sz dsr)
  pl = sl / s0
  pr = sr / s0

partition :: Functor f => (a -> Bool) -> f [a] -> (f [a], f [a])
partition p = filterF p &&& filterF (not . p)

uniques :: Foldable t => (a -> IM.Key) -> Dataset k (t a) -> IM.IntMap a
uniques kf (Dataset ds) = M.foldr insf IM.empty ds where
  insf ll acc = fromFoldableIM kf ll `IM.union` acc

fromFoldableIM :: Foldable t => (a -> IM.Key) -> t a -> IM.IntMap a
fromFoldableIM kf x = IM.fromList $ map (left kf) $ F.toList x

-- * Little abstract friends

-- insideOut2 :: (Maybe a, Maybe b) -> Maybe (a, b)
-- insideOut2 mm = case mm of
--   (Just a, Just b) -> Just (a, b)
--   _ -> Nothing

left :: (b -> c) -> b -> (c, b)
left f = f &&& id

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f




-- * Exceptions

data ValueException = ZeroProbabilityE String deriving (Eq, Show, Typeable)

instance Exception ValueException 






