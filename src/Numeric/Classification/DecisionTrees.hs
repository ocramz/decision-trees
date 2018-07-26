{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Numeric.Classification.DecisionTrees where

import Control.Arrow ((&&&))
import Data.Maybe (isJust)

import qualified Data.Foldable as F
import Data.Bifunctor (Bifunctor(..), first, second)

import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

import Data.Function (on)
import Data.Ord (comparing)

import Control.Monad.Catch (MonadThrow(..))
import Control.Exception (Exception(..))
-- import Data.Functor.Compose
import Numeric.Classification.Internal.Datum (X)
import qualified Numeric.Classification.Internal.Datum as X

import Numeric.Classification.Exceptions


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

mapWithKey :: (k -> a -> b) -> Dataset k a -> Dataset k b
mapWithKey f (Dataset ds) = Dataset $ M.mapWithKey f ds

foldrWithKey :: (k -> a -> b -> b) -> b -> Dataset k a -> b
foldrWithKey f z (Dataset ds) = M.foldrWithKey f z ds

foldlWithKey' :: (a -> k -> b -> a) -> a -> Dataset k b -> a
foldlWithKey' f z (Dataset ds) = M.foldlWithKey' f z ds

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
probClasses :: (Fractional prob, Foldable t) => Dataset k (t a) -> M.Map k prob
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

-- | Gini index (expected error rate)
gini :: (Foldable t, Floating c) => Dataset k (t a) -> c
gini = gini_ . probClasses

gini_ :: (Foldable t, Functor t, Floating c) => t c -> c
gini_ ps = 1 - sum ((**2) <$> ps)


-- | A "tagged tree"; at each branching point we can attach decision information and other metadata
data TTree c a = Leaf c a
  | Branch c (TTree c a) (TTree c a) deriving (Eq, Show)

train ds = undefined
  where
    




-- | Split decision: find feature value that maximizes the entropy drop (i.e the information gain, or KL divergence between the joint and factored datasets)

-- Dataset k (t a) -> (a -> Bool)




-- | Tabulate the information gain for a number of decision thresholds and return a decision function corresponding to the threshold that yields the maximum information gain.
--
-- The decision thresholds can be obtained with 'uniques' or 'uniquesEnum'
--
-- tjs = [(t, j) | t <- ts, j <- js]
--
-- * Usage : 
-- 
-- optimalSplitDataset decision tjs ds = splitDatasetAtAttr (decision tstar) jstar ds where
--   (tstar, jstar) = maxInfoGainSplit tjs decision ds
--
    
-- maxInfoGainSplit :: (Foldable f, Functor f, Ord j, Ord k) =>
--                     f (t, j)          -- ^ (Decision thresholds, feature indices)
--                  -> (t -> a -> Bool)  -- ^ Comparison function
--                  -> Dataset k [X j a]
--                  -> (t, j)            -- ^ Optimal dataset splitting (threshold, feature index)
maxInfoGainSplit tjs decision ds = (tstar, jstar, dsl, dsr) where
  (tstar, jstar, _, dsl, dsr) = F.maximumBy (comparing third5) $ infog <$> tjs
  infog (t, j) = (t, j, ig, dsl, dsr) where
    (ig, dsl, dsr) = infoGainR (decision t) j ds
--   infog (t, j) = (t, j, infoGainR (decision t) j ds)

third5 :: (a, b, c, d, e) -> c
third5 (_, _, c, _, _) = c


-- data DatasetSplit k j h a = DS {
--       -- dsDecision :: Compare a
--     dsTStar :: a
--   , dsJStar :: j
--   , dsInfoGain :: h
--   , dsPos :: Dataset k [X j a]
--   , dsNeg :: Dataset k [X j a]} 
  

-- | Information gain due to a dataset split (regularized, H(0) := 0)
infoGainR :: (Ord j, Ord k, Ord h, Floating h) =>
             (a -> Bool)
          -> j
          -> Dataset k [X j a]
          -> (h, Dataset k [X j a], Dataset k [X j a])
infoGainR p j ds = (informationGain, dsl, dsr)  where
    (dsl, pl, dsr, pr) = splitDatasetAtAttr p j ds
    (h0, hl, hr) = (entropyR ds, entropyR dsl, entropyR dsr)
    informationGain = h0 - (pl * hl + pr * hr)


-- | helper function for 'infoGain' and 'infoGainR'
splitDatasetAtAttr :: (Fractional n, Ord j, Ord k) =>
                      (a -> Bool)
                   -> j
                   -> Dataset k [X j a]
                   -> (Dataset k [X j a], n, Dataset k [X j a], n)  
splitDatasetAtAttr p j ds = (dsl, pl, dsr, pr) where
  sz = fromIntegral . size 
  (dsl, dsr) = partition p j ds
  (s0, sl, sr) = (sz ds, sz dsl, sz dsr)
  pl = sl / s0
  pr = sr / s0 

-- | Partition a Dataset in two, according to a decision function
--
-- e.g. "is the j'th component of datum X_i larger than threshold t ?" 
partition :: (Foldable t, Ord k, Ord j) =>
              (a -> Bool) -- ^ Decision function (element-level)
           -> j           -- ^ Feature index
           -> Dataset k (t (X j a))
           -> (Dataset k [X j a], Dataset k [X j a])
partition p j ds@Dataset{} = foldrWithKey insf (empty, empty) ds where
  insf k lrow (l, r) = (insert k lp l, insert k rp r) where    
    (lp, rp) = partitionX p j lrow

-- | Partition a Foldable of data [X ..] according to the values taken by their j'th feature
partitionX :: (Foldable t, Ord j) =>
               (a -> Bool)  -- ^ Decision function (element-level)
            -> j   -- ^ Feature index
            -> t (X j a)   -- ^ Data
            -> ([X j a], [X j a]) -- ^ Positive decision in the left bucket, negative in the right
partitionX p j = partition1 (X.dataSplitDecision p j)

partition1 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition1 p = foldr ins ([], [])  where
  ins x (l, r) | p x = (x : l, r)
               | otherwise = (l , x : r)



-- * Unique dataset entries

uniquesEnum :: (Foldable t, Enum a) => Dataset k (t a) -> IM.IntMap a
uniquesEnum = uniques fromEnum

uniques :: Foldable t => (a -> IM.Key) -> Dataset k (t a) -> IM.IntMap a
uniques kf (Dataset ds) = M.foldr insf IM.empty ds where
  insf ll acc = fromFoldableIM ll `IM.union` acc
  fromFoldableIM x = IM.fromList $ map (left kf) $ F.toList x

-- * Little abstract friends

-- insideOut2 :: (Maybe a, Maybe b) -> Maybe (a, b)
-- insideOut2 mm = case mm of
--   (Just a, Just b) -> Just (a, b)
--   _ -> Nothing

left :: (b -> c) -> b -> (c, b)
left f = f &&& id

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f

third3 :: (a, b, c) -> c
third3 (_, _, c) = c








