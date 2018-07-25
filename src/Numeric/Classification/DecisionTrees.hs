{-# language DeriveFunctor #-}
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

import qualified Numeric.Classification.Internal.Datum as D



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
maxInfoGainSplit_ :: (D.Datum d, Foldable f, Functor f) =>
                     f t  -- ^ Decision thresholds
                  -> (t -> D.Attr d -> Bool)  -- ^ Comparison function
                  -> D.Key d
                  -> Dataset k [d]
                  -> (D.Attr d -> Bool) -- ^ Dataset splitting decision function 
maxInfoGainSplit_ vals decision k ds = decision thr
  where
    (thr, _) = F.maximumBy (comparing snd) $ mf <$> vals 
    mf x = (x, infoGainR (decision x) k ds)



-- | Information gain due to a dataset split (regularized, H(0) := 0)
infoGainR :: (D.Datum d, Ord h, Floating h) =>
              (D.Attr d -> Bool)
           -> D.Key d
           -> Dataset k [d] -> h
infoGainR p k ds = h0 - (pl * hl + pr * hr) where
    (dsl, pl, dsr, pr) = splitDatasetAtAttr p k ds
    (h0, hl, hr) = (entropyR ds, entropyR dsl, entropyR dsr)   


-- | helper function for 'infoGain' and 'infoGainR'
splitDatasetAtAttr :: (Fractional a, D.Datum d) =>
                      (D.Attr d -> Bool)
                   -> D.Key d
                   -> Dataset k [d]
                   -> (Dataset k [d], a, Dataset k [d], a)  
splitDatasetAtAttr pa k ds = (dsl, pl, dsr, pr) where
  p = D.splitAttrP pa k
  sz = fromIntegral . size 
  (dsl, dsr) = partition p ds
  (s0, sl, sr) = (sz ds, sz dsl, sz dsr)
  pl = sl / s0
  pr = sr / s0 
  

partition :: Functor f => (a -> Bool) -> f [a] -> (f [a], f [a])
partition p = filterF p &&& filterF (not . p)

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




-- * Exceptions

data ValueException = ZeroProbabilityE String deriving (Eq, Show, Typeable)

instance Exception ValueException 






