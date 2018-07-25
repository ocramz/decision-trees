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
import Numeric.Classification.Internal.Datum (X, splitAttrP)
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



-- | A "tagged tree"; at each branching point we can attach decision information, e.g. an Ordering or an equality comparison
data TTree c a = Leaf a
  | Branch c (TTree c a) (TTree c a)

type OrdTree = TTree Ordering

type BoolTree = TTree Bool



-- | Split decision: find feature value that maximizes the entropy drop (i.e the information gain, or KL divergence between the joint and factored datasets)

-- Dataset k (t a) -> (a -> Bool)

-- | Tabulate the information gain for a number of decision thresholds and return a decision function corresponding to the threshold that yields the maximum information gain.
--
-- The decision thresholds can be obtained with 'uniques' or 'uniquesEnum'
maxInfoGainSplit_ :: (Ord j, Ord k, Foldable f, Functor f) =>
                     f t  -- ^ Decision thresholds
                  -> (t -> a -> Bool)  -- ^ Comparison function
                  -> j
                  -> Dataset k [X j a]
                  -> (a -> Bool) -- ^ Dataset splitting decision function 
maxInfoGainSplit_ tvals decision k ds = decision tstar
  where
    (tstar, _) = F.maximumBy (comparing snd) $ mf <$> tvals 
    mf t = (t, infoGainR (decision t) k ds)


-- maxInfoGainSplit' tkvals decision k ds = (tstar, kstar) -- decision tstar
--   where
--     (tstar, kstar, _) = F.maximumBy (comparing third3) $ D.imap mf tkvals 
--     mf k t = (t, k, infoGainR (decision t) k ds)


-- -- asd :: (Floating b, D.Datum d, Ord b, Ord k) =>
-- --      (a -> a -> Bool) -> Dataset k [d a] -> Dataset k [d b]
-- asd decision ds = map (D.imap g) <$> ds where
--   g k t = (k, t, infoGainR (decision t) k ds)

third3 (_, _, c) = c


-- | Information gain due to a dataset split (regularized, H(0) := 0)
infoGainR :: (Ord j, Ord k, Ord h, Floating h) =>
             (a -> Bool)
          -> j
          -> Dataset k [X j a]
          -> h
infoGainR p k ds = h0 - (pl * hl + pr * hr) where
    (dsl, pl, dsr, pr) = splitDatasetAtAttr p k ds
    (h0, hl, hr) = (entropyR ds, entropyR dsl, entropyR dsr)   


-- | helper function for 'infoGain' and 'infoGainR'
splitDatasetAtAttr :: (Fractional n, Ord j, Ord k) =>
                      (a -> Bool)
                   -> j
                   -> Dataset k [X j a]
                   -> (Dataset k [X j a], n, Dataset k [X j a], n)  
splitDatasetAtAttr p k ds = (dsl, pl, dsr, pr) where
  sz = fromIntegral . size 
  (dsl, dsr) = partition (splitAttrP p k) ds
  (s0, sl, sr) = (sz ds, sz dsl, sz dsr)
  pl = sl / s0
  pr = sr / s0 

-- | Partition a Dataset in two, according to a decision function
partition :: (Ord k, Foldable t) =>
             (a -> Bool)  -- ^ Decision function (element-level)
          -> Dataset k (t a)
          -> (Dataset k [a], Dataset k [a])  
partition p ds@Dataset{} = foldrWithKey insf (empty, empty) ds where
  insf k lrow (l, r) = (insert k lp l, insert k rp r) where    
    (lp, rp) = partition1 p lrow

partition1 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition1 p = foldr ins ([], [])  where
  ins x (l, r) | p x = (x : l, r)
               | otherwise = (l , x : r)

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











