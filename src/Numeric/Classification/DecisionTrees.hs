{-# language DeriveFunctor #-}
module Numeric.Classification.DecisionTrees where

import Control.Arrow ((&&&))
import Prelude hiding (lookup)
import qualified Prelude as P (filter)

import qualified Data.Foldable as F
import Data.Bifunctor (Bifunctor(..), first, second)

import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

import Data.Function (on)
import Data.Ord (comparing)


-- | Map representation

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

-- | Size of the individual classes 
sizeClasses :: (Foldable t, Num n) => Dataset k (t a) -> M.Map k n
sizeClasses (Dataset ds) = (fromIntegral . length) <$> ds


-- | Entropy of a Dataset
--
-- Entropy is defined as: sum (p_i * log_2 p_i)
-- where p_i = |{ x | x has Label i}|/|Dataset|
--
-- NB: returns Nothing if any class contains 0 points (i.e. if the integration support contains any 0-measure subsets)
entropy :: (Foldable t, Ord h, Floating h) => Dataset k (t a) -> Maybe h
entropy = entropy_ . probClasses

probClasses :: (Fractional b, Foldable t) => Dataset k (t a) -> M.Map k b
probClasses ds = (\n -> n / fromIntegral (size ds)) <$> sizeClasses ds

entropy_ :: (Traversable t, Ord a, Floating a) => t a -> Maybe a
entropy_ ps = negate . sum <$> traverse entropyD ps where
  entropyD :: (Ord a, Floating a) => a -> Maybe a
  entropyD p | p > 0 = Just ( p * logBase 2 p)
             | otherwise = Nothing

-- | Differential entropy has a singularity at 0 but converges ~ linearly to 0 for small positive values.
entropyR :: (Foldable t, Ord h, Floating h) => Dataset k (t a) -> h
entropyR = entropyR_ . probClasses

entropyR_ :: (Foldable t, Functor t, Ord c, Floating c) => t c -> c
entropyR_ ps = negate . sum $ entropyReg <$> ps where
  entropyReg p | p > 0 =  p * logBase 2 p
               | otherwise = 0




-- | Split decision: find feature value that maximizes the entropy drop (i.e the information gain, or KL divergence)

-- Dataset k (t a) -> (a -> Bool)


tabulateSplits :: (Ord h, Floating h) =>
                  (a -> IM.Key)
               -> (a -> a -> Bool)  -- ^ Decision function (first argument is the decision boundary)
               -> Dataset k [a]
               -> IM.IntMap (Maybe (a, h))
tabulateSplits kf decision ds = IM.map mf (uniques kf ds) where
  mf x = do
    -- let decx = decision x
    ig <- infoGain (decision x) ds
    pure (x, ig)
    -- ig <- infoGain decx ds
    -- pure (decx, ig)        -- returns the decision function itself (decx)

-- | Information gain due to a dataset split (regularized, H(0) := 0)
infoGainR :: (Ord h, Floating h) => (a -> Bool) -> Dataset k [a] -> h
infoGainR p ds = h0 - (pl * hl + pr * hr) where
    sz = fromIntegral . size 
    (dsl, dsr) = partition p ds
    (s0, sl, sr) = (sz ds, sz dsl, sz dsr)
    (h0, hl, hr) = (entropyR ds, entropyR dsl, entropyR dsr)    
    pl = sl / s0
    pr = sr / s0
    
-- | Information gain due to a dataset split
infoGain :: (Ord h, Floating h) => (a -> Bool) -> Dataset k [a] -> Maybe h
infoGain p ds = do
  h0 <- entropy ds
  hl <- entropy dsl
  hr <- entropy dsr
  pure $ h0 - (pl * hl + pr * hr)
  where
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
