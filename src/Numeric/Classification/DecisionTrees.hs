{-# language DeriveFunctor #-}
module Numeric.Classification.DecisionTrees where

import Control.Arrow ((***), (&&&))
import Prelude hiding (lookup)
import qualified Prelude as P (filter)

import qualified Data.Foldable as F
import Data.Bifunctor (Bifunctor(..), first, second)

import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

import Data.Function (on)
import Data.Ord (comparing)

-- -- | Nested map representation

-- newtype D k a = D {unD :: M.Map k (IM.IntMap a) } deriving (Eq, Show, Functor)

-- emptyD :: D k a
-- emptyD = D M.empty

-- -- insertLabeled :: Ord k => k -> [a] -> D k a -> D k a
-- -- insertLabeled k x (D dm) = D $ M.insert k xl dm where
-- --   xl = indexed x

-- fromListD :: Ord k => [(k, [a])] -> D k a
-- fromListD lx = D $ indexed <$> M.fromList lx

-- unionD :: D k a -> IM.IntMap a
-- unionD = foldrD IM.union IM.empty

-- foldrD :: (IM.IntMap a -> b -> b) -> b -> D k a -> b
-- foldrD ffold z (D mm) = foldr ffold z mm

-- indexed :: [a] -> IM.IntMap a
-- indexed ll = IM.fromList $ zip [0..] ll

-- lookupD :: Ord k => k -> IM.Key -> D k b -> Maybe b
-- lookupD ki kj (D mm) = M.lookup ki mm >>= IM.lookup kj



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

lookup :: Ord k => k -> Dataset k a -> Maybe a
lookup k (Dataset ds) = M.lookup k ds

-- | Size of the dataset
size :: Foldable t => Dataset k (t a) -> Int
size (Dataset ds) = M.foldl' (\acc l -> acc + length l) 0 ds

-- | Size of the individual classes (Nothing if the class has no datapoints)
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



-- | Split decision: find feature value that maximizes the entropy drop (i.e the information gain, or KL divergence)

-- Dataset k (t a) -> (a -> Bool)

tabulateSplits :: (Floating h, Ord h, Ord a) =>
                  (a -> IM.Key)
               -> Dataset k [a]
               -> IM.IntMap (Maybe (a, h, h))
tabulateSplits kf ds = IM.map mf (uniques kf ds) where
  mf x = do
    (h1, h2) <- splitEntropies (> x) ds
    pure (x, h1, h2)

splitEntropies ::
  (Ord h, Floating h) =>
     (a -> Bool)       -- ^ Splitting function
  -> Dataset k [a]
  -> Maybe (h, h)
splitEntropies p ds = insideOut2 $ both entropy (partition p ds)

partition :: Functor f => (a -> Bool) -> f [a] -> (f [a], f [a])
partition p = filterF p &&& filterF (not . p)

uniques :: Foldable t => (a -> IM.Key) -> Dataset k (t a) -> IM.IntMap a
uniques kf (Dataset ds) = M.foldr insf IM.empty ds where
  insf ll acc = fromFoldableIM kf ll `IM.union` acc

fromFoldableIM :: Foldable t => (a -> IM.Key) -> t a -> IM.IntMap a
fromFoldableIM kf x = IM.fromList $ map (left kf) $ F.toList x

-- * Little abstract friends

insideOut2 :: (Maybe a, Maybe b) -> Maybe (a, b)
insideOut2 mm = case mm of
  (Just a, Just b) -> Just (a, b)
  _ -> Nothing

left :: (b -> c) -> b -> (c, b)
left f = f &&& id

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f
