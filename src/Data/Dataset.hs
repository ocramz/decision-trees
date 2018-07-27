{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.Dataset where

import qualified Data.Map.Strict as M

-- | Labeled dataset represented as a 'Map'. The map keys are the class labels
newtype Dataset k a = Dataset { unDataset :: M.Map k a } deriving (Eq, Show, Functor, Foldable, Traversable)

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