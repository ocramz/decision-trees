{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language TypeOperators #-}
module Data.Dataset where

import qualified Data.Foldable as F (maximumBy, foldl', toList)
import Data.Ord (comparing)

import qualified Data.Map.Strict as M (Map(..), empty, fromList, toList, fromListWith, mapWithKey, foldl', foldrWithKey, foldlWithKey', insert)
import qualified Data.Map.Internal.Debug as M (showTree)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

import System.Random.MWC
import Control.Monad.Primitive

import Control.Monad
import Data.Maybe (maybeToList)

import Control.Monad.Catch (MonadThrow(..))
import Numeric.Classification.Exceptions

-- | Labeled dataset represented as a 'Map'. The map keys are the class labels
newtype Dataset k a = Dataset { unDataset :: M.Map k a } deriving (Eq, Show, Functor, Foldable, Traversable)

showTree :: (Show k, Show a) => Dataset k a -> String
showTree (Dataset mm) = M.showTree mm

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

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Dataset k a
fromListWith f ld = Dataset $ M.fromListWith f ld

toList :: Dataset k a -> [(k, a)]
toList (Dataset ds) = M.toList ds

-- lookup :: Ord k => k -> Dataset k a -> Maybe a
-- lookup k (Dataset ds) = M.lookup k ds

-- | Size of the dataset
size :: Foldable t => Dataset k (t a) -> Int
size (Dataset ds) = M.foldl' (\acc l -> acc + length l) 0 ds

-- | Maximum likelihood estimate of class label
mlClass :: Dataset k [a] -> k
mlClass = fst . F.maximumBy (comparing f) . toList where
  f (_, ll) = length ll


-- | Number of items in each class
sizeClasses :: (Foldable t, Num n) => Dataset k (t a) -> M.Map k n
sizeClasses (Dataset ds) = (fromIntegral . length) <$> ds

-- | Empirical class probabilities i.e. for each k, number of items in class k / total number of items
probClasses :: (Fractional prob, Foldable t) => Dataset k (t a) -> M.Map k prob
probClasses ds = (\n -> n / fromIntegral (size ds)) <$> sizeClasses ds






{-
choosing a set S of M unique random samples from a population of size N:

    initialize set S to empty
    for J := N-M + 1 to N do
      T := RandInt(1, J)
      if T is not in S then
        insert T in S
      else
        insert J in S
-}



sampleNoReplace ::
  (MonadThrow m, PrimMonad m, Foldable t) => t (IM.Key, a) -> Int -> Gen (PrimState m) -> m [a]
sampleNoReplace iml nsamples gen
  | nsamples > n = throwM $ DimMismatchE "sampleIM" nsamples n
  | otherwise = do
      ixs <- S.toList <$> sampleUniques n nsamples gen 
      pure $ mconcat . maybeToList $ traverse (`IM.lookup` im) ixs
        where
          im = IM.fromList $ F.toList iml
          n = IM.size im 

-- | Sample without replacement
sampleUniques :: PrimMonad m =>
                 Int   -- ^ Largest number 
              -> Int   -- ^ # of unique numbers to sample
              -> Gen (PrimState m)
              -> m (S.Set Int)  
sampleUniques n nsamples gen = foldM sample1 S.empty [p .. n] where
  p = n - nsamples + 1
  sample1 s j = do
    t <- uniformR (1, j) gen
    let set' =
          if not (S.member t s)
          then
            S.insert t s
          else
            S.insert j s
    return set'

-- stest n ntot = withSystemRandom . asGenIO $ \g -> do
--   let set = S.fromList [0..ntot - 1]
--   sampleUniques set n g




-- * Playground






-- -- takeIth ii xs = go 0 [] xs where
-- --   go _ acc [] = acc 
-- --   go i acc xs
-- --     | i == ii = go (succ i) (head xs : acc) (tail xs)
-- --     | otherwise = go (succ i) acc (tail xs)

-- takeIth ii xs = snd $ F.foldl' insf (0, []) xs where
--   insf (i, acc) x | i == ii = (succ i, x : acc)
--                   | otherwise = (succ i, acc)

-- -- takeIth :: (Show a, Foldable f) => Int -> f a -> IO (Int, [a])
-- -- takeIth ii xs = foldM insf (0, []) xs where
-- --   insf (i, acc) x | i == ii = do
-- --                       let b' = (succ i, x : acc)
-- --                       print b'
-- --                       return b'
-- --                   | otherwise = pure (succ i, acc)   
                  
                  
