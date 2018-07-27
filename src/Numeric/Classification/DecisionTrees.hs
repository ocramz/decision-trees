{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- {-# language RecordWildCards #-}
module Numeric.Classification.DecisionTrees where

import Control.Arrow ((&&&))

import qualified Data.Foldable as F
import Data.Bifunctor (Bifunctor(..))

import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM

-- import Data.Function (on)
import Data.Ord (comparing)

import Control.Monad.Catch (MonadThrow(..))
-- import Control.Exception (Exception(..))
-- import Data.Functor.Compose

import Data.Dataset
import Numeric.InformationTheory
import Numeric.Classification.Internal.Datum (X)

-- import qualified Numeric.Classification.Internal.Datum as X
import qualified Numeric.Classification.Internal.Datum.Vector as XV

import Numeric.Classification.Exceptions


-- | A binary tree.
--
-- We can attach metadata at branching point.
data Tree d a =
    Node d (Tree d a) (Tree d a)
  | Leaf a
  deriving (Eq, Show, Functor, Foldable, Traversable)

unfoldTree :: (t -> Either a (d, t, t)) -> t -> Tree d a
unfoldTree f x =
  either Leaf (\(d, l, r) -> Node d (unfoldTree f l) (unfoldTree f r) ) (f x)


-- | Dataset + local tree depth
data TDs a = TDs { tdsDepth :: !Int, tds :: a } deriving (Eq, Show)

-- | Tree growing global options
data TOptions = TOptions {
    toMaxDepth :: !Int  -- ^ Max tree depth
  , toMinLeafSize :: !Int
  , toOrdering :: Order -- ^ (<) or (>=)
  } 

-- | Tree node metadata
data TNData a = TNData {
    tJStar :: !Int
  , tTStar :: a } deriving (Eq, Show)

growTree :: (Foldable f, Functor f, Ord a, Ord k) =>
            f (a, Int)
         -> TOptions
         -> Dataset k [XV.V a]
         -> Tree (TNData a) (Dataset k [XV.V a])
growTree tjs opts ds = unfoldTree (treeRecurs tjs opts) tds0 where
  tds0 = TDs 0 ds

-- | Split decision: find feature (value, index) that maximizes the entropy drop (i.e the information gain, or KL divergence between the joint and factored datasets)
treeRecurs :: (Ord a, Ord k, Foldable t, Functor t) =>
              t (a, Int)
           -> TOptions
           -> TDs (Dataset k [XV.V a])
           -> Either
           (Dataset k [XV.V a])
           (TNData a, TDs (Dataset k [XV.V a]), TDs (Dataset k [XV.V a]))
treeRecurs tjList (TOptions maxdepth minls ord) (TDs depth ds)
  | q1 || q2 = Left ds
  | otherwise = Right (mdata, tdsl, tdsr)
  where
    szq d = size d <= minls
    q1 = depth >= maxdepth
    q2 = szq dsl || szq dsr
    mdata = TNData jstar tstar
    (tstar, jstar, dsl, dsr) = maxInfoGainSplit tjList ordf ds
    ordf = fromOrder ord
    d' = depth + 1
    tdsl = TDs d' dsl
    tdsr = TDs d' dsr


-- | Tabulate the information gain for a number of decision thresholds and return a decision function corresponding to the threshold that yields the maximum information gain.
--
-- The decision thresholds can be obtained with 'uniques' or 'uniquesEnum'  
maxInfoGainSplit :: (Foldable f, Functor f, Ord k) =>
                    f (t, Int)          -- ^ (Decision thresholds, feature indices)
                 -> (t -> a -> Bool)  -- ^ Comparison function
                 -> Dataset k [XV.V a]
                 -> (t, Int, Dataset k [XV.V a], Dataset k [XV.V a]) -- ^ Optimal dataset splitting (threshold, feature index), positive subset of the data, negative subset
maxInfoGainSplit tjs decision ds = (tstar, jstar, dsl, dsr) where
  (tstar, jstar, _, dsl, dsr) = F.maximumBy (comparing third5) $ infog <$> tjs
  infog (t, j) = (t, j, h, dsl, dsr) where
    (h, dsl, dsr) = infoGainR (decision t) j ds

third5 :: (a, b, c, d, e) -> c
third5 (_, _, c, _, _) = c

-- | Information gain due to a dataset split (regularized, H(0) := 0)
infoGainR :: (Ord k, Ord h, Floating h) =>
             (a -> Bool)
          -> Int
          -> Dataset k [XV.V a]
          -> (h, Dataset k [XV.V a], Dataset k [XV.V a])
infoGainR p j ds = (infoGain, dsl, dsr) where
    (dsl, pl, dsr, pr) = splitDatasetAtAttr p j ds
    (h0, hl, hr) = (entropyR ds, entropyR dsl, entropyR dsr)
    infoGain = h0 - (pl * hl + pr * hr)


-- | helper function for 'infoGain' and 'infoGainR'
splitDatasetAtAttr :: (Fractional n, Ord k) =>
                      (a -> Bool)
                   -> Int
                   -> Dataset k [XV.V a]
                   -> (Dataset k [XV.V a], n, Dataset k [XV.V a], n)  
splitDatasetAtAttr p j ds = (dsl, pl, dsr, pr) where
  sz = fromIntegral . size 
  (dsl, dsr) = partition p j ds
  (s0, sl, sr) = (sz ds, sz dsl, sz dsr)
  pl = sl / s0
  pr = sr / s0 

-- | Partition a Dataset in two, according to a decision function
--
-- e.g. "is the j'th component of datum X_i larger than threshold t ?" 
partition :: (Foldable t, Ord k) =>
              (a -> Bool) -- ^ Decision function (element-level)
           -> Int           -- ^ Feature index
           -> Dataset k (t (XV.V a))
           -> (Dataset k [XV.V a], Dataset k [XV.V a])
partition p j ds@Dataset{} = foldrWithKey insf (empty, empty) ds where
  insf k lrow (l, r) = (insert k lp l, insert k rp r) where    
    (lp, rp) = partitionX p j lrow

-- | Partition a Foldable of data [X ..] according to the values taken by their j'th feature
partitionX :: (Foldable t) =>
               (a -> Bool)  -- ^ Decision function (element-level)
            -> Int   -- ^ Feature index
            -> t (XV.V a)   -- ^ Data
            -> ([XV.V a], [XV.V a]) -- ^ Positive decision in the left bucket, negative in the right
partitionX p j = partition1 (XV.dataSplitDecision p j)

partition1 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition1 p = foldr ins ([], [])  where
  ins x (l, r) | p x = (x : l, r)
               | otherwise = (l , x : r)




-- | tjs := T * J where
-- T := unique data values
-- J := feature indices
--
-- NB : if `a` is a floating point type, the "key function" kf will have to quantize it into a histogram
-- tjs :: (Functor t, Foldable t, Foldable v) =>
--        (a -> IM.Key)  -- ^ Quantization function
--     -> Int   -- ^ Number of features
--     -> Dataset k (t (v a))
--     -> [(a, Int)]
-- tjs kf n ds = [(t, j) | j <- js, t <- ts] where
--   js = [0 .. n-1]
--   ts = map snd . IM.toList $ uniques kf ds


-- * Unique dataset entries

uniquesEnum :: (Functor t, Foldable t, Foldable v, Enum a) =>
           Dataset k (t (v a))
        -> IM.IntMap a
uniquesEnum = uniques fromEnum

uniques :: (Functor t, Foldable t, Foldable v) =>
           (a -> IM.Key)
        -> Dataset k (t (v a))
        -> IM.IntMap a
uniques kf (Dataset ds) = M.foldr insf IM.empty ds where
  insf im acc = uniquesClass kf im `IM.union` acc

uniquesClass :: (Functor t, Foldable t, Foldable v) =>
                (a -> IM.Key)
             -> t (v a)
             -> IM.IntMap a
uniquesClass kf xs = foldr IM.union IM.empty $ fromFoldableIM kf <$> xs

fromFoldableIM :: Foldable t => (a -> IM.Key) -> t a -> IM.IntMap a
fromFoldableIM kf x = IM.fromList $ map (left kf) $ F.toList x



-- | A well-defined Ordering, for strict half-plane separation
data Order = LessThan | GreaterOrEqual deriving (Eq, Show, Ord, Enum, Bounded)

fromOrder :: Ord a => Order -> (a -> a -> Bool)
fromOrder o = case o of
  LessThan -> (<)
  _ -> (>=)



-- * Little abstract friends

-- insideOut2 :: (Maybe a, Maybe b) -> Maybe (a, b)
-- insideOut2 mm = case mm of
--   (Just a, Just b) -> Just (a, b)
--   _ -> Nothing

left :: (b -> c) -> b -> (c, b)
left f = f &&& id

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f









