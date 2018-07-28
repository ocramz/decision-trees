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
-- Each leaf carries data of type 'a' and we can attach metadata of type 'd' at each branching point.
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
  , toMinLeafSize :: !Int  -- ^ Minimum size of the contents of a leaf
  , toOrdering :: Order -- ^ Less than | Equal or larger than
  } 

-- | Tree node metadata
--
-- For decision trees, at each node we store the decision feature and its decision threshold
data TNData a = TNData {
    tJStar :: !Int  -- ^ Decision feature index
  , tTStar :: a  -- ^ Decision threshold
  } deriving (Eq, Show)

-- | For binary decision trees, all the subsets of data that /pass/ the decision are referenced in the /left/ branch, and those that /fail/ the test end up in the /right/ branch.
growTree :: (Foldable f, Functor f, Ord a, Ord k) =>
            f (a, Int)  -- ^ (Data threshold, feature label)
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


{- | Note (OPTIMIZATIONS maxInfoGainSplit)

1. After splitting a dataset, remove the (threshold, feature index) pair corresponding to the succesful split

2. " " " " , remove /all/ (threshold, index) pairs that are subsumed by the successful test (e.g in the test ((<), 3.2, 27) , remove all [(t, 27) | t <- [tmin ..], t < 3.2 ] )
-}



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

-- | Partition a Dataset in two, according to a decision predicate applied to a given feature.
--
-- e.g. "is the j'th component of datum X_i larger than threshold t ?" 
partition :: (Foldable t, Ord k) =>
              (a -> Bool) -- ^ Decision function (element-level)
           -> Int           -- ^ Feature index
           -> Dataset k (t (XV.V a))
           -> (Dataset k [XV.V a], Dataset k [XV.V a])
partition p j ds@Dataset{} = foldrWithKey insf (empty, empty) ds where
  insf k lrow (l, r) = (insert k lp l, insert k rp r) where    
    (lp, rp) = partition1 (XV.dataSplitDecision p j) lrow 

-- | Partition a Foldable in two lists according to a predicate
partition1 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition1 p = foldr ins ([], [])  where
  ins x (l, r) | p x = (x : l, r)
               | otherwise = (l , x : r)




  
fromFoldableIM :: Foldable t => (a -> IM.Key) -> t a -> IM.IntMap a
fromFoldableIM kf x = IM.fromList $ map (left kf) $ F.toList x



-- | A well-defined Ordering, for strict half-plane separation
data Order = LessThan | GreaterOrEqual deriving (Eq, Show, Ord, Enum, Bounded)

fromOrder :: Ord a => Order -> (a -> a -> Bool)
fromOrder o = case o of
  LessThan -> (<)
  _ -> (>=)


-- * General purpose combinators

featureBounds :: (Traversable t, Applicative v, Ord a) => t (v a) -> v (a, a)
featureBounds = featureSummary (minimum &&& maximum)

-- | Each class is represented as a Traversable of Applicatives (e.g. a list of V's).
-- We "transpose" it via sequenceA (thus obtaining a V of lists), and for each entry  
-- of the V we compute a summary function.
-- This could be used e.g. to compute unique values for each of the features, or various statistical moments for the purpose of summarization and/or standardization. 
featureSummary :: (Traversable t, Applicative v) => (t a -> b) -> t (v a) -> v b
featureSummary f = fmap f . sequenceA

-- * Feature binning logic

-- | Decide an Enum bin based on a lower bound and a bin width
--
-- To be used as a quantization function
bin :: (Ord a, Num a, Enum i) =>
       a  -- ^ Lower bound
    -> a  -- ^ Bin width
    -> a  -- ^ Test value
    -> i
bin = gbin (<) (+)    

gbin :: Enum i => (a -> b -> Bool) -> (b -> c -> b) -> b -> c -> a -> i
gbin fdecide fstep xmin dx x = go (toEnum 0) xmin where
  go i xthr | fdecide x xthr = i
            | otherwise = go (succ i) (fstep xthr dx)


-- * Little abstract friends

left :: (b -> c) -> b -> (c, b)
left f = f &&& id

both :: Bifunctor p => (a -> d) -> p a a -> p d d
both f = bimap f f









