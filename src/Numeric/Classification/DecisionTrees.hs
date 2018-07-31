{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- {-# language RecordWildCards #-}
module Numeric.Classification.DecisionTrees where

import Control.Arrow ((&&&))

import qualified Data.Foldable as F
import Data.Bifunctor (Bifunctor(..))

import qualified Data.Map.Strict as M (Map(..))
import qualified Data.IntMap as IM
import qualified Data.Set as S

-- import Data.Function (on)
import Data.Ord (comparing)

import Control.Monad.Catch (MonadThrow(..))
-- import Control.Exception (Exception(..))
-- import Data.Functor.Compose

import Data.Dataset
import Numeric.InformationTheory (entropyR, gini)
import Numeric.Classification.Internal.Datum (X)
-- import qualified Numeric.Classification.Internal.Datum as X
import qualified Numeric.Classification.Internal.Datum.Vector as XV
import Numeric.Classification.Exceptions

import Text.Printf (PrintfArg(..), printf)


-- sample ran s gen = do
--   i <- uniformR ran gen



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

-- * Draw a decision tree 
drawDecisionTree :: (Show a, PrintfArg a2) =>
                    XV.FeatureLabels
                 -> TOptions
                 -> Tree (TNData a2) a
                 -> String
drawDecisionTree fls opts = drawTree (nodeLabel fls opts)

drawTree :: Show a => (d -> String) -> Tree d a -> String
drawTree fnode = unlines . draw
  where
    draw t = case t of
      (Leaf a) ->
        "|" : pad "`- " "   " [show a] 
      (Node d tl tr) ->
        "|" : fnode d : pad "+- " "|  " (draw tl) ++ draw tr
    pad f other = zipWith (++) (f : repeat other)

nodeLabel :: PrintfArg a =>
             XV.FeatureLabels
          -> TOptions
          -> TNData a
          -> String
nodeLabel fls (TOptions _ _ ord) (TNData j t) = expr where
  label = XV.lookupFeatureLabelUnsafe j fls
  expr = unwords [printf "%.2f" t, show ord, label]



-- | Tree state : list of candidate dataset cuts and dataset
data TState k a  = TState {
    tsFeatCuts :: S.Set (Int, a)
  , tsDataset :: Dataset k [XV.V a] } 

-- | Tree state + local tree depth
data TSd k a = TSd { tsDepth :: !Int, tState :: TState k a }

-- | Tree growing global options
data TOptions = TOptions {
    toMaxDepth :: !Int  -- ^ Max tree depth
  , toMinLeafSize :: !Int  -- ^ Minimum size of the contents of a leaf
  , toOrder :: Order -- ^ Less than | Equal or larger than
  } deriving (Eq, Show)

-- | Tree node metadata
--
-- For decision trees, at each node we store the decision feature and its decision threshold
data TNData a = TNData {
    tJStar :: !Int  -- ^ Decision feature index
  , tTStar :: a  -- ^ Decision threshold
  } deriving (Eq)
instance Show a => Show (TNData a) where
  show (TNData j t) = unwords ["(j =", show j, ", t =", show t, ")"]


-- | For binary decision trees, all the subsets of data that /pass/ the decision are referenced in the /left/ branch, and those that /fail/ the test end up in the /right/ branch.
growTree :: (Ord a, Ord k) =>
            TOptions
         -> S.Set (Int, a)   -- ^ (Data threshold, feature label)
         -> Dataset k [XV.V a]
         -> Tree (TNData a) (Dataset k [XV.V a])
growTree opts tjs0 ds = unfoldTree (treeUnfoldStep opts) tds0 where
  tds0 = TSd 0 (TState tjs0 ds)

-- | Split decision: find feature (value, index) that maximizes the entropy drop (i.e the information gain, or KL divergence between the joint and factored datasets)
--
-- NB generates empty leaves
treeUnfoldStep :: (Ord a, Ord k) =>
              TOptions
           -> TSd k a
           -> Either (Dataset k [XV.V a]) (TNData a, TSd k a, TSd k a)
treeUnfoldStep (TOptions maxdepth minls ord) (TSd depth tst)
  | depth >= maxdepth || sizeDs tst <= minls = Left (tsDataset tst)
  | sizeDs tsl == 0 = Left (tsDataset tsr)
  | sizeDs tsr == 0 = Left (tsDataset tsl)
  | otherwise = Right (mdata, tdsl, tdsr)
  where
    sizeDs = size . tsDataset
    mdata = TNData jstar tstar
    (jstar, tstar, tsl, tsr) = maxInfoGainSplit ordf tst
    ordf = fromOrder ord
    d' = depth + 1
    tdsl = TSd d' tsl
    tdsr = TSd d' tsr


{- | Note (OPTIMIZATIONS maxInfoGainSplit)

1. After splitting a dataset, remove the (threshold, feature index) pair corresponding to the succesful split

2. " " " " , remove /all/ (threshold, index) pairs that are subsumed by the successful test (e.g in the test ((<), 3.2, 27) , remove all [(t, 27) | t <- [tmin ..], t < 3.2 ] ). This is only a useful optimization for /monotonic/ class boundaries.
-}

-- | Tabulate the information gain for a number of decision thresholds and return a decision function corresponding to the threshold that yields the maximum information gain.
maxInfoGainSplit :: (Ord k, Ord a, Eq a) =>
                    (a -> a -> Bool)
                 -> TState k a
                 -> (Int, a, TState k a, TState k a)
maxInfoGainSplit decision (TState tjs ds) = (jstar, tstar, TState tjs' dsl, TState tjs' dsr) where
  tjs' = S.delete (jstar, tstar) tjs  -- See Note (OPTIMIZATIONS maxInfoGainSPlit)
  (jstar, tstar, _, dsl, dsr) = F.maximumBy (comparing third5) $ infog `map` (S.toList tjs)  
  infog (j, t) = (j, t, h, dsl, dsr) where
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

-- partitionJoint :: (Ord k, Foldable t) =>
--                   XV.V (a -> Bool)  -- ^ Joint decision function
--                -> Dataset k (t (XV.V a))
--                -> (Dataset k [XV.V a], Dataset k [XV.V a])
-- partitionJoint ps ds@Dataset{} = foldrWithKey insf (empty, empty) ds where
--   insf k lrow (l, r) = (insert k lp l, insert k rp r) where    
--     (lp, rp) = partition1 (XV.allComponents ps) lrow     

-- | Partition a Foldable in two lists according to a predicate
partition1 :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
partition1 p = foldr ins ([], [])  where
  ins x (l, r) | p x = (x : l, r)
               | otherwise = (l , x : r)




-- | All (j, t) cuts for a given class.
allCuts :: (Traversable f, Applicative v, Foldable v, Fractional a, Ord a) => a -> a -> f (v a) -> [(Int, a)]
allCuts xmin dx xs = concatMap ixed vls where
  vls = zip [0..] $ F.toList $ featureBinnedMeans xmin dx xs
  ixed (i, xs) = zip (repeat i) xs

featureBinnedMeans :: (Traversable t, Applicative v, Fractional a, Ord a) =>
                      a  -- ^ Min value
                   -> a  -- ^ Bin width
                   -> t (v a) 
                   -> v [a]
featureBinnedMeans xmin dx = featureSummary binnedMeans where
  binnedMeans xs = F.toList $ mean <$> fromFoldableAppendIM (bin xmin dx) xs

fromFoldableAppendIM :: Foldable t => (a -> IM.Key) -> t a -> IM.IntMap [a]
fromFoldableAppendIM kf xs = IM.fromListWith (++) $ map (kf &&& (: [])) $ F.toList xs

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / n where
  n = fromIntegral $ length xs



-- | A well-defined Ordering, for strict half-plane separation
data Order = LessThan | GreaterOrEqual deriving (Eq, Ord, Enum, Bounded)
instance Show Order where
  show LessThan = "<"
  show GreaterOrEqual = ">="

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









