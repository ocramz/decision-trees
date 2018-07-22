module Numeric.Classification.DecisionTrees where

import qualified Data.Map as M



-- | Computes the entropy of a Dataset
--
-- the Entropy is defined as: sum (p_i * log_2 p_i)
-- where p_i = |{ x | x has Label i}|/|Dataset|

newtype Dataset k a = Dataset { unDataset :: M.Map k [a] } deriving (Eq, Show)

empty :: Dataset k a
empty = Dataset M.empty

insert :: Ord k => k -> [a] -> Dataset k a -> Dataset k a
insert k ls (Dataset ds) = Dataset $ M.insert k ls ds

-- | Size of the dataset
size :: Dataset k a -> Int
size (Dataset ds) = M.foldl' (\acc l -> acc + length l) 0 ds

data Label a k = Label (a -> k) (k -> Bool)

-- lab0 = Label $ \x -> if x > 0 then 1 else 0



-- | Partition a dataset accoring to a list of labeling functions
partition :: Ord k => [Label a k] -> [a] -> Dataset k a -> Dataset k a
partition [] _ ds = ds
partition (lab0:ls) xs ds
  | null xs = ds
  | otherwise = let
      (ds', neg) = partition1 lab0 xs ds
      in partition ls neg ds' 

partition1 :: (Ord k, Foldable t) =>
              Label a k
           -> t a
           -> Dataset k a
           -> (Dataset k a, [a])
partition1 (Label pf p) xs ds
  | null pos = (ds, neg) 
  | otherwise = (insert k pos ds, neg)
  where
  k = pf $ head pos
  (pos, neg) = foldr ins ([], []) xs
  ins x (l, r) | p (pf x)  = (x : l, r)
               | otherwise = (l, x : r)

-- partition :: Foldable t => (a -> Bool) -> t a -> ([a], [a])
-- partition p = foldr ins ([], []) where
--   ins x (l, r) | p x = (x : l, r)
--                | otherwise = (l, x : r)





-- groupWith getKey singleton fuse = 
--     foldl (\m x -> M.insertWith fuse (getKey x) (singleton x) m) M.empty
