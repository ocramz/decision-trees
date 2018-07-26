{-# language TypeFamilies, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Numeric.Classification.Internal.Datum
 --   (Datum(..), (!?), splitAttrP)
 where

import qualified Data.Foldable as F
-- import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
-- import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
-- import Data.Typeable
import Control.Monad.Catch (MonadThrow(..))
import Numeric.Classification.Exceptions


-- | A data point, with features labeled by index j
newtype X j a = X { unX :: M.Map j a } deriving (Eq, Show, Functor, Foldable, Traversable)

empty :: X j a 
empty = X M.empty

(!?) :: Ord j => X j a -> j -> Maybe a
(X mm) !? j = M.lookup j mm

-- | Index and default values for a datum
newtype XDef j a = XDef (V.Vector (j, a)) deriving (Eq, Show)

mkXDef :: Int -> [j] -> [a] -> Maybe (XDef j a)
mkXDef n ixs xds | q1 && q2 = Just $ XDef $ V.fromList $ zip ixs xds
                 | otherwise = Nothing where
                     q1 = length ixs == n
                     q2 = length xds == n

-- lookupXDef :: Int -> XDef j a -> Maybe (j, a)
lookupXDef j (XDef ixds) | q = snd $ ixds V.! j where
                             q = 0 <= j && j < V.length ixds

-- |

fromOrdering :: Ord a => Ordering -> a -> a -> Bool
fromOrdering c = case c of
  LT -> (<)
  _  -> (>=)

data Split j a = Split {
    splitValueDefault :: a
  , splitFeatureIx :: j
  , splitFun :: Ordering } deriving (Eq, Show)

-- | Returns a 'Datum' decision function according to a Boolean function of its j'th feature
--
-- Supply a default value that's tested if the feature is missing
dataSplitDecisionWD :: (Ord a, Ord j) => Split j a -> a -> X j a -> Bool
dataSplitDecisionWD (Split tdef j o) t dat = maybe z (`p` t) (dat !? j) where
  p = fromOrdering o
  z | tdef `p` t = True
    | otherwise = False

-- dataSplitDecisionWD :: Ord j => (a -> Bool) -> Xdef j a -> (X j a -> Bool)
-- dataSplitDecisionWD p (Xdef j def) dat = maybe z p (dat !? j) where
--   z | p def = True
--     | otherwise = False

-- | Returns a 'Datum' decision function according to a Boolean function of its j'th feature
--
-- NB : missing features _fail_ the test by default (which is generally a bad idea)
dataSplitDecision :: Ord j => (a -> Bool) -> j -> X j a -> Bool
dataSplitDecision p j dat = maybe False p (dat !? j)
  
-- splitAttrPM :: (MonadThrow m, Show j, Typeable j, Ord j) => j -> X j a -> m a
-- splitAttrPM j dat = maybe (throwM $ MissingFeatureE j) pure (dat !? j)

mapWithKey :: (j -> a -> b) -> X j a -> X j b
mapWithKey f (X mm) = X $ M.mapWithKey f mm

foldrWithKey :: (j -> a -> b -> b) -> b -> X j a -> b
foldrWithKey f z (X mm) = M.foldrWithKey f z mm

foldlWithKey' :: (a -> j -> b -> a) -> a -> X j b -> a
foldlWithKey' f z (X mm) = M.foldlWithKey' f z mm

union :: Ord j => X j a -> X j a -> X j a
union (X m1) (X m2) = X $ M.union m1 m2

unionWithKey :: Ord j => (j -> a -> a -> a) -> X j a -> X j a -> X j a
unionWithKey f (X m1) (X m2) = X $ M.unionWithKey f m1 m2

fromList :: Ord j => [(j, a)] -> X j a 
fromList = X . M.fromList

toList :: X j a -> [(j, a)]
toList (X m) = M.toList m





-- * Typeclass-based interface


-- splitAttrP :: Datum d => (a -> Bool) -> Key d -> (d a -> Bool)
-- splitAttrP p k dat = maybe False p (dat !? k)

-- class Foldable d => Datum (d :: * -> *) where
-- --   {-# minimal lookupAttribute, fromList #-}
--   type Key d :: * 
--   type V d a :: *
--   lookupAttribute :: d a -> Key d -> Maybe a
--   fromList :: [V d a] -> d a
--   imap :: (Key d -> a -> b) -> d a -> d b

-- (!?) :: Datum d => d a -> Key d -> Maybe a
-- (!?) = lookupAttribute  

-- maximumD, minimumD :: (Datum d, Ord a) => d a -> a
-- maximumD = maximum . F.toList
-- minimumD = minimum . F.toList


-- -- | A 'SparseD' datum is internally a 'Map'
-- newtype SparseD k a = SD { unSD :: M.Map k a } deriving (Eq, Show, Functor, Foldable, Traversable)

-- instance Ord k => Datum (SparseD k) where
--   type Key (SparseD k) = k 
--   type V (SparseD k) a = (k, a)
--   lookupAttribute (SD mm) i = M.lookup i mm
--   fromList = SD . M.fromList
--   imap f (SD mm) = SD $ M.mapWithKey f mm

-- -- | A 'SparseDI' datum is internally an 'IntMap'
-- newtype SparseDI a = SDI { unSDI :: IM.IntMap a } deriving (Eq, Show, Functor, Foldable, Traversable)

-- instance Datum SparseDI where
--   type Key SparseDI = IM.Key
--   type V SparseDI a = (IM.Key, a)
--   lookupAttribute (SDI mm) i = IM.lookup i mm
--   fromList = SDI . IM.fromList
--   imap f (SDI mm) = SDI $ IM.mapWithKey f mm  


-- -- -- | A 'DenseD' datum is internally an unboxed vector
-- -- newtype DenseD a = DD { unDD :: VU.Vector a } deriving (Eq, Show)

-- -- instance VU.Unbox a => Datum DenseD where
-- --   type Key (DenseD a) = Int
-- --   type V (DenseD a) = a
-- --   lookupAttribute (DD v) i = v VU.!? i
-- --   fromList = DD . VU.fromList
