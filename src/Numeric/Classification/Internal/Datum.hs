{-# language TypeFamilies, MultiParamTypeClasses, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Numeric.Classification.Internal.Datum
 --   (Datum(..), (!?), splitAttrP)
 where

import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
-- import qualified Data.Vector.Unboxed as VU



-- | Return a 'Datum' decision function according to a Boolean function of one of its attributes
splitAttrP :: Datum d => (a -> Bool) -> Key d -> (d a -> Bool)
splitAttrP p k dat = maybe False p (dat !? k)

class Foldable d => Datum (d :: * -> *) where
--   {-# minimal lookupAttribute, fromList #-}
  type Key d :: * 
  type V d a :: *
  lookupAttribute :: d a -> Key d -> Maybe a
  fromList :: [V d a] -> d a
  imap :: (Key d -> a -> b) -> d a -> d b

(!?) :: Datum d => d a -> Key d -> Maybe a
(!?) = lookupAttribute  

maximumD, minimumD :: (Datum d, Ord a) => d a -> a
maximumD = maximum . F.toList
minimumD = minimum . F.toList


-- | A 'SparseD' datum is internally a 'Map'
newtype SparseD k a = SD { unSD :: M.Map k a } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Ord k => Datum (SparseD k) where
  type Key (SparseD k) = k 
  type V (SparseD k) a = (k, a)
  lookupAttribute (SD mm) i = M.lookup i mm
  fromList = SD . M.fromList
  imap f (SD mm) = SD $ M.mapWithKey f mm

-- | A 'SparseDI' datum is internally an 'IntMap'
newtype SparseDI a = SDI { unSDI :: IM.IntMap a } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Datum SparseDI where
  type Key SparseDI = IM.Key
  type V SparseDI a = (IM.Key, a)
  lookupAttribute (SDI mm) i = IM.lookup i mm
  fromList = SDI . IM.fromList
  imap f (SDI mm) = SDI $ IM.mapWithKey f mm  


-- -- | A 'DenseD' datum is internally an unboxed vector
-- newtype DenseD a = DD { unDD :: VU.Vector a } deriving (Eq, Show)

-- instance VU.Unbox a => Datum DenseD where
--   type Key (DenseD a) = Int
--   type V (DenseD a) = a
--   lookupAttribute (DD v) i = v VU.!? i
--   fromList = DD . VU.fromList
