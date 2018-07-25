{-# language TypeFamilies, MultiParamTypeClasses, InstanceSigs, RankNTypes #-}
module Numeric.Classification.Internal.Datum (Datum(..), (!?), splitAttrP) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU



-- class IxFunctor f where
--   type IKey f :: * 
--   imap :: (IKey f -> a -> b) -> f a -> f b

-- instance IxFunctor (SparseD k) where
--   type IKey (SparseD k) = k
--   imap f (SD mm) = SD $ M.mapWithKey f mm

-- instance IxFunctor SparseDI where
--   type IKey SparseDI = IM.Key
--   imap f (SDI mm) = SDI $ IM.mapWithKey f mm 







-- | Return a 'Datum' decision function according to a Boolean function of one of its attributes
splitAttrP :: Datum d => (Attr d -> Bool) -> Key d -> (d -> Bool)
splitAttrP p k dat = maybe False p (dat !? k)

class Datum d where
--   {-# minimal lookupAttribute, fromList #-}
  type Key d :: *
  type Attr d :: *
  type V d :: *
  lookupAttribute :: d -> Key d -> Maybe (Attr d)
  fromList :: [V d] -> d

(!?) :: Datum d => d -> Key d -> Maybe (Attr d)
(!?) = lookupAttribute  

-- | A 'DenseD' datum is internally an unboxed vector
newtype DenseD a = DD { unDD :: VU.Vector a } deriving (Eq, Show)

instance VU.Unbox a => Datum (DenseD a) where
  type Key (DenseD a) = Int
  type Attr (DenseD a) = a
  type V (DenseD a) = a
  lookupAttribute (DD v) i = v VU.!? i
  fromList = DD . VU.fromList


-- | A 'SparseD' datum is internally a 'Map'
newtype SparseD k a = SD { unSD :: M.Map k a } deriving (Eq, Show)

instance Ord k => Datum (SparseD k a) where
  type Key (SparseD k a) = k 
  type Attr (SparseD k a) = a
  type V (SparseD k a) = (k, a)
  lookupAttribute (SD mm) i = M.lookup i mm
  fromList = SD . M.fromList

-- | A 'SparseDI' datum is internally an 'IntMap'
newtype SparseDI a = SDI { unSDI :: IM.IntMap a } deriving (Eq, Show)

instance Datum (SparseDI a) where
  type Key (SparseDI a) = IM.Key
  type Attr (SparseDI a) = a
  type V (SparseDI a) = (IM.Key, a)
  lookupAttribute (SDI mm) i = IM.lookup i mm
  fromList = SDI . IM.fromList


