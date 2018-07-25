{-# language TypeFamilies #-}
module Numeric.Classification.Internal.Datum (Datum(..), (!?), splitAttrP) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU

class Datum d where
  {-# minimal lookupAttribute #-}
  type DKey d :: *
  type DData d :: *
  type Dkv d :: *
  lookupAttribute :: d -> DKey d -> Maybe (DData d)
  fromList :: [Dkv d] -> d

(!?) :: Datum d => d -> DKey d -> Maybe (DData d)
(!?) = lookupAttribute  

-- | A 'DenseD' datum is internally an unboxed vector
newtype DenseD a = DD { unDD :: VU.Vector a } deriving (Eq, Show)

instance VU.Unbox a => Datum (DenseD a) where
  type DKey (DenseD a) = Int
  type DData (DenseD a) = a
  type Dkv (DenseD a) = a
  lookupAttribute (DD v) i = v VU.!? i
  fromList = DD . VU.fromList

-- | A 'SparseD' datum is internally a 'Map'
newtype SparseD k a = SD { unSD :: M.Map k a } deriving (Eq, Show)

instance Ord k => Datum (SparseD k a) where
  type DKey (SparseD k a) = k 
  type DData (SparseD k a) = a
  type Dkv (SparseD k a) = (k, a)
  lookupAttribute (SD mm) i = M.lookup i mm
  fromList = SD . M.fromList

-- | A 'SparseDI' datum is internally an 'IntMap'
newtype SparseDI a = SDI { unSDI :: IM.IntMap a } deriving (Eq, Show)

instance Datum (SparseDI a) where
  type DKey (SparseDI a) = IM.Key
  type DData (SparseDI a) = a
  type Dkv (SparseDI a) = (IM.Key, a)
  lookupAttribute (SDI mm) i = IM.lookup i mm
  fromList = SDI . IM.fromList



-- | Return a 'Datum' decision function according to a Boolean function of one of its attributes
splitAttrP :: Datum d => (DData d -> Bool) -> DKey d -> (d -> Bool)
splitAttrP p k dat = maybe False p (dat !? k)
