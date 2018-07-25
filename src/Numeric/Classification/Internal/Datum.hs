{-# language TypeFamilies #-}
module Numeric.Classification.Internal.Datum where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as VU

class Datum d where
  type DKey d :: *
  type DData d :: *
  lookupAttribute :: d -> DKey d -> Maybe (DData d)

(!?) :: Datum d => d -> DKey d -> Maybe (DData d)
(!?) = lookupAttribute  

newtype DenseD a = DD { unDD :: VU.Vector a } deriving (Eq, Show)

fromListDD :: VU.Unbox a => [a] -> DenseD a
fromListDD ll = DD $ VU.fromList ll

instance VU.Unbox a => Datum (DenseD a) where
  type DKey (DenseD a) = Int
  type DData (DenseD a) = a
  lookupAttribute (DD v) i = v VU.!? i

newtype SparseD k a = SD { unSD :: M.Map k a } deriving (Eq, Show)

fromListSD :: Ord k => [(k, a)] -> SparseD k a
fromListSD ll = SD $ M.fromList ll

instance Ord k => Datum (SparseD k a) where
  type DKey (SparseD k a) = k 
  type DData (SparseD k a) = a
  lookupAttribute (SD mm) i = M.lookup i mm

newtype SparseDI a = SDI { unSDI :: IM.IntMap a } deriving (Eq, Show)

fromListSDI :: [(IM.Key, a)] -> SparseDI a
fromListSDI ll = SDI $ IM.fromList ll

instance Datum (SparseDI a) where
  type DKey (SparseDI a) = IM.Key
  type DData (SparseDI a) = a
  lookupAttribute (SDI mm) i = IM.lookup i mm



-- | Return a 'Datum' decision function according to a Boolean function of one of its attributes
splitAttrP :: Datum d => (DData d -> Bool) -> DKey d -> (d -> Bool)
splitAttrP p k dat = maybe False p (dat !? k)
