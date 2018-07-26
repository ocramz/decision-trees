{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Numeric.Classification.Internal.Datum.Vector where

-- import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import Control.Monad.Catch (MonadThrow(..))
import Numeric.Classification.Exceptions

import Prelude hiding (zip, unzip)

newtype V a = V (V.Vector a) deriving (Eq, Show, Functor, Foldable, Traversable)

fromList :: [a] -> V a
fromList = V . V.fromList
{-# inline fromList #-}

zip :: V a -> V b -> V (a, b)
zip (V v1) (V v2) = V $ V.zip v1 v2
{-# inline zip #-}

unzip :: V (a, b) -> (V a, V b)
unzip (V vs) = (V v1, V v2) where
  (v1, v2) = V.unzip vs
{-# inline unzip #-}  

mkV :: MonadThrow m => Int -> V.Vector a -> m (V a)
mkV n xs | dxs == n = pure $ V xs
         | otherwise = throwM $ DimMismatchE "mkV" n dxs where
             dxs = V.length xs

indexUnsafe :: V a -> Int -> a
(V vv) `indexUnsafe` j = vv V.! j
{-# inline indexUnsafe #-}

(!) :: MonadThrow m => V a -> Int -> m a
v ! j | j >= 0 && j < d = pure $ v `indexUnsafe` j
             | otherwise = throwM $ IndexOobE "(!)" j 0 d where
                 d = dim v

dim :: V a -> Int
dim (V vv) = V.length vv
{-# inline dim #-}

-- foldrWithKey :: (Int -> a -> b -> b) -> b -> V a -> b
-- foldrWithKey f z (V vv) = foldr ins z $ zip [0..] (V.toList vv) where
--   ins (i, x) acc = f i x acc

foldrWithKey :: (Int -> a -> b -> b) -> b -> V a -> b
foldrWithKey f z vv = foldr ins z $ zip (fromList [0..]) vv where
  ins (i, x) acc = f i x acc
{-# inline foldrWithKey #-}  


dataSplitDecision :: (a -> Bool) -> Int -> (V a -> Bool)
dataSplitDecision p j dat = p (dat `indexUnsafe` j)

-- dataSplitDecision :: Ord j => (a -> Bool) -> j -> X j a -> Bool
-- dataSplitDecision p j dat = maybe False p (dat !? j)







-- | Vectors with measurable entries

-- data Measurable a = BoundedBoth a a deriving (Eq, Show)

-- newtype Xf f a = Xf (V.Vector (f a))

-- type XMeas = Xf Measurable
