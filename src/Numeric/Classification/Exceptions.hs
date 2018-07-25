module Numeric.Classification.Exceptions where

import Control.Exception
import Data.Typeable


-- * Exceptions

data ValueException = ZeroProbabilityE String deriving (Eq, Show, Typeable)

instance Exception ValueException 


data DataException i = MissingFeatureE i deriving (Eq, Typeable)
instance Show i => Show (DataException i) where
  show e = case e of
    MissingFeatureE i -> unwords ["Missing feature", show i]

instance (Show i, Typeable i) => Exception (DataException i)
