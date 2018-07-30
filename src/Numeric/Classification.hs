module Numeric.Classification (
  -- * Tree
  Tree(..), growTree, TOptions(..), Order(..), TNData(..),
  -- * Dataset
  D.Dataset(..), D.fromListWith, 
  -- * Data point
  X.V(..), X.mkV, (X.!), X.indexUnsafe, X.dim, X.toList, X.fromList
  -- ,
  -- -- ** Helpers
  -- allCuts
  )
  where

import qualified Data.Dataset as D
import Numeric.Classification.DecisionTrees
import qualified Numeric.Classification.Internal.Datum.Vector as X
import Numeric.Classification.Exceptions (DataException(..))






