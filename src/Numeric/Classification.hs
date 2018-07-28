module Numeric.Classification (
  -- * Tree
  Tree, growTree, TOptions(..), Order(..), TNData(..),
  -- * Dataset
  Dataset(..), fromList, toList, uniques,
  -- * Data point
  XV.V(..), XV.mkV
  ) where

import Data.Dataset
import Numeric.Classification.DecisionTrees
import qualified Numeric.Classification.Internal.Datum.Vector as XV
import Numeric.Classification.Exceptions (DataException(..))
