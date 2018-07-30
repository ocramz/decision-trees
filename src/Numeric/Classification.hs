module Numeric.Classification (
  -- * Tree
  Tree(..), growTree, TOptions(..), Order(..), TNData(..)
  -- * Dataset
  , D.Dataset(..), D.fromListWith -- D.showDsTree,
  -- * Data point
    , X.V(..), X.mkV, (X.!), X.indexUnsafe, X.dim, X.toList, X.fromList
  -- ** Helpers
  , entropyR, gini
  -- **
  , drawTree
  )
  where

import qualified Data.Dataset as D
import Numeric.Classification.DecisionTrees
import Numeric.InformationTheory (entropyR, gini)
import qualified Numeric.Classification.Internal.Datum.Vector as X
import Numeric.Classification.Exceptions (DataException(..))






