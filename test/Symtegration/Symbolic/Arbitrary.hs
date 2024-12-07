{-# OPTIONS_GHC -fno-warn-orphans #-}

module Symtegration.Symbolic.Arbitrary where

import Data.Map (Map)
import Data.Text (Text)
import Symtegration.Symbolic

data ComputedResult = IntegerResult Integer | DoubleResult Double
  deriving (Eq, Ord, Show)

newtype ComputedExpression = ComputedExpression (Expression, Map Text ComputedResult, ComputedResult)
  deriving (Eq, Ord, Show)
