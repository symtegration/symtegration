{-# OPTIONS_GHC -fno-warn-orphans #-}

module Symtegration.Symbolic.Arbitrary where

import Data.Map (Map)
import Symtegration.Symbolic

data ComputedResult = IntegerResult Integer | DoubleResult Double
  deriving (Eq, Ord, Show)

newtype SymbolMap = SymbolMap (Map Symbol ComputedResult)
  deriving (Eq, Ord, Show)

newtype ComputedExpression = ComputedExpression (Expression, SymbolMap, ComputedResult)
  deriving (Eq, Ord, Show)
