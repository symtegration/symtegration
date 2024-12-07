{-# OPTIONS_GHC -fno-warn-orphans #-}

module Symtegration.Symbolic.Arbitrary where

import Data.Map (Map)
import Data.String (fromString)
import Data.Text (Text)
import Symtegration.Symbolic
import Test.QuickCheck

newtype Value = Value Expression deriving (Eq, Show)

instance Arbitrary Value where
  arbitrary =
    oneof
      [ Value . Number <$> arbitrary,
        Value . Symbol . fromString <$> listOf arbitraryPrintableChar
      ]

data ComputedResult = IntegerResult Integer | DoubleResult Double
  deriving (Eq, Ord, Show)

newtype ComputedExpression = ComputedExpression (Expression, Map Text ComputedResult, ComputedResult)
  deriving (Eq, Ord, Show)
