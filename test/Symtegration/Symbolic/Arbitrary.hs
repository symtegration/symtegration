{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: QuickCheck Arbitrary instances for generating Symtegration.Symbolic values.
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Arbitrary where

import Data.String (fromString)
import Symtegration.Symbolic
import Test.QuickCheck

-- | QuickCheck modifier for generating symbolic mathematical expressions with a particular value.
-- Specically, mathematical expressions representing a single symbol or a single number will
-- be generated.
newtype Value = Value Expression deriving (Eq, Show)

instance Arbitrary Value where
  arbitrary =
    oneof
      [ Value . Number <$> arbitrary,
        Value . Symbol . fromString <$> listOf arbitraryPrintableChar
      ]
