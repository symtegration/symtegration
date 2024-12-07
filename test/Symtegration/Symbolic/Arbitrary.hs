{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: QuickCheck Arbitrary instances for generating Symtegration.Symbolic values.
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Arbitrary where

import Data.String (fromString)
import Generic.Random.DerivingVia
import Symtegration.Symbolic
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

deriving via
  (GenericArbitraryRec '[1, 1, 1, 1] `AndShrinking` Expression)
  instance
    Arbitrary Expression

deriving via (GenericArbitraryU UnaryFunction) instance Arbitrary UnaryFunction

deriving via (GenericArbitraryU BinaryFunction) instance Arbitrary BinaryFunction

-- | QuickCheck modifier for generating simple symbolic mathematical expressions.
-- Specically, those which represent a single symbol or a single number.
newtype Simple = Simple Expression deriving (Eq, Show)

instance Arbitrary Simple where
  arbitrary =
    oneof
      [ Simple . Number <$> arbitrary,
        Simple . Symbol . fromString <$> listOf arbitraryPrintableChar
      ]
