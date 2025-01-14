-- |
-- Description: Tests Symtegration.Symbolic.Simplify.Fraction
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.FractionSpec (spec) where

import Symtegration.Symbolic.Simplify.Fraction
import Symtegration.Symbolic.Simplify.Properties
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "simplify" $ do
    modifyMaxSuccess (* 100) $
      prop "maintains semantics" $
        equivalentProperty simplify
