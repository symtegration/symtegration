-- |
-- Description: Tests Symtegration.Symbolic.Simplify.Factor.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.FactorSpec (spec) where

import Symtegration.Symbolic.Simplify.Factor
import Symtegration.Symbolic.Simplify.Properties
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "simplify" $ do
    modifyMaxSuccess (* 100) $
      prop "maintains semantics" $
        equivalentProperty simplify
