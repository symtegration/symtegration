-- |
-- Description: Tests Symtegration.Symbolic.Simplify.Symbolic
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.SymbolicFoldingSpec (spec) where

import Symtegration.Symbolic.Simplify.Properties
import Symtegration.Symbolic.Simplify.SymbolicFolding
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "simplify" $ do
    modifyMaxSuccess (* 100) $
      prop "maintains semantics" $
        equivalentProperty simplify
