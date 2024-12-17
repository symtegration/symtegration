-- |
-- Description: Tests for Symtegration.Symbolic.Simplify.AlgebraicRingOrder.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.AlgebraicRingOrderSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Simplify.AlgebraicRingOrder
import Symtegration.Symbolic.Simplify.Properties
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "simplify" $ do
    modifyMaxSuccess (* 100) $
      prop "maintains semantics" $
        equivalentProperty' order

equivalentProperty' :: (Text -> Expression -> Expression) -> Complete -> Property
equivalentProperty' f (Complete e m) = do
  forAll (elements $ Map.keys m) $ \v ->
    equivalentProperty (f v) (Complete e m)
