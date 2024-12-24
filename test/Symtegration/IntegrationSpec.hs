-- |
-- Description: General testing of specific integration algorithms with numeric coefficients.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.IntegrationSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.FiniteDouble
import Symtegration.Integration
import Symtegration.Integration.Exponential qualified as Exponential
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Properties qualified as Properties
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  -- Each integration algorithm should have their own tests,
  -- where they focus the input expressions which are generated.
  -- These tests are for checking whether they could have problems
  -- with expressions they do not focus on.
  modifyMaxSuccess (* 10) $ context "for any expression" $ do
    describe "integral consistent with derivative" $ do
      prop "for integration of powers" $
        antiderivativeProperty Powers.integrate

      prop "for trigonometric integration" $
        antiderivativeProperty Trigonometric.integrate

      prop "for integration of exponential and logarithmic functions" $
        antiderivativeProperty Exponential.integrate

      prop "for general integration" $
        antiderivativeProperty integrate

antiderivativeProperty ::
  (Text -> Expression -> Maybe Expression) ->
  Complete ->
  Double ->
  Property
antiderivativeProperty f (Complete e m) =
  Properties.antiderivativeProperty f (Map.map (\(FiniteDouble z) -> z) m) e
