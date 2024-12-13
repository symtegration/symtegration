-- |
-- Description: General testing of specific integration algorithms with numeric coefficients.
-- Maintainer: dev@chungyc.org
module Symtegration.IntegrationSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.AD
import Symtegration.FiniteDouble
import Symtegration.Integration.Polynomial qualified as Polynomial
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  -- Each integration algorithm should have their own tests,
  -- where they focus the input expressions which are generated.
  modifyMaxSuccess (* 10) $ context "for any expression" $ do
    describe "integral consistent with derivative" $ do
      prop "for polynomial integration" $
        antiderivativeProperty Polynomial.rationalIntegrate

      prop "for trigonometric integration" $
        antiderivativeProperty Trigonometric.integrate

antiderivativeProperty ::
  (Text -> Expression -> Maybe Expression) ->
  Complete ->
  Double ->
  Property
antiderivativeProperty integrate (Complete e m) x =
  not (Map.null m) ==> forAll (elements $ Map.keys m) $ \v -> check (integrate v e) v
  where
    check Nothing _ = label "integration fail" True
    check (Just integrated) v =
      isFinite (FiniteDouble $ f x) && isFinite (FiniteDouble $ f' x) ==>
        label "integration success" $
          counterexample ("derivative = " <> Text.unpack (toHaskell e)) $
            counterexample ("antiderivative = " <> Text.unpack (toHaskell integrated)) $
              Near (FiniteDouble (f' x)) `shouldBe` Near (FiniteDouble (f x))
      where
        -- The original function and the derivative of the integral should behave similarly.
        --
        -- These are (Double -> Double).  It seems Numeric.AD does not like FiniteDouble.
        f = toFunction e (replace v)
        f' = diff (toFunction integrated (replaceForDiff v))

    -- Map all but the variable symbol to concrete numbers.
    replace var s
      | s == var = id
      | (Just (FiniteDouble z)) <- Map.lookup s m = const z
      | otherwise = error "unknown symbol"
    replaceForDiff var s
      | s == var = id
      | (Just (FiniteDouble z)) <- Map.lookup s m = const $ auto z
      | otherwise = error "unknown symbol"
