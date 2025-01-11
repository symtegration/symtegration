-- |
-- Description: Tests Symtegration.Symbolic.Simplify.Factor.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.FactorSpec (spec) where

import Data.Text (unpack)
import Symtegration.Symbolic
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify.Factor
import Symtegration.Symbolic.Simplify.Properties
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "simplify" $ do
    modifyMaxSuccess (* 100) $
      prop "maintains semantics" $
        equivalentProperty simplify

    prop "factors common sub-expressions" $ \x y z ->
      x /= y && x /= z && y /= z ==>
        conjoin
          [ simplify ((z * x) + (z * y)) === z * (x + y),
            simplify ((z * x) + (y * z)) === z * (x + y),
            simplify ((x * z) + (z * y)) === z * (x + y),
            simplify ((x * z) + (y * z)) === z * (x + y)
          ]

    prop "factors common factors in multiplication" $ \n m x y ->
      x /= y ==>
        let n' = fromIntegral n
            m' = fromIntegral m
            f "x" = Just x
            f "y" = Just y
            f _ = Nothing
            eval e = fractionalEvaluate e f :: Maybe Rational
            shouldEvaluateTo e z =
              counterexample (unpack $ toHaskell e) $
                eval e `shouldBe` Just z
         in conjoin
              [ let e = Number n + Number m
                 in e `shouldEvaluateTo` (n' + m'),
                let e = Number n + Number m * "y"
                 in e `shouldEvaluateTo` (n' + m' * y),
                let e = Number n * "x" + Number m
                 in e `shouldEvaluateTo` (n' * x + m'),
                let e = Number n * "x" + Number m * "y"
                 in e `shouldEvaluateTo` (n' * x + m' * y)
              ]

    prop "cancels out common factors in division" $ \n m x y ->
      x /= y && y /= 0 && m /= 0 ==>
        let n' = fromIntegral n :: Rational
            m' = fromIntegral m :: Rational
            f "x" = Just x
            f "y" = Just y
            f _ = Nothing
            e = (Number n * "x") / (Number m * "y")
            e' = simplify e
         in counterexample ("e = " <> unpack (toHaskell e)) $
              counterexample ("simplify e = " <> unpack (toHaskell e')) $
                fractionalEvaluate e f
                  `shouldBe` Just ((n' * x) / (m' * y))
