-- |
-- Description: Tests for Symtegration.Polynomial.Symbolic
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.SymbolicSpec (spec) where

import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "fromExpression" $ do
    prop "is inverse of toExpression" $ \p (SymbolText s) ->
      let e = toExpression s toRationalCoefficient (p :: IndexedPolynomial)
          p' = fromExpression (forVariable s) e
       in counterexample ("p = " <> show p) $
            counterexample ("p'" <> show p') $
              -- With exact rational coefficients, the polynomial representation of
              -- a particular polynomial is unique.
              p' `shouldBe` Just p

    prop "from number" $ \(SymbolText s) n ->
      fromExpression (forVariable s) (Number n)
        `shouldBe` Just (fromInteger n :: IndexedPolynomial)

    prop "from symbol" $ \(SymbolText s) ->
      fromExpression (forVariable s) (Symbol s)
        `shouldBe` Just (power 1 :: IndexedPolynomial)

    prop "from symbol with exponent" $ \(SymbolText s) (Positive n) ->
      n > 1 ==>
        fromExpression (forVariable s) (Symbol s :**: Number n)
          `shouldBe` Just (power (fromIntegral n) :: IndexedPolynomial)
