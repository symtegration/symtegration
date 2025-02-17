-- |
-- Description: Tests "Symtegration.Polynomial.Rational".
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.RationalSpec (spec) where

import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Symtegration.Polynomial.Rational
import Symtegration.Polynomial.Rational.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Function, scale)

spec :: Spec
spec = parallel $ do
  describe "Function" $ do
    prop "numerator is coprime with denominator" $ \x ->
      let Function p q = x :: Function IndexedPolynomial
       in greatestCommonDivisor p q `shouldSatisfy` (== 0) . degree

    prop "denominator is monic" $ \x ->
      let Function _ p = x :: Function IndexedPolynomial
       in p `shouldSatisfy` (== 1) . leadingCoefficient

  describe "equality" $ do
    prop "is equal" $ \x ->
      x == (x :: Function IndexedPolynomial) `shouldBe` True

    prop "is not equal" $ \x (NonZero y) ->
      x /= (x + y :: Function IndexedPolynomial) `shouldBe` True

    prop "is equal when numerator and denominator equally scaled" $ \p q r ->
      fromPolynomials p q `shouldBe` fromPolynomials (p * r) (q * r :: IndexedPolynomial)

    it "does not equate zero and one" $
      0 == (1 :: Function IndexedPolynomial) `shouldBe` False

  describe "addition" $ do
    prop "zero is identity" $ \x ->
      let zero = 0 :: Function IndexedPolynomial
       in x + zero `shouldBe` x

    prop "is commutative" $ \x y ->
      x + y `shouldBe` (y + x :: Function IndexedPolynomial)

  describe "subtraction" $ do
    prop "is inverse of addition" $ \x y ->
      (x + y) - y `shouldBe` (x :: Function IndexedPolynomial)

  describe "multiplication" $ do
    prop "one is identity" $ \x ->
      let one = 1 :: Function IndexedPolynomial
       in x * one `shouldBe` x

    prop "is commutative" $ \x y ->
      x * y `shouldBe` (y * x :: Function IndexedPolynomial)

  describe "division" $ do
    prop "is inverse of multiplication" $ \(NonZero x) (NonZero y) ->
      (x * y) / y `shouldBe` (x :: Function IndexedPolynomial)

  describe "negate" $ do
    prop "is same as subtraction from zero" $ \x ->
      let zero = 0 :: Function IndexedPolynomial
       in negate x `shouldBe` zero - x

  describe "recip" $ do
    prop "is same as dividing one" $ \(NonZero x) ->
      recip x `shouldBe` (1 / x :: Function IndexedPolynomial)

  describe "abs and signum" $ do
    prop "multiplies to original" $ \x ->
      abs x * signum x `shouldBe` (x :: Function IndexedPolynomial)

  describe "fromInteger" $ do
    prop "zero has zero numerator" $ \(NonZero x) ->
      fromPolynomials 0 x `shouldBe` (0 :: Function IndexedPolynomial)

    prop "one has equal numerator and denominator" $ \(NonZero x) ->
      fromPolynomials x x `shouldBe` (1 :: Function IndexedPolynomial)

  describe "fromRational" $ do
    prop "is ratio" $ \x (NonZero y) ->
      let x' = fromPolynomial $ scale (fromIntegral (x :: Integer)) 1
          y' = fromPolynomial $ scale (fromIntegral (y :: Integer)) 1 :: Function IndexedPolynomial
       in fromRational (fromIntegral x / fromIntegral y) `shouldBe` x' / y'

  describe "fromPolynomial" $ do
    prop "is inverse of toPolynomial" $ \p ->
      toPolynomial (fromPolynomial p) `shouldBe` Just (p :: IndexedPolynomial)

  describe "toPolynomial" $ do
    prop "is nothing with zero denominator" $ \x ->
      toPolynomial (fromPolynomials x (0 :: IndexedPolynomial)) `shouldBe` Nothing
