-- |
-- Description: Tests Symtegration.Polynomial.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.PolynomialSpec (spec) where

import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = parallel $ do
  describe "monic" $ do
    prop "is zero for zero" $
      monic 0 `shouldBe` (0 :: IndexedPolynomial)

    prop "has leading coefficient of one" $ \p ->
      p /= 0 ==> leadingCoefficient (monic p :: IndexedPolynomial) `shouldBe` 1

    prop "is rational multiple of original polynomial" $ \p ->
      let p' = monic p :: IndexedPolynomial
          (q, r) = p `divide` p'
       in counterexample (show p') $
            conjoin [r `shouldBe` 0, degree q `shouldBe` 0]

  describe "polynomial algorithms" $ do
    describe "division" $ do
      prop "matches multiplication" $ \a b ->
        degree b > 0 ==>
          let (q, r) = divide a b
           in b * q + r `shouldBe` (a :: IndexedPolynomial)

      prop "remainder has smaller degree than divisor" $ \a b ->
        degree b > 0 ==>
          let (_, r) = divide a b
           in degree r `shouldSatisfy` (< degree (b :: IndexedPolynomial))

    describe "extended Euclidean algorithm" $ do
      prop "gets common divisor" $ \a b ->
        degree a > 0 && degree b > 0 ==>
          let (_, _, g :: IndexedPolynomial) = extendedEuclidean a b
           in conjoin (map (\p -> let (_, r) = divide p g in r `shouldBe` 0) [a, b])

      prop "coefficients generate greatest common divisor" $ \a b ->
        degree a > 0 && degree b > 0 ==>
          let (s, t, g :: IndexedPolynomial) = extendedEuclidean a b
           in s * a + t * b `shouldBe` g

      prop "any sa+tb must be multiple of gcd a b" $ \a b s t ->
        degree a > 0 && degree b > 0 ==>
          let (_, _, g :: IndexedPolynomial) = extendedEuclidean a b
           in snd (divide (s * a + t * b) g) `shouldBe` 0

    describe "greatest common divisor" $ do
      prop "is consistent with extended Euclidean algorithm" $ \a b ->
        let (_, _, g :: IndexedPolynomial) = extendedEuclidean a b
         in greatestCommonDivisor a b `shouldBe` g

    describe "differentiation" $ do
      prop "computes derivative of constant" $ \c ->
        differentiate (scale c (power 0) :: IndexedPolynomial) `shouldBe` 0

      prop "computes derivative of integral power" $ \(Positive e) c ->
        differentiate (scale c (power e) :: IndexedPolynomial)
          `shouldBe` scale (fromIntegral e * c) (power (e - 1))

      prop "compute derivative of compound polynomials" $ \a b ->
        differentiate (a + b :: IndexedPolynomial)
          `shouldBe` differentiate a + differentiate b

    describe "squarefree factorization" $ do
      prop "divides polynomial" $ \p ->
        let qs = squarefree p :: [IndexedPolynomial]
         in counterexample (show qs) $
              conjoin $
                map (\q -> counterexample (show q) $ snd (p `divide` q) === 0) qs

      prop "multiplies to polynomial" $ \p ->
        let qs = squarefree p :: [IndexedPolynomial]
            prod :: Int -> [IndexedPolynomial] -> IndexedPolynomial
            prod _ [] = 1
            prod k (x : xs) = x ^ k * prod (k + 1) xs
         in counterexample (show qs) $
              prod 1 qs `shouldBe` p
