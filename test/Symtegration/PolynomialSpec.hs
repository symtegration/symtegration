-- |
-- Description: Tests Symtegration.Polynomial.
-- Maintainer: dev@chungyc.org
module Symtegration.PolynomialSpec (spec) where

import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
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
