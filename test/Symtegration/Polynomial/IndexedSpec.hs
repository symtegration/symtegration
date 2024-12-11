-- |
-- Description: Tests Symtegration.Poynomial.Indexed.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.IndexedSpec (spec) where

import Data.List (dropWhileEnd)
import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = parallel $ describe "IndexedPolynomial" $ do
  prop "fromInteger" $ \n ->
    let p = fromInteger n :: IndexedPolynomial
     in conjoin
          [ degree p `shouldBe` 0,
            coefficient p 0 `shouldBe` fromInteger n,
            leadingCoefficient p `shouldBe` fromInteger n
          ]

  prop "from power" $ \n ->
    let p = power n :: IndexedPolynomial
     in conjoin
          [ degree p `shouldBe` n,
            coefficient p n `shouldBe` 1,
            leadingCoefficient p `shouldBe` 1,
            [coefficient p k | k <- [0 .. degree p - 1]] `shouldSatisfy` all (== 0)
          ]

  prop "from series of powers" $ \cs ->
    let cs' = dropWhileEnd (== 0) cs
        p = foldl accumulate 0 (zip [0 ..] cs') :: IndexedPolynomial
        accumulate p' (e, c) = p' + scale c (power e)
     in not (null cs') ==> getCoefficients p `shouldBe` cs'

  prop "leading coefficients match" $ \p ->
    leadingCoefficient p `shouldBe` coefficient (p :: IndexedPolynomial) (degree p)

  describe "addition" $ do
    prop "adds numbers" $ \m n ->
      fromInteger m + fromInteger n `shouldBe` (fromInteger (m + n) :: IndexedPolynomial)

    prop "adds number and polynomial" $ \m p c ->
      let leadingTerm = scale c (power $ 1 + degree p)
          p' = p + leadingTerm :: IndexedPolynomial
       in fromInteger m + p' `shouldBe` (fromInteger m + p) + leadingTerm

    prop "adds polynomials" $ \p q c ->
      let leadingTerm = scale c (power $ 1 + degree p)
          p' = p + leadingTerm :: IndexedPolynomial
       in p' + q `shouldBe` (p + q) + leadingTerm

  describe "multiplication" $ do
    prop "multiplies numbers" $ \m n ->
      fromInteger m * fromInteger n `shouldBe` (fromInteger (m * n) :: IndexedPolynomial)

    prop "multiplies number and polynomial" $ \m p c ->
      let leadingTerm = scale c (power $ 1 + degree p)
          p' = p + leadingTerm :: IndexedPolynomial
       in fromInteger m * p' `shouldBe` (fromInteger m * p) + fromInteger m * leadingTerm

    prop "multiplies polynomials" $ \p q c ->
      let leadingTerm = scale c (power $ 1 + degree p)
          p' = p + leadingTerm :: IndexedPolynomial
       in p' * q `shouldBe` (p * q) + (leadingTerm * q)

  describe "subtraction" $ do
    prop "is same as adding negation" $ \p q ->
      let q' = negate q :: IndexedPolynomial
       in p - q `shouldBe` p + q'

  describe "negate" $ do
    prop "negates coefficients" $ \p ->
      getCoefficients (negate p) `shouldBe` map negate (getCoefficients p)

-- | Returns the coefficients of the given polynomial, in ascending order of the power.
getCoefficients :: IndexedPolynomial -> [Rational]
getCoefficients p = [coefficient p k | k <- [0 .. degree p]]
