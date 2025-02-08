-- |
-- Description: Tests Symtegration.Polynomial.
-- Copyright: Copyright 2025 Yoo Chung
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

    prop "has leading coefficient of one" $ \(NonZero p) ->
      leadingCoefficient (monic p :: IndexedPolynomial) `shouldBe` 1

    prop "is rational multiple of original polynomial" $ \p ->
      let p' = monic p :: IndexedPolynomial
          (q, r) = p `divide` p'
       in counterexample (show p') $
            conjoin [r `shouldBe` 0, degree q `shouldBe` 0]

  describe "mapCoefficients" $ do
    prop "scales" $ \(NonZero p) (NonZero x) ->
      let q = mapCoefficients (* x) p :: IndexedPolynomial
       in conjoin
            [ monic p === monic q,
              leadingCoefficient p * x === leadingCoefficient q
            ]

  describe "mapCoefficientsM" $ do
    prop "with Maybe" $ \p (Fun _ f) ->
      let q = mapCoefficientsM (f :: Rational -> Maybe Rational) (p :: IndexedPolynomial)
          p' = filter (\(_, c) -> c /= 0) <$> mapM (\(e, c) -> (e,) <$> f c) (toList p)
          toList = foldTerms (\e c -> [(e, c)])
       in toList <$> q `shouldBe` p'

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

    describe "pseudo-division" $ do
      prop "matches division for integer coefficients" $ \a (NonZero b) ->
        let delta = max (-1) (degree a - degree b)
            x = leadingCoefficient b ^ (1 + delta)
         in pseudoDivide a b `shouldBe` divide (scale x a) (b :: IndexedPolynomial)

    describe "extended Euclidean algorithm" $ do
      prop "gets common divisor" $ \a b ->
        let (_, _, g :: IndexedPolynomial) = extendedEuclidean a b
         in conjoin (map (\p -> let (_, r) = divide p g in r `shouldBe` 0) [a, b])

      prop "coefficients generate greatest common divisor" $ \a b ->
        let (s, t, g :: IndexedPolynomial) = extendedEuclidean a b
         in s * a + t * b `shouldBe` g

      prop "any sa+tb must be multiple of gcd a b" $ \a b s t ->
        let (_, _, g :: IndexedPolynomial) = extendedEuclidean a b
         in snd (divide (s * a + t * b) g) `shouldBe` 0

    describe "diophantine extended Euclidean algorithm" $ do
      prop "solves for (s,t)" $ \a b c ->
        degree a > 0 && degree b > 0 && degree c > 0 ==>
          let p Nothing =
                label "no solution" $
                  snd (c `divide` greatestCommonDivisor a b) `shouldSatisfy` (/= 0)
              p (Just (s, t)) =
                label "solved" $
                  counterexample ("(s,t) = " <> show (s, t)) $
                    conjoin
                      [ s * a + t * b === (c :: IndexedPolynomial),
                        disjoin [s === 0, property $ degree s < degree b]
                      ]
           in p (diophantineEuclidean a b c)

    describe "greatest common divisor" $ do
      prop "is consistent with extended Euclidean algorithm" $ \a b ->
        let (_, _, g :: IndexedPolynomial) = extendedEuclidean a b
         in greatestCommonDivisor a b `shouldBe` g

    describe "subresultant polynomial remainder sequence" $ do
      prop "resultant is zero iff gcd has non-zero degree" $ \a (NonZero b) ->
        let (resultant, _) = subresultant a (b :: IndexedPolynomial)
         in resultant == 0 `shouldBe` degree (greatestCommonDivisor a b) > 0

      modifyMaxSize (const 25) $
        prop "resultant has expected value" $
          forAll (arbitrarySizedFractional `suchThat` (/= 0)) $ \a ->
            forAll (arbitrarySizedFractional `suchThat` (/= 0)) $ \b ->
              forAll (listOf1 arbitrarySizedFractional) $ \as ->
                forAll (listOf1 arbitrarySizedFractional) $ \bs ->
                  let x = scale a $ product [power 1 - scale t 1 | t <- as] :: IndexedPolynomial
                      y = scale b $ product [power 1 - scale t 1 | t <- bs] :: IndexedPolynomial
                      r@(resultant, _) = subresultant x y
                      resultant' = a ^ length bs * b ^ length as * product [u - v | u <- as, v <- bs]
                   in counterexample (show r) $
                        resultant `shouldBe` resultant'

      prop "is polynomial remainder sequence" $ \a b ->
        let (_, prs) = subresultant a b
            -- Whether z is a numeric multiple of prem(x, y).
            fromPseudoRemainder (x, y, z)
              | y == 0 = z == 0
              | prem == 0 = z == 0
              | otherwise = degree q == 0 && r == 0
              where
                (_, prem) = pseudoDivide x y
                (q, r) = divide z (prem :: IndexedPolynomial)
            isPolynomialRemainderSequence xs =
              all fromPseudoRemainder $ zip3 xs (drop 1 xs) (drop 2 xs)
         in prs `shouldSatisfy` isPolynomialRemainderSequence

      prop "has zero as last element in sequence" $ \a b ->
        let (_, prs) = subresultant a (b :: IndexedPolynomial)
         in counterexample (show prs) $
              drop (length prs - 1) prs `shouldBe` [0]

    describe "differentiation" $ do
      prop "computes derivative of constant" $ \c ->
        differentiate (scale c (power 0) :: IndexedPolynomial) `shouldBe` 0

      prop "computes derivative of integral power" $ \(Positive e) c ->
        differentiate (scale c (power e) :: IndexedPolynomial)
          `shouldBe` scale (fromIntegral e * c) (power (e - 1))

      prop "computes derivative of compound polynomials" $ \a b ->
        differentiate (a + b :: IndexedPolynomial)
          `shouldBe` differentiate a + differentiate b

    describe "integration" $ do
      prop "computes integral of integral power" $ \(NonNegative e) c ->
        integrate (scale c (power e) :: IndexedPolynomial)
          `shouldBe` scale (c / (1 + fromIntegral e)) (power (e + 1))

      prop "computes integral of compound polynomials" $ \a b ->
        integrate (a + b :: IndexedPolynomial)
          `shouldBe` integrate a + integrate b

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

  describe "with symbolic coefficients" $ modifyMaxSize (`div` 10) $ do
    prop "divide is total" $ \a (NonZero b) ->
      total $ divide a (b :: IndexedSymbolicPolynomial)

    prop "pseudoDivide is total" $ \a (NonZero b) ->
      total $ pseudoDivide a (b :: IndexedSymbolicPolynomial)

    prop "extendedEuclidean is total" $ \a b ->
      total $ extendedEuclidean a (b :: IndexedSymbolicPolynomial)

    prop "diophantineEuclidean is total" $ \a b c ->
      total $ diophantineEuclidean a b (c :: IndexedSymbolicPolynomial)

    prop "greatestCommonDivisor is total" $ \a b ->
      total $ greatestCommonDivisor a (b :: IndexedSymbolicPolynomial)

    prop "subresultant is total" $ \a b ->
      total $ subresultant a (b :: IndexedSymbolicPolynomial)

    prop "differentiate is total" $ \p ->
      total $ differentiate (p :: IndexedSymbolicPolynomial)

    prop "integrate is total" $ \p ->
      total $ integrate (p :: IndexedSymbolicPolynomial)
