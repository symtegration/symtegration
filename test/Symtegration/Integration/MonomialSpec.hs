-- |
-- Description: Tests of Symtegration.Integration.Monomial.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.MonomialSpec where

import Data.Bifunctor (first)
import Symtegration.Integration.Monomial
import Symtegration.Integration.Rational (rationalIntegralLogTerms)
import Symtegration.Polynomial
import Symtegration.Polynomial.Differential
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Symtegration.Polynomial.Rational
import Symtegration.Polynomial.Rational.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Function, scale)

spec :: Spec
spec = parallel $ do
  hermiteReduceSpec
  polynomialReduceSpec
  residueReduceSpec

hermiteReduceSpec :: Spec
hermiteReduceSpec = describe "hermiteReduce" $ do
  prop "adds back to original function" $ \(D d _) f ->
    forReduction (hermiteReduce d f) $ \(gs, h, r) ->
      let dg = sum [fromPolynomials (gd * d gn - gn * d gd) (gd * gd) | Function gn gd <- gs]
       in dg + h + r `shouldBe` f

  prop "denominators divide denominator of original function" $ \(D d _) p (NonZero q) ->
    forReduction (hermiteReduce d $ fromPolynomials p q) $
      \(gs, Function _ s, Function _ v) -> do
        [gd | Function _ gd <- gs] `shouldSatisfy` all (`divides` q)
        s `shouldSatisfy` (`divides` q)
        v `shouldSatisfy` (`divides` q)

  prop "denominator for h is simple" $ \(D d _) f ->
    forReduction (hermiteReduce d f) $ \(_, Function _ s, _) -> do
      degree (greatestCommonDivisor s $ d s) `shouldBe` 0

  prop "denominator for r is reduced" $ \(D d _) f ->
    forReduction (hermiteReduce d f) $ \(_, _, Function _ v) -> do
      monic (greatestCommonDivisor v $ d v) `shouldBe` monic v
  where
    forReduction (g, h, r) f = labels $ counterexamples $ f (g, h, r)
      where
        counterexamples =
          counterexample ("g = " <> show g)
            . counterexample ("h = " <> show h)
            . counterexample ("r = " <> show r)
        labels = label deriv . label simple . label reduced
        deriv | [] <- g = "zero derivative" | otherwise = "non-zero derivative"
        simple | 0 <- h = "zero simple" | otherwise = "non-zero simple"
        reduced | 0 <- r = "zero reduced" | otherwise = "non-zero reduced"

    divides q p = case p `divide` q of
      (_, 0) -> True
      _ -> False

polynomialReduceSpec :: Spec
polynomialReduceSpec = describe "polynomialReduce" $ do
  prop "adds back to original function" $ \(D d _) p ->
    let (q, r) = polynomialReduce d p
     in counterexample (show (q, r)) $ d q + r `shouldBe` p

  prop "degree r < D-degree" $ \(D d _) p ->
    let (q, r) = polynomialReduce d p
        delta = degree $ d $ power 1
     in counterexample (show (q, r)) $
          degree r `shouldSatisfy` (\x -> x < delta || delta == 0)

residueReduceSpec :: Spec
residueReduceSpec = describe "residueReduce" $ do
  modifyMaxSize (`div` 5) $
    prop "equivalent to resultant reduction for rational functions" $ \a d ->
      (d /= 0 && degree a < degree d && degree (greatestCommonDivisor a d) == 0) ==>
        fmap (first (map termMonic)) (residueReduce differentiate $ fromPolynomials a d)
          `shouldBe` fmap ((,True) . map termMonic) (rationalIntegralLogTerms $ fromPolynomials a d)

  -- What we would really like to test for is that Just (g, True) == residueReduce d f
  -- if and only if f - Dg is a polynomial in the field comprising the coefficients of the monomial.
  -- Unfortunately, we do not have sophisticated enough support for computer algebra to test this,
  -- which involves solutions of polynomial equations and comparisons of algebraic numbers in general.
  -- For this reason, we test with particular examples of simple functions which are known
  -- to either have or not have elementary integrals.

  -- For simple functions whose numerator has smaller degree than denominator.
  describe "examples which do have elementary integrals" $
    let hasElementary ::
          (IndexedPolynomialWith (Function IndexedPolynomial) -> IndexedPolynomialWith (Function IndexedPolynomial)) ->
          Function (IndexedPolynomialWith (Function IndexedPolynomial)) ->
          Expectation
        hasElementary deriv f@(Function a d)
          | degree a >= degree d = error "bad example: degree of numerator not smaller than denominator"
          | degree (greatestCommonDivisor d $ deriv d) > 0 = error "bad example: not a simple function"
          | otherwise = fmap snd (residueReduce deriv f) `shouldBe` Just True

        -- Plain differentiation of rational functions.
        diff :: Function IndexedPolynomial -> Function IndexedPolynomial
        diff (Function y x) = fromPolynomials (x * differentiate y - y * differentiate x) (x * x)
     in do
          it "for 1 / tan x" $
            let deriv = extend diff $ power 2 + 1 -- Dt = t^2+1
                f = fromPolynomials 1 $ power 1 -- f = 1 / tan x
             in hasElementary deriv f

          it "for 1 / (tan x + 1)" $
            let deriv = extend diff $ power 2 + 1 -- Dt = t^2+1
                f = fromPolynomials 1 $ power 1 + 1 -- f = 1 / (tan x + 1)
             in hasElementary deriv f

          it "for (tan x ^ 2 + 1) / (tan x ^ 5 + 1)" $
            let deriv = extend diff $ power 2 + 1 -- Dt = t^2+1
                f = fromPolynomials (power 2 + 1) (power 5 + 1) -- f = (tan x ^ 2 + 1) / (tan x ^ 5 + 1)
             in hasElementary deriv f

  -- For simple functions whose numerator has smaller degree than denominator.
  describe "examples which do not have elementary integrals" $
    let hasNoElementary ::
          (IndexedPolynomialWith (Function IndexedPolynomial) -> IndexedPolynomialWith (Function IndexedPolynomial)) ->
          Function (IndexedPolynomialWith (Function IndexedPolynomial)) ->
          Expectation
        hasNoElementary deriv f@(Function a d)
          | degree a >= degree d = error "bad example: degree of numerator not smaller than denominator"
          | degree (greatestCommonDivisor d $ deriv d) > 0 = error "bad example: not a simple function"
          | otherwise = fmap snd (residueReduce deriv f) `shouldBe` Just False

        -- Plain differentiation of rational functions.
        diff :: Function IndexedPolynomial -> Function IndexedPolynomial
        diff (Function y x) = fromPolynomials (x * differentiate y - y * differentiate x) (x * x)
     in do
          it "for 1 / log x" $
            let deriv = extend diff $ scale (fromPolynomials 1 $ power 1) 1 -- Dt = 1/x
                f = fromPolynomials 1 $ power 1 -- f = 1 / log x
             in hasNoElementary deriv f

          it "for x / log x" $
            let deriv = extend diff $ scale (fromPolynomials 1 $ power 1) 1 -- Dt = 1/x
                f = fromPolynomials (scale (fromPolynomial $ power 1) 1) (power 1) -- f = x / log x
             in hasNoElementary deriv f

          it "for 1 / (log x + x)" $
            let deriv = extend diff $ scale (fromPolynomials 1 $ power 1) 1 -- Dt = 1/x
                f = fromPolynomials 1 $ power 1 + scale (fromPolynomial $ power 1) 1 -- f = 1 / (log x + x)
             in hasNoElementary deriv f

          it "for 1 / (tan x + x)" $
            let deriv = extend diff $ power 2 + 1 -- Dt = 2t+1
                f = fromPolynomials 1 $ power 1 + scale (fromPolynomial $ power 1) 1 -- f = 1 / (tan x + x)
             in hasNoElementary deriv f

          it "for x / tan x" $
            let deriv = extend diff $ power 2 + 1 -- Dt = 2t+1
                f = fromPolynomials (scale (fromPolynomial $ power 1) 1) (power 1) -- f = x / tan x
             in hasNoElementary deriv f

          it "for 1 / (e ^ x + x)" $
            let deriv = extend diff $ power 1 -- Dt = t
                f = fromPolynomials 1 $ power 1 + scale (fromPolynomial $ power 1) 1 -- f = 1 / (e ^ x + x)
             in hasNoElementary deriv f
  where
    -- Make the leading coefficients monic.
    -- It does not change the roots for a polynomial equation,
    -- and the constant multiplicative factor in a logarithmic argument
    -- only adds an additive constant factor to an integral.
    termMonic (p, q) = (monic p, nestedMonic q)

    nestedMonic p = mapCoefficients (scale (1 / c)) p
      where
        c = leadingCoefficient $ leadingCoefficient p

-- | Generate an arbitrary derivation for a polynomial with rational number coefficients.
-- The string is a description of the derivation.
data D = D (IndexedPolynomial -> IndexedPolynomial) String

instance Show D where
  show (D _ desc) = show desc

instance Arbitrary D where
  arbitrary = oneof [zeroExtension, diff]
    where
      zeroExtension = do
        anchor <- arbitrary
        let extension = extend (const 0) anchor
        let description = "zero extension with Dx = " <> show anchor
        return $ D extension description

      diff = pure $ D differentiate "differentiation"
