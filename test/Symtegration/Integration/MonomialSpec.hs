-- |
-- Description: Tests of Symtegration.Integration.Monomial.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.MonomialSpec where

import Symtegration.Integration.Monomial
import Symtegration.Integration.Rational (toRationalFunction)
import Symtegration.Polynomial
import Symtegration.Polynomial.Differential
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = parallel $ do
  describe "hermiteReduce" $ do
    prop "adds back to derivative of original function" $ \(D d _) p (NonZero q) ->
      forReduction (hermiteReduce d p q) $ \(gs, (r, s), (f, (u, v))) ->
        let dg' = sum [toRationalFunction (gd * d gn - gn * d gd) (gd * gd) | (gn, gd) <- gs]
            h' = toRationalFunction r s
            r' = toRationalFunction f 1 + toRationalFunction u v
         in dg' + h' + r' `shouldBe` toRationalFunction p q

    prop "denominators divide denominator of original function" $ \(D d _) p (NonZero q) ->
      forReduction (hermiteReduce d p q) $ \(gs, (_, s), (_, (_, v))) -> do
        [gd | (_, gd) <- gs] `shouldSatisfy` all (`divides` q)
        s `shouldSatisfy` (`divides` q)
        v `shouldSatisfy` (`divides` q)

    prop "denominator for h is simple" $ \(D d _) p (NonZero q) ->
      forReduction (hermiteReduce d p q) $ \(_, (_, s), (_, (_, _))) -> do
        degree (greatestCommonDivisor s $ d s) `shouldBe` 0

    prop "denominator for r is reduced" $ \(D d _) p (NonZero q) ->
      forReduction (hermiteReduce d p q) $ \(_, (_, _), (_, (_, v))) -> do
        monic (greatestCommonDivisor v $ d v) `shouldBe` monic v
  where
    forReduction (g, h, r@(rp, rr)) f = labels $ counterexamples $ f (g, h, r)
      where
        counterexamples =
          counterexample ("g = " <> show g)
            . counterexample ("h = " <> show h)
            . counterexample ("r = " <> show (rp, rr))
        labels = label deriv . label simple . label reduced
        deriv | [] <- g = "zero derivative" | otherwise = "non-zero derivative"
        simple | (0, _) <- h = "zero simple" | otherwise = "non-zero simple"
        reduced | (0, (0, _)) <- r = "zero reduced" | otherwise = "non-zero reduced"

    divides q p = case p `divide` q of
      (_, 0) -> True
      _ -> False

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
