-- |
-- Description: Tests of Symtegration.Integration.Monomial.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.MonomialSpec where

import Symtegration.Integration.Monomial
import Symtegration.Polynomial
import Symtegration.Polynomial.Differential
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Symtegration.Polynomial.Rational
import Symtegration.Polynomial.Rational.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = parallel $ do
  hermiteReduceSpec
  polynomialReduceSpec

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
