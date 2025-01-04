-- |
-- Description: Tests Symtegration.Poynomial.Solve.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.SolveSpec (spec) where

import Symtegration.FiniteDouble
import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Solve
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = parallel $ do
  describe "solve" $ do
    describe "linear polynomials" $ do
      prop "found roots are roots" $ \(NonZero a) b ->
        let p = scale a (power 1) + scale b (power 0)
         in counterexample (show p) $
              solve p `shouldSatisfy` areRoots p

    describe "quadratic polynomials" $ do
      prop "found roots are roots" $ \(NonZero a) b c ->
        let p = scale a (power 2) + scale b (power 1) + scale c (power 0)
         in counterexample (show p) $
              solve p `shouldSatisfy` areRoots p

-- | Whether x is a root of p.
isRoot :: IndexedPolynomial -> Expression -> Bool
isRoot p x
  | (Just x') <- evaluate x (const Nothing) = Near (f x') == Near 0
  | otherwise = False
  where
    p' = toExpression "x" toRationalCoefficient p
    f = toFunction p' (\case "x" -> id; _ -> undefined)

-- | Whether the given roots are indeed roots of the given polynomial,
-- or if roots could not be found.
areRoots :: IndexedPolynomial -> Maybe [Expression] -> Bool
areRoots _ Nothing = True
areRoots p (Just xs) = all (isRoot p) xs
