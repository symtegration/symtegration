-- |
-- Description: Tests Symtegration.Poynomial.Solve.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.SolveSpec (spec) where

import Data.List (sort)
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

      prop "finds root" $ \(NonZero a) x ->
        let p = scale a 1 * (power 1 - scale x 1)
         in counterexample (show p) $
              solve p `shouldBe` Just [fromRational x]

    describe "quadratic polynomials" $ do
      prop "found roots are roots" $ \(NonZero a) b c ->
        let p = scale a (power 2) + scale b (power 1) + scale c (power 0)
         in counterexample (show p) $
              solve p `shouldSatisfy` areRoots p

      prop "finds roots" $ \(NonZero a) x y ->
        let p = scale a 1 * (power 1 - scale x 1) * (power 1 - scale y 1)
         in counterexample (show p) $
              if x == y
                then toFiniteDoubleRoots (solve p) `shouldBe` Just (toFiniteDoubles [x])
                else toFiniteDoubleRoots (solve p) `shouldBe` Just (toFiniteDoubles [x, y])

      prop "does not find real roots" $ \(NonZero a) b c ->
        let p = scale a (power 2) + scale b (power 1) + scale c 1
            sq = b * b - 4 * a * c
         in sq < 0 ==> solve p `shouldBe` Just []

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

-- | Evaluate an expression into a floating-point value for comparisons.
eval :: Expression -> Near
eval e
  | (Just x) <- evaluate e (const Nothing) = Near x
  | otherwise = Near $ 0 / 0 -- not a number

-- | Convert a potential list of polynomial root solutions into floating-point values for comparisons.
toFiniteDoubleRoots :: Maybe [Expression] -> Maybe [Near]
toFiniteDoubleRoots = fmap (sort . map eval)

-- | Convert a list of rational numbers into floating-point values for comparisons.
toFiniteDoubles :: [Rational] -> [Near]
toFiniteDoubles = sort . map (Near . fromRational)
