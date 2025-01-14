-- |
-- Description: Tests Symtegration.Poynomial.Solve.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.SolveSpec (spec) where

import Data.List (nub, sort)
import Symtegration.FiniteDouble
import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Solve
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = parallel $ do
  describe "solve" $ do
    describe "linear polynomials" $ do
      prop "found roots are roots" $ \(NonZero a) b ->
        let p = scale a (power 1) + scale b (power 0)
         in correctlySolves p

      prop "finds root" $ \(NonZero a) x ->
        let p = scale a 1 * (power 1 - scale x 1)
         in counterexample (show p) $
              solve p `shouldBe` Just [fromRational x]

    describe "quadratic polynomials" $ do
      prop "found roots are roots" $ \(NonZero a) b c ->
        let p = scale a (power 2) + scale b (power 1) + scale c (power 0)
         in correctlySolves p

      prop "finds all roots" $ \(NonZero a) x y ->
        let p = scale a 1 * (power 1 - scale x 1) * (power 1 - scale y 1)
         in counterexample (show p) $
              if x == y
                then toFiniteDoubleRoots (solve p) `shouldBe` Just (toFiniteDoubles [x])
                else toFiniteDoubleRoots (solve p) `shouldBe` Just (toFiniteDoubles [x, y])

      prop "does not find real roots" $ \(NonZero a) b c ->
        let p = scale a (power 2) + scale b (power 1) + scale c 1
            sq = b * b - 4 * a * c
         in sq < 0 ==> solve p `shouldBe` Just []

    describe "cubic polynomials" $ do
      modifyMaxSuccess (* 10) $
        prop "found roots are roots" $ \(NonZero a) b c d ->
          let p = scale a (power 3) + scale b (power 2) + scale c (power 1) + scale d 1
           in correctlySolves p

      prop "with zero lower order terms" $ \(NonZero a) ->
        let p = scale a (power 3)
         in correctlySolves p

      prop "with zero discriminant" $ \u ->
        let p = -(3 * u * u)
            q = 2 * u * u * u
            r = power 3 + scale p (power 1) + scale q 1
         in conjoin
              [ counterexample ("p = " <> show p <> ", q = " <> show q) $
                  4 * p * p * p + 27 * q * q === 0,
                correctlySolves r
              ]

      modifyMaxSuccess (* 100) $
        prop "finds roots" $ \(NonZero a) x y z ->
          let p = scale a 1 * (power 1 - scale x 1) * (power 1 - scale y 1) * (power 1 - scale z 1)
              roots = nub [x, y, z]
           in counterexample (show p) $
                toFiniteDoubleRoots (solve p) `shouldBe` Just (toFiniteDoubles roots)

    describe "quartic polynomials" $ do
      modifyMaxSuccess (* 10) $
        prop "found roots are roots" $ \(NonZero a) b c d e ->
          let p = scale a (power 4) + scale b (power 3) + scale c (power 2) + scale d (power 1) + scale e 1
           in correctlySolves p

      describe "special cases" $ do
        prop "ax^4 + bx^3 = 0" $ \(NonZero a) b ->
          let p = scale a (power 4) + scale b (power 3)
           in correctlySolves p

        prop "ax^4 + bx^3 + cx^2 = 0" $ \(NonZero a) b c ->
          let p = scale a (power 4) + scale b (power 3) + scale c (power 2)
           in correctlySolves p

        prop "ax^4 + bx^3 + cx^2 + dx = 0" $ \(NonZero a) b c d ->
          let p = scale a (power 4) + scale b (power 3) + scale c (power 2) + scale d (power 1)
           in correctlySolves p

        prop "ax^4 + b = 0" $ \(NonZero a) b ->
          let p = scale a (power 4) + scale b 1
           in correctlySolves p

        modifyMaxSuccess (* 10) $
          prop "ax^4 + bx^2 + c = 0" $ \(NonZero a) b c ->
            let p = scale a (power 4) + scale b (power 2) + scale c 1
             in correctlySolves p

      modifyMaxSuccess (* 100) $
        prop "finds all real roots when any found" $ \(NonZero a) x y z w ->
          let p = scale a $ product [power 1 - scale v 1 | v <- [x, y, z, w]]
              roots = nub [x, y, z, w]
           in counterexample (show p) $
                case solve p of
                  Nothing -> label "not solved" True
                  xs@(Just _) ->
                    label "solved" $
                      toFiniteDoubleRoots xs `shouldBe` Just (toFiniteDoubles roots)

-- | Passes if either all the roots found are indeed roots of the polynomial
-- or solutions could not be derived.
correctlySolves :: IndexedPolynomial -> Property
correctlySolves p =
  counterexample (show p) $
    counterexample (show $ map (toHaskell . simplify) <$> roots) $
      label (case roots of Nothing -> "not solved"; Just _ -> "solved") $
        roots `shouldSatisfy` areRoots p
  where
    roots = solve p

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
