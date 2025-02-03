-- |
-- Description: Tests Symtegration.Poynomial.Solve.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.SolveSpec (spec) where

import Data.Complex (realPart)
import Data.List (nub, sort, sortOn)
import Data.Monoid (Sum (..))
import Symtegration.Approximate
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
                then toDoubleRoots (solve p) `shouldBe` Just (toDoubles [x])
                else toDoubleRoots (solve p) `shouldBe` Just (toDoubles [x, y])

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
                toDoubleRoots (solve p) `shouldBe` Just (toDoubles roots)

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

      modifyMaxSuccess (* 1000) $
        prop "finds all real roots when any found" $ \(NonZero a) x y z w ->
          let p = scale a $ product [power 1 - scale v 1 | v <- [x, y, z, w]]
              roots = nub [x, y, z, w]
           in counterexample (show p) $
                case solve p of
                  Nothing -> label "not solved" True
                  xs@(Just _) ->
                    label "solved" $
                      toDoubleRoots xs `shouldBe` Just (toDoubles roots)

  describe "complexSolve" $ do
    describe "linear polynomials" $ do
      prop "finds root" $ \(NonZero a) x ->
        let p = scale a 1 * (power 1 - scale x 1)
         in counterexample (show p) $
              complexSolve p `shouldBe` Just [fromRational x]

    describe "quadratic polynomials" $ do
      prop "finds real solutions" $ \(NonZero a) b c ->
        let p = scale a (power 2) + scale b (power 1) + scale c 1
         in filter isFinite <$> toDoubleRoots (complexSolve p)
              `shouldBe` toDoubleRoots (solve p)

    describe "cubic polynomials" $ do
      modifyMaxSuccess (* 100) $
        prop "finds roots" $ \(NonZero a) b c d ->
          let p = scale a (power 3) + scale b (power 2) + scale c (power 1) + scale d 1
           in consistentWithComplexRoots p (complexSolve p)

      describe "special cases" $ do
        prop "ax^3 = 0" $ \(NonZero a) ->
          let p = scale a (power 3)
           in consistentWithComplexRoots p (complexSolve p)

        prop "ax^3 + bx^2 = 0" $ \(NonZero a) (NonZero b) ->
          let p = scale a (power 3) + scale b (power 2)
           in consistentWithComplexRoots p (complexSolve p)

        prop "ax^3 + bx^2 + cx= 0" $ \(NonZero a) (NonZero b) (NonZero c) ->
          let p = scale a (power 3) + scale b (power 2) + scale c (power 1)
           in consistentWithComplexRoots p (complexSolve p)

    describe "quartic polynomials" $ do
      modifyMaxSuccess (* 100) $
        prop "finds roots" $ \(NonZero a) b c d e ->
          let p = scale a (power 4) + scale b (power 3) + scale c (power 2) + scale d (power 1) + scale e 1
           in consistentWithComplexRoots p (complexSolve p)

      describe "special cases" $ do
        prop "ax^4 = 0" $ \(NonZero a) ->
          let p = scale a (power 4)
           in consistentWithComplexRoots p (complexSolve p)

        prop "ax^4 + bx^3 = 0" $ \(NonZero a) (NonZero b) ->
          let p = scale a (power 4) + scale b (power 3)
           in consistentWithComplexRoots p (complexSolve p)

        prop "ax^4 + bx^3 + cx^2 = 0" $ \(NonZero a) (NonZero b) (NonZero c) ->
          let p = scale a (power 4) + scale b (power 3) + scale c (power 2)
           in consistentWithComplexRoots p (complexSolve p)

        prop "ax^4 + bx^3 + cx^2 + dx = 0" $ \(NonZero a) (NonZero b) (NonZero c) (NonZero d) ->
          let p = scale a (power 4) + scale b (power 3) + scale c (power 2) + scale d (power 1)
           in consistentWithComplexRoots p (complexSolve p)

        prop "ax^4 + bx^2 + c = 0" $ \(NonZero a) b c ->
          let p = scale a (power 4) + scale b (power 2) + scale c (power 0)
           in consistentWithComplexRoots p (complexSolve p)

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
  | (Just x') <- evaluate x (const Nothing) = f x' == approximate 0
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
eval :: Expression -> Approximate
eval e
  | (Just x) <- evaluate e (const Nothing) = x
  | otherwise = 0 / 0 -- not a number

-- | Convert a potential list of polynomial root solutions into floating-point values for comparisons.
toDoubleRoots :: Maybe [Expression] -> Maybe [Approximate]
toDoubleRoots = fmap (sortOn (realPart . center) . map eval)

-- | Convert a list of rational numbers into floating-point values for comparisons.
toDoubles :: [Rational] -> [Approximate]
toDoubles = map fromRational . sort

-- | Evaluate an expression to a concrete complex number.
complexEval :: Expression -> Approximate
complexEval expr
  | (Just x) <- evaluate expr (const Nothing) = x
  | otherwise = 0 / 0 -- not a number

-- | Evaluate a polynomial with a complex number substituted in the variable.
complexPolyEval :: IndexedPolynomial -> Approximate -> Approximate
complexPolyEval p x = getSum $ foldTerms (\e c -> Sum $ fromRational c * x ** fromIntegral e) p

-- | Check whether the given polynomial is consistent with the given solutions,
-- which may include complex numbers.
consistentWithComplexRoots :: IndexedPolynomial -> Maybe [Expression] -> Property
consistentWithComplexRoots p roots =
  label (rootsLabel roots) $
    counterexample (show roots') $
      map (complexPolyEval p) <$> roots' `shouldSatisfy` maybe True (all (== 0))
  where
    roots' = map complexEval <$> roots

    rootsLabel Nothing = "did not solve"
    rootsLabel (Just xs) = "root count = " <> show (length xs)
