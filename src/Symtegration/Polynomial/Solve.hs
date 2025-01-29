-- |
-- Module: Symtegration.Polynomial.Solve
-- Description: Derive the roots of polynomials with rational coefficients.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This module supports deriving exact solutions to polynomial equations.
-- It cannot derive solutions for all polynomials; it will only return those which it can.
module Symtegration.Polynomial.Solve (solve, complexSolve) where

import Data.List (nub)
import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration
-- >>> import Symtegration.Polynomial

-- | Derive the roots for the given polynomial.  Only real roots are returned.
--
-- >>> map (toHaskell . simplify) <$> solve (2 * power 1 - 6)
-- Just ["3"]
--
-- >>> map (toHaskell . simplify) <$> solve (power 2 - 4)
-- Just ["2","-2"]
--
-- Returns 'Nothing' if the function does not know how to derive the roots.
solve :: IndexedPolynomial -> Maybe [Expression]
solve p
  | degree p == 1 = solveLinear (c 1) (c 0)
  | degree p == 2 = solveQuadratic (c 2) (c 1) (c 0)
  | degree p == 3 = solveCubic (c 3) (c 2) (c 1) (c 0)
  | degree p == 4 = solveQuartic (c 4) (c 3) (c 2) (c 1) (c 0)
  | otherwise = Nothing
  where
    c = coefficient p

-- | Returns the real root for a polynomial of degree 1.
solveLinear :: Rational -> Rational -> Maybe [Expression]
solveLinear a b = Just [fromRational ((-b) / a)]

-- | Returns the real roots for a polynomial of degree 2.
solveQuadratic :: Rational -> Rational -> Rational -> Maybe [Expression]
solveQuadratic a b c
  | sq == 0 = Just [fromRational $ (-b) / (2 * a)]
  | sq > 0 =
      Just
        [ ((-b') + sq' ** (1 / 2)) / (2 * a'),
          ((-b') - sq' ** (1 / 2)) / (2 * a')
        ]
  | otherwise = Just []
  where
    sq = b * b - 4 * a * c
    sq' = fromRational sq
    a' = fromRational a
    b' = fromRational b

-- | Returns the real roots for a polynomial of degree 3.
solveCubic :: Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
solveCubic a b c d = map restore <$> depressedRoots
  where
    restore x = x - fromRational b / (3 * fromRational a)
    depressedRoots = solveDepressedCubic p q
    p = (3 * a * c - b ^ two) / (3 * a ^ two)
    q = (2 * b ^ three - 9 * a * b * c + 27 * a ^ two * d) / (27 * a ^ three)
    two = 2 :: Int
    three = 3 :: Int

-- | Solve depressed cubic equations of the form \(x^3 + px + q = 0\).
-- Only returns real roots.
--
-- #### References
--
-- * [Wikipedia](https://en.wikipedia.org/wiki/Cubic_equation#Trigonometric_and_hyperbolic_solutions)
-- * [Wolfram MathWorld](https://mathworld.wolfram.com/CubicFormula.html)
solveDepressedCubic :: Rational -> Rational -> Maybe [Expression]
solveDepressedCubic 0 q
  | q < 0 = Just [fromRational (-q) ** (1 / 3)]
  | otherwise = Just [negate $ fromRational q ** (1 / 3)]
solveDepressedCubic p q
  | s < 0 =
      let c = 2 * sqrt (-(p' / 3))
          theta = acos (3 / 2 * q' / p' * sqrt (-(3 / p'))) / 3
       in Just [c * cos theta, c * cos (theta - 2 * pi / 3), c * cos (theta - 4 * pi / 3)]
  | p < 0,
    s > 0 =
      Just [(-2) * signum q' * sqrt (-(p' / 3)) * cosh (acosh ((-3) / 2 * abs q' / p' * sqrt (-(3 / p'))) / 3)]
  | s == 0 = Just [fromRational (3 * q / p), fromRational ((-3) / 2 * q / p)]
  | p > 0 =
      Just [(-2) * sqrt (p' / 3) * sinh (asinh (3 / 2 * q' / p' * sqrt (3 / p')) / 3)]
  | otherwise = Nothing
  where
    s = 4 * p ^ (3 :: Int) + 27 * q ^ (2 :: Int)
    p' = fromRational p
    q' = fromRational q

-- | Returns the real roots for a polynomial of degree 4.
solveQuartic :: Rational -> Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
solveQuartic a b 0 0 0
  | b /= 0 = Just [0, fromRational $ -(b / a)]
  | otherwise = Just [0]
solveQuartic a b c 0 0
  | (Just xs) <- solveQuadratic a b c = Just $ nub $ 0 : xs
  | otherwise = Nothing
solveQuartic a b c d 0
  | (Just xs) <- solveCubic a b c d = Just $ nub $ 0 : xs
  | otherwise = Nothing
solveQuartic a 0 0 0 b
  | a > 0, b > 0 = Just []
  | a < 0, b < 0 = Just []
  | b == 0 = Just [0]
  | otherwise = Just [x, -x]
  where
    x = fromRational ((-b) / a) ** (1 / 4)
solveQuartic a 0 b 0 c
  | sq < 0 = Just []
  | sq == 0, st < 0 = Just []
  | sq == 0 = Just [sqrt st', -sqrt st']
  | a > 0, sq > 0, b > 0, sq > b * b = Just [sqrt x1, -sqrt x1]
  | a < 0, sq > 0, b < 0, sq > b * b = Just [sqrt x2, -sqrt x2]
  | a > 0, sq > 0, b < 0, sq < b * b = Just [sqrt x1, -sqrt x1, sqrt x2, -sqrt x2]
  | a < 0, sq > 0, b > 0, sq < b * b = Just [sqrt x1, -sqrt x1, sqrt x2, -sqrt x2]
  | otherwise = Nothing
  where
    sq = b * b - 4 * a * c
    st = (-b) / (2 * a)
    sq' = fromRational sq
    st' = fromRational st
    a' = fromRational a
    b' = fromRational b
    x1 = ((-b') + sqrt sq') / (2 * a')
    x2 = ((-b') - sqrt sq') / (2 * a')
solveQuartic _ _ _ _ _ = Nothing

-- | Derive the roots for the given polynomial.
-- All roots are returned, including complex roots.
--
-- >>> map (toHaskell . simplify) <$> complexSolve (2 * power 1 - 6)
-- Just ["3"]
--
-- >>> map (toHaskell . simplify) <$> complexSolve (scale (1/2) (power 2) + 1)
-- Just ["(-2) ** (1 / 2)","(-1) * (-2) ** (1 / 2)"]
--
-- >>> map (toHaskell . simplify) <$> complexSolve (power 3 - 1)
-- Just ["1","((-1) + (-3) ** (1 / 2)) / 2","((-1) + (-1) * (-3) ** (1 / 2)) / 2"]
--
-- Returns 'Nothing' if the function does not know how to derive the roots.
complexSolve :: IndexedPolynomial -> Maybe [Expression]
complexSolve p
  | degree p == 1 = complexSolveLinear (c 1) (c 0)
  | degree p == 2 = complexSolveQuadratic (c 2) (c 1) (c 0)
  | degree p == 3 = complexSolveCubic (c 3) (c 2) (c 1) (c 0)
  | degree p == 4 = complexSolveQuartic (c 4) (c 3) (c 2) (c 1) (c 0)
  | otherwise = Nothing
  where
    c = coefficient p

-- | Returns the roots for a polynomial of degree 1.
complexSolveLinear :: Rational -> Rational -> Maybe [Expression]
complexSolveLinear a b = Just [fromRational $ (-b) / a]

-- | Returns the roots for a polynomial of degree 2.
complexSolveQuadratic :: Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveQuadratic a b c
  | sq == 0 = Just [p]
  | otherwise = Just [p + q, p - q]
  where
    sq = b * b - 4 * a * c
    p = fromRational $ (-b) / (2 * a)
    q = sqrt (fromRational sq) / fromRational (2 * a)

-- | Returns the roots for a polynomial of degree 3.
complexSolveCubic :: Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveCubic _ 0 0 0 = Just [0]
complexSolveCubic a b 0 0 = Just [0, fromRational $ (-b) / a]
complexSolveCubic a b c 0
  | Just xs <- complexSolveQuadratic a b c = Just $ nub $ 0 : xs
  | otherwise = Just [0]
complexSolveCubic a b c d = map restore <$> complexSolveDepressedCubic p q
  where
    restore t = t - fromRational (b / (3 * a))
    p = (3 * a * c - b * b) / (3 * a * a)
    q = (2 * b * b * b - 9 * a * b * c + 27 * a * a * d) / (27 * a * a * a)

-- | Solve depressed cubic equations of the form \(x^3 + px + q = 0\).
--
-- #### References
--
-- * [Wikipedia](https://en.wikipedia.org/wiki/Cubic_equation)
complexSolveDepressedCubic :: Rational -> Rational -> Maybe [Expression]
complexSolveDepressedCubic p q
  | discriminant == 0, p == 0 = Just [0]
  | discriminant == 0 = Just $ map fromRational $ nub [3 * q / p, (-3) / 2 * q / p]
  | p == 0 = Just [x * e | let x = fromRational (-q) ** (1 / 3), e <- [1, e1, e2]]
  | otherwise =
      Just
        [ c - fromRational p / (3 * c),
          c * e1 - fromRational p / (3 * c * e1),
          c * e2 - fromRational p / (3 * c * e2)
        ]
  where
    discriminant = -(4 * p * p * p + 27 * q * q)
    c = (fromRational (-(q / 2)) + sqrt (fromRational (q * q / 4 + p * p * p / 27))) ** (1 / 3)
    e1 = (-1 + sqrt (-3)) / 2
    e2 = (-1 - sqrt (-3)) / 2

-- | Returns the roots for a polynomial of degree 4.
complexSolveQuartic :: Rational -> Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveQuartic _ 0 0 0 0 = Just [0]
complexSolveQuartic a b 0 0 0 = Just $ nub [0, fromRational $ -(b / a)]
complexSolveQuartic a b c 0 0
  | Just xs <- complexSolveQuadratic a b c = Just $ nub $ 0 : xs
  | otherwise = Just [0]
complexSolveQuartic a b c d 0
  | Just xs <- complexSolveCubic a b c d = Just $ nub $ 0 : xs
  | otherwise = Just [0]
complexSolveQuartic a 0 b 0 c = concatMap restore <$> complexSolveQuadratic a b c
  where
    restore 0 = [0]
    restore x = [sqrt x, -sqrt x]
complexSolveQuartic a b c d e = map restore <$> complexSolveDepressedQuartic p q r
  where
    restore x = x - fromRational (b / (4 * a))

    p = (-3) * b ^ two / (8 * a ^ two) + c / a
    q = b ^ three / (8 * a ^ three) - b * c / (2 * a ^ two) + d / a
    r = (-3) * b ^ four / (256 * a ^ four) + c * b ^ two / (16 * a ^ three) - b * d / (4 * a ^ two) + e / a

    two = 2 :: Int
    three = 3 :: Int
    four = 4 :: Int

-- | Returns the roots for a depressed quartic equation \(x^4+ax^2+bx+c=0\).
-- Complex numbers roots are included.
--
-- #### References
--
-- * [Wikipedia](https://en.wikipedia.org/wiki/Quartic_equation#The_general_case)
complexSolveDepressedQuartic :: Rational -> Rational -> Rational -> Maybe [Expression]
complexSolveDepressedQuartic a 0 c = concatMap restore <$> complexSolveQuadratic 1 a c
  where
    restore 0 = [0]
    restore x = [sqrt x, -sqrt x]
complexSolveDepressedQuartic a b c = do
  -- Get any cubic root of the following cubic equation.
  ys <- complexSolveCubic 2 (-a) (-(2 * c)) (a * c - b * b / 4)
  y <- case ys of x : _ -> Just x; [] -> Nothing

  -- Because b /= 0, it is the case that s /= 0.
  let s = sqrt $ 2 * y - fromRational a
  let t = (-2) * y - fromRational a

  return
    [ (1 / 2) * (-s + sqrt (t + 2 * fromRational b / s)),
      (1 / 2) * (-s - sqrt (t + 2 * fromRational b / s)),
      (1 / 2) * (s + sqrt (t - 2 * fromRational b / s)),
      (1 / 2) * (s - sqrt (t - 2 * fromRational b / s))
    ]
