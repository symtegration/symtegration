-- |
-- Module: Symtegration.Polynomial.Solve
-- Description: Derive the roots of polynomials with rational coefficients.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This module supports deriving exact solutions to polynomial equations.
-- It cannot derive solutions for all polynomials; it will only return those which it can.
module Symtegration.Polynomial.Solve (solve) where

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
  | degree p == 1 = solveLinear (coefficient p 1) (coefficient p 0)
  | degree p == 2 = solveQuadratic (coefficient p 2) (coefficient p 1) (coefficient p 0)
  | degree p == 3 = solveCubic (coefficient p 3) (coefficient p 2) (coefficient p 1) (coefficient p 0)
  | degree p == 4 = solveQuartic (coefficient p 4) (coefficient p 3) (coefficient p 2) (coefficient p 1) (coefficient p 0)
  | otherwise = Nothing

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

-- | Returns the real roots for a polynomial of degree 2.
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
  | s == 0, p == 0 = Just [0]
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
  | otherwise = Just [0]
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
