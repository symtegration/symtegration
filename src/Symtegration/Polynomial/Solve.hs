-- |
-- Module: Symtegration.Polynomial.Solve
-- Description: Derive the roots of polynomials with rational coefficients.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This module supports deriving exact solutions to polynomial equations.
-- It cannot derive solutions for all polynomials; it will only return those which it can.
module Symtegration.Polynomial.Solve where

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
    p = (3 * a * c - b ^ (2 :: Int)) / (3 * a ^ (2 :: Int))
    q = (2 * b ^ (3 :: Int) - 9 * a * b * c + 27 * a ^ (2 :: Int) * d) / (27 * a ^ (3 :: Int))

-- | Solve depressed cubic equations of the form \(x^2 + px + q = 0\).
--
-- https://en.wikipedia.org/wiki/Cubic_equation#Trigonometric_and_hyperbolic_solutions
solveDepressedCubic :: Rational -> Rational -> Maybe [Expression]
solveDepressedCubic 0 _ = Nothing
solveDepressedCubic p q
  | s < 0 = Nothing
  | p < 0, s > 0 = Nothing
  | p > 0 = Nothing
  | otherwise = Nothing
  where
    s = 4 * p ^ (3 :: Int) + 27 * q ^ (2 :: Int)

-- | Returns the real roots for a polynomial of degree 2.
--
-- For now, only solves the very special case of ax^4+bx^2=0,
-- which is useful for finding solutions (u,v) for the resultant of P and Q,
-- where R(u+iv) = P(u,v) + iQ(u,v) and R is quadratic.
solveQuartic :: Rational -> Rational -> Rational -> Rational -> Rational -> Maybe [Expression]
solveQuartic a 0 b 0 0
  | a > 0, b > 0 = Just [0]
  | a < 0, b < 0 = Just [0]
  | b == 0 = Just [0]
  | otherwise = Just [0, Sqrt' (fromRational (-(b / a))), -Sqrt' (fromRational (-(b / a)))]
solveQuartic a 0 0 0 b
  | a > 0, b > 0 = Just []
  | a < 0, b < 0 = Just []
  | b == 0 = Just [0]
  | otherwise = Nothing
solveQuartic _ _ _ _ _ = Nothing
