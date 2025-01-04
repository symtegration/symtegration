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
