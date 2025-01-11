-- |
-- Module: Symtegration.Symbolic.Simplify.Factor
-- Description: Factors out or cancels out common factors.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This factors out common factors in multiplication or cancels out common factors
-- in division to simplify symbolic representations of mathematical expressions.
module Symtegration.Symbolic.Simplify.Factor (simplify) where

import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic.Haskell

-- | Factors out or cancels common factors in multiplication or division.
-- Assumes algebraic ring ordering has been applied.
-- It will not factor out common factors in powers of a symbol.
--
-- >>> toHaskell $ simplify $ (4 * "x") + (4 * "y")
-- "4 * (x + y)"
-- >>> toHaskell $ simplify $ ("x" * "a") + ("a" * "z")
-- "a * (x + z)"
-- >>> toHaskell $ simplify $ (105 * "x") + (210 * "y")
-- "105 * (1 * x + 2 * y)"
-- >>> toHaskell $ simplify $ 213 / 426
-- "1 / 2"
simplify :: Expression -> Expression
simplify e@(s@(x :*: y) :+: t@(u :*: v))
  | x == u = x :*: (y :+: v)
  | x == v = x :*: (y :+: u)
  | y == u = y :*: (x :+: v)
  | y == v = y :*: (x :+: u)
  | (Just (s', t', c)) <- commonFactor s t = c :*: (s' :+: t')
  | otherwise = e
simplify e@(x :/: y)
  | (Just (x', y', _)) <- commonFactor x y = x' :/: y'
  | otherwise = e
simplify e = e

-- | For given expressions \(x\) and \(y\), returns \(x', y', c\)
-- such that \(x = cx'\) and \(y = cy').  It returns 'Nothing' if it is not
-- worth extracting a common factor, i.e., when it is 1.
commonFactor ::
  -- | Expression \(x\).
  Expression ->
  -- | Expression \(y\).
  Expression ->
  -- | \(x', y', g\).
  Maybe (Expression, Expression, Expression)
commonFactor (Number n :*: x) (Number m :*: y)
  | g /= 1 = Just (Number (n `div` g) :*: x, Number (m `div` g) :*: y, Number g)
  | otherwise = Nothing
  where
    g = gcd n m
commonFactor (Number n :*: x) (Number m)
  | g /= 1 = Just (Number (n `div` g) :*: x, Number (m `div` g), Number g)
  | otherwise = Nothing
  where
    g = gcd n m
commonFactor (Number n) (Number m :*: y)
  | g /= 1 = Just (Number (n `div` g), Number (m `div` g) :*: y, Number g)
  | otherwise = Nothing
  where
    g = gcd n m
commonFactor (Number n) (Number m)
  | g /= 1 = Just (Number (n `div` g), Number (m `div` g), Number g)
  | otherwise = Nothing
  where
    g = gcd n m
commonFactor _ _ = Nothing
