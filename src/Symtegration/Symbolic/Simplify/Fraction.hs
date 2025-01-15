-- |
-- Module: Symtegration.Symbolic.Simplify.Fraction
-- Description: Cancel out common numeric factors in fractions.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.Fraction (simplify) where

import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic
-- >>> import Symtegration.Symbolic.Haskell

-- | Cancel out common numeric factors in fractions.
--
-- >>> toHaskell $ simplify $ 10 / 20
-- "1 / 2"
--
-- >>> toHaskell $ simplify $ Number (-15) / Number (-10)
-- "3 / 2"
--
-- >>> toHaskell $ simplify $ (12 * "x") / (4 * "y")
-- "(3 * x) / (1 * y)"
--
-- >> toHaskell $ simplify $ (15 * "x" + 20 * "y") / (5 * "z" - 35 * "u")
-- "(3 * x + 4 * y) / (1 * z - 7 * u)"
--
-- Assumes numeric folding and algebraic ring ordering has been applied.
simplify :: Expression -> Expression
simplify e@(Number _ :/: Number 0) = e
simplify (Number n :/: Number m)
  | m > 0 = Number (n `div` g) :/: Number (m `div` g)
  | otherwise = Number ((-n) `div` g) :/: Number ((-m) `div` g)
  where
    g = gcd n m
simplify (x :/: y) = divideFactor g x' :/: divideFactor g y'
  where
    g = gcd (commonFactor x') (commonFactor y')
    x' = simplify x
    y' = simplify y
simplify ((1 :/: x) :*: y) = (1 :/: divideFactor g x') :*: divideFactor g y'
  where
    g = gcd (commonFactor x') (commonFactor y')
    x' = simplify x
    y' = simplify y
simplify (UnaryApply func x) = UnaryApply func $ simplify x
simplify (BinaryApply func x y) = BinaryApply func (simplify x) (simplify y)
simplify e = e

-- | Finds a common factor which multiplies each term in an expression.
-- Ignores terms not in algebraic ring ordering or includes direct negations of numbers.
commonFactor :: Expression -> Integer
commonFactor (Number n) = n
commonFactor (x :+: y) = gcd (commonFactor x) (commonFactor y)
commonFactor (x :-: y) = gcd (commonFactor x) (commonFactor y)
commonFactor (Number n :*: _) = n
commonFactor _ = 1

-- | Divides each term in an expression by a common factor.
-- Specialized for dividing factors found by 'commonFactor.
divideFactor :: Integer -> Expression -> Expression
divideFactor 0 e = e
divideFactor 1 e = e
divideFactor g (Number n) = Number $ n `div` g
divideFactor g (x :+: y) = divideFactor g x :+: divideFactor g y
divideFactor g (Number n :*: x) = Number (n `div` g) :*: x
divideFactor g e = e :/: Number g
