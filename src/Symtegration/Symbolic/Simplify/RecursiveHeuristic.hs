-- |
-- Module: Symtegration.Symbolic.Simplify.RecursiveHeuristic
-- Description: Simplification of mathematical expressions with recursive heuristics.
-- Maintainer: dev@chungyc.org
--
-- Supports simplification of the symbolic representation of a mathematical expression
-- using recursive heuristics.  This applies simple transformations to simplify
-- mathematical expressions recursively on sub-terms, and is not very sophisticated.
module Symtegration.Symbolic.Simplify.RecursiveHeuristic (simplify) where

import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration

-- | Simplifies a symbolic representation of a mathematical expression.
--
-- This implementation recursively applies simple heuristics to each sub-term.
--
-- >>> toHaskell $ simplify $ 1 + 5
-- "6"
-- >>> toHaskell $ simplify $ "x" ** (3 - 1) * 1 + 0 * sin "x"
-- "x ** 2"
simplify :: Expression -> Expression
simplify (UnaryApply func x) =
  unary $ UnaryApply func $ simplify x
simplify (BinaryApply func x y) =
  binary $ BinaryApply func (simplify x) (simplify y)
simplify e = e

-- | Simplify expression with unary function.
-- The argument to the function should already be simplified.
unary :: Expression -> Expression
unary (Negate' (Negate' x)) = x
unary e@(Negate' (Number n))
  | n < 0 = Number (-n)
  | otherwise = e
unary (Exp' 0) = 1
unary (Exp' (Log' x)) = x
unary (Log' (Exp' x)) = x
unary (Log' 1) = 0
unary e = e

-- | Simplify expression with binary function.
-- The arguments to the function should already be simplified.
binary :: Expression -> Expression
binary (Number m :+: Number n) = Number $ m + n
binary (0 :+: x) = x
binary (x :+: 0) = x
binary e@((Negate' x) :+: y)
  | x == y = 0
  | otherwise = e
binary e@(x :+: (Negate' y))
  | x == y = 0
  | otherwise = e
binary (Number m :*: Number n) = Number $ m * n
binary (0 :*: _) = Number 0
binary (_ :*: 0) = Number 0
binary (1 :*: x) = x
binary (x :*: 1) = x
binary e@((x :**: y) :*: (x' :**: y'))
  | x == x' = x :**: simplify (y :+: y')
  | otherwise = e
binary e@((x :**: y) :*: x')
  | x == x' = x :**: simplify (y :+: 1)
  | otherwise = e
binary e@(x :*: (x' :**: y'))
  | x == x' = x :**: simplify (1 :+: y')
  | otherwise = e
binary (Number m :-: Number n) = Number $ m - n
binary (x :-: 0) = x
binary (x :-: (Negate' y)) = binary (x :+: y)
binary e@(x :-: y)
  | x == y = 0
  | otherwise = e
binary e@(_ :/: 0) = e
binary (x :/: 1) = x
binary (Number m :/: Number n)
  | n == d = m'
  | otherwise = m' :/: n'
  where
    d = gcd m n
    m' = Number $ m `div` d
    n' = Number $ n `div` d
binary e@(1 :/: (x :**: Number n))
  | n < 0 = x :**: Number (-n)
  | otherwise = e
binary e@((x :**: y) :/: (x' :**: y'))
  | x == x' = x :**: simplify (y :-: y')
  | otherwise = e
binary e@((x :**: y) :/: x')
  | x == x' = x :**: simplify (y :-: 1)
  | otherwise = e
binary e@(x :/: y)
  | x == y = 1
  | otherwise = e
binary (_ :**: 0) = 1
binary (x :**: 1) = x
binary e = e
