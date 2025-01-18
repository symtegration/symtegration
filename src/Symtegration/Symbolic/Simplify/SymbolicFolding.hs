-- |
-- Module: Symtegration.Symbolic.Simplify.SymbolicFolding
-- Description: Folding of symbolic terms.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This merges symbolic terms as much as it can to simplify expressions.
-- Simplifications are finitely equivalent; i.e., any calculation with
-- finite inputs should result in the equivalent finite input.
module Symtegration.Symbolic.Simplify.SymbolicFolding (simplify) where

import Symtegration.Symbolic

-- | Folds symbolic terms as much as it can to simplify expressions.
--
-- Assumes algebraic ring ordering has been applied.
simplify :: Expression -> Expression
simplify e@(Number _) = e
simplify e@(Symbol _) = e
simplify (UnaryApply func x) =
  unary $ UnaryApply func $ simplify x
simplify (BinaryApply func x y) =
  binary $ BinaryApply func (simplify x) (simplify y)

-- | Folds symbolic terms for unary expressions.
--
-- The arguments should already have been simplified.
unary :: Expression -> Expression
unary (Negate' (Negate' x)) = simplify x
unary (Negate' x) = (-1) * x
unary e = e

-- | Folds symbolic terms for binary expressions.
--
-- The arguments should already have been simplified.
binary :: Expression -> Expression
-- Fold addition.
binary e@(x :+: Negate' y)
  | x == y = Number 0
  | otherwise = e
binary e@(Negate' x :+: y)
  | x == y = Number 0
  | otherwise = e
binary (Number 0 :+: x) = x
binary (x :+: Number 0) = x
binary e@((Number n :*: x) :+: ((Number m :*: y) :+: z))
  | x == y = simplify $ (Number (m + n) :*: x) :+: z
  | otherwise = e
binary e@((Number n :*: x) :+: (y :+: z))
  | x == y = simplify $ (Number (n + 1) :*: x) :+: z
  | otherwise = e
binary e@(x :+: ((Number n :*: y) :+: z))
  | x == y = simplify (Number (n + 1) :*: x) :+: z
  | otherwise = e
binary e@((Number n :*: x) :+: (Number m :*: y))
  | x == y = simplify $ Number (n + m) :*: x
  | otherwise = e
binary e@(x :+: (Number n :*: y))
  | x == y = simplify $ Number (n + 1) :*: x
  | otherwise = e
binary e@((Number n :*: x) :+: y)
  | x == y = simplify $ Number (n + 1) :*: x
  | otherwise = e
binary e@(x :+: (y :+: z))
  | x == y = simplify $ Number 2 :*: x :+: z
  | otherwise = e
binary e@(x :+: y)
  | x == y = simplify $ Number 2 :*: x
  | otherwise = e
-- Fold multiplication.
binary (Number 0 :*: _) = Number 0
binary (_ :*: Number 0) = Number 0
binary (x :*: Number 1) = x
binary (Number 1 :*: x) = x
binary e@(x :*: (y :**: Number n))
  | x == y = simplify $ x :**: Number (n + 1)
  | otherwise = e
binary e@((x :**: y) :*: (x' :**: y'))
  | x == x' = simplify $ x :**: (y :+: y')
  | otherwise = e
binary e@(x :*: ((y :**: Number n) :*: z))
  | x == y = simplify $ (x :**: Number (n + 1)) :*: z
  | otherwise = e
binary e@((x :**: Number n) :*: (y :*: z))
  | x == y = simplify $ (x :**: Number (n + 1)) :*: z
  | otherwise = e
binary e@(x :*: (y :*: z))
  | x == y = simplify $ (x :**: Number 2) :*: z
  | otherwise = e
binary e@(x :*: y)
  | x == y = simplify $ x :**: Number 2
  | otherwise = e
-- Fold division.
binary (x :/: (y :/: z)) = simplify $ (x :*: z) :/: y
binary ((x :/: y) :/: z) = simplify $ x :/: (y :*: z)
binary (x :/: Number 1) = x
binary (x :/: Number (-1)) = (-1) * x
-- Fold powers.
binary (_ :**: Number 0) = Number 1
binary (x :**: Number 1) = x
binary ((x :**: y) :**: z) =
  simplify $ x :**: (y :*: z)
-- Fold subtraction.
binary e@(x :-: y)
  | x == y = Number 0
  | otherwise = e
binary e = e
