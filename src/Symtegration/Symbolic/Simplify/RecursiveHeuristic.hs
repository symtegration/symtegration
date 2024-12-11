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
-- >>> toHaskellText $ simplify $ 1 + 5
-- "6"
-- >>> toHaskellText $ simplify $ "x" ** (3 - 1) * 1 + 0 * sin "x"
-- "x ** 2"
simplify :: Expression -> Expression
simplify (UnaryApply func x)
  | Negate <- func, Negate' x'' <- x' = x''
  | Negate <- func, Number n <- x', n < 0 = Number (-n)
  | otherwise = UnaryApply func x'
  where
    x' = simplify x
simplify (BinaryApply func x y)
  | Add <- func, Number m <- x', Number n <- y' = Number (m + n)
  | Add <- func, Number 0 <- x' = y'
  | Add <- func, Number 0 <- y' = x'
  | Multiply <- func, Number m <- x', Number n <- y' = Number (m * n)
  | Multiply <- func, Number 0 <- x' = Number 0
  | Multiply <- func, Number 0 <- y' = Number 0
  | Multiply <- func, Number 1 <- x' = y'
  | Multiply <- func, Number 1 <- y' = x'
  | Subtract <- func, Number m <- x', Number n <- y' = Number (m - n)
  | Subtract <- func, Number 0 <- y' = x'
  | Subtract <- func, x' == y' = Number 0
  | Divide <- func, Number 0 <- y' = x' :/: y'
  | Divide <- func, Number 1 <- y' = x'
  | Divide <- func,
    Number m <- x',
    Number n <- y' =
      let d = gcd m n
          x'' = Number $ m `div` d
          y'' = Number $ n `div` d
       in if n == d then x'' else x'' :/: y''
  | Divide <- func, x' == y' = Number 1
  | Power <- func, y' == Number 1 = x'
  | otherwise = BinaryApply func x' y'
  where
    x' = simplify x
    y' = simplify y
simplify e = e
