-- |
-- Module: Symtegration.Integration.Factor
-- Description: Factor a term into constant and non-constant parts.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Factor (factor, isConstant) where

import Data.Text (Text)
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- $setup
-- >>> import Symtegration

-- | Factor a multiplicative term into a constant portion and the variable-dependent portion.
-- E.g., \(2a x \sin x\) into \(2a\) and \(x \sin x\) when the variable is \(x\).
--
-- >>> let s (x, y) = (toHaskell $ simplify x, toHaskell $ simplify y)
-- >>> s $ factor "x" $ 2 * ("a" * sin "x")
-- ("2 * a","sin x")
--
-- Assumes algebraic ring ordering has been applied to the term.
factor ::
  -- | Symbol for the variable.
  Text ->
  -- | Term to separate into constant and non-constant portions.
  Expression ->
  (Expression, Expression)
factor _ e@(Number _) = (e, Number 1)
factor v e@(Symbol s) | v == s = (Number 1, e) | otherwise = (e, Number 1)
factor v e@(UnaryApply _ x) | isConstant v x = (e, Number 1) | otherwise = (Number 1, e)
factor v e@(x :*: (y :*: z))
  | isConstant v x, isConstant v y, isConstant v z = (e, Number 1)
  | isConstant v x, isConstant v y = (simplifyForVariable v $ x :*: (y :*: c), z')
  | isConstant v x = (simplifyForVariable v $ x :*: d, y')
  | otherwise = (Number 1, e)
  where
    (c, z') = factor v z
    (d, y') = factor v $ y :*: z
factor v e@(x :*: y)
  | isConstant v x, isConstant v y = (e, Number 1)
  | isConstant v x = (x, y)
  | otherwise = (Number 1, e)
factor v e | isConstant v e = (e, Number 1) | otherwise = (Number 1, e)

-- | Returns whether an expression contains the variable.
--
-- >>> isConstant "x" $ 1 + "x"
-- False
-- >>> isConstant "x" $ 1 + "a"
-- True
isConstant ::
  -- | Symbol for the variable.
  Text ->
  -- | Expression to check.
  Expression ->
  -- | Whether the expression is a constant.
  Bool
isConstant _ (Number _) = True
isConstant v (Symbol s) = s /= v
isConstant v (UnaryApply _ x) = isConstant v x
isConstant v (BinaryApply _ x y) = isConstant v x && isConstant v y
