-- |
-- Module: Symtegration.Integration.Substitution
-- Description: Integration by substitution.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Substitution (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symtegration.Differentiation
import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify.RecursiveHeuristic

-- | Integrates by substitution.
--
-- Specifically, if for
--
-- \[ \int f(g(x)) h(x) \, dx\]
--
-- it is the case that \(\frac{dg(x)}{dx} = h(x)\), then compute \(\int f(v) \, dv\) and substitute with \(v=g(x)\).
--
-- >>> import Symtegration.Integration.Trigonometric qualified as Trigonometric
-- >>> toHaskell <$> simplify <$> integrate [Trigonometric.integrate] "x" (sin ("a" * "x" + 1))
-- Just "(1 / a) * (negate (cos (a * x + 1)))"
integrate ::
  -- | Integration algorithms to try after substitution.
  [Text -> Expression -> Maybe Expression] ->
  -- | Symbol for the variable.
  Text ->
  -- | Expression to integrate.
  Expression ->
  -- | Integral, if derived.
  Maybe Expression
integrate fs v (x :*: UnaryApply func y)
  | Number 0 <- d = Nothing -- Argument is constant.
  | x' == y',
    -- Re-use v as the variable, as it is the one symbol guaranteed not to appear outside the argument.
    Just e <- integrateSubstitution fs v (UnaryApply func (Symbol v)) =
      Just $ (c :/: d) :*: substitute e (\s -> if s == v then Just y else Nothing)
  | otherwise = Nothing
  where
    (c, x') = factor v x
    (d, y') = factor v $ differentiate v y
integrate fs v (e@(UnaryApply _ _) :*: x) = integrate fs v $ x :*: e
integrate fs v e@(UnaryApply _ _) = integrate fs v $ Number 1 :*: e
integrate fs v (x :*: BinaryApply func y z)
  -- Re-use v as the variable, as it is the one symbol guaranteed not to appear outside the argument.
  | c /= Number 0,
    x' == y',
    isConstant v z,
    Just e <- integrateSubstitution fs v (BinaryApply func (Symbol v) z) =
      Just $ (b :/: c) :*: substitute e (\s -> if s == v then Just y else Nothing)
  | d /= Number 0,
    x' == z',
    isConstant v y,
    Just e <- integrateSubstitution fs v (BinaryApply func y (Symbol v)) =
      Just $ (b :/: d) :*: substitute e (\s -> if s == v then Just z else Nothing)
  | otherwise = Nothing
  where
    (b, x') = factor v x
    (c, y') = factor v $ differentiate v y
    (d, z') = factor v $ differentiate v z
integrate fs v (e@(BinaryApply _ _ _) :*: x) = integrate fs v $ x :*: e
integrate fs v e@(BinaryApply func _ _)
  | func /= Multiply = integrate fs v $ Number 1 :*: e
  | otherwise = Nothing
integrate _ _ _ = Nothing

-- | Use the given functions to integrate the given expression.
integrateSubstitution :: [Text -> Expression -> Maybe Expression] -> Text -> Expression -> Maybe Expression
integrateSubstitution fs v e = asum $ map (\f -> f v e) fs

-- | Factor a term into a constant portion and the variable-dependent portion.
-- E.g., \(2a x \sin x\) into \(2a\) and \(x \sin x\) when the variable is \(x\).
--
-- Assumes algebraic ring ordering has been applied to the term.
factor :: Text -> Expression -> (Expression, Expression)
factor _ e@(Number _) = (e, Number 1)
factor v e@(Symbol s) | v == s = (Number 1, e) | otherwise = (e, Number 1)
factor v e@(UnaryApply _ x) | isConstant v x = (e, Number 1) | otherwise = (Number 1, e)
factor v e@(x :*: (y :*: z))
  | isConstant v x, isConstant v y, isConstant v z = (e, Number 1)
  | isConstant v x, isConstant v y = (x :*: y, z)
  | isConstant v x = (x, y :*: z)
  | otherwise = (Number 1, e)
factor v e | isConstant v e = (e, Number 1) | otherwise = (Number 1, e)

-- | Returns whether an expression contains the variable.
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
