-- |
-- Module: Symtegration.Integration.Term
-- Description: Integrates a single term.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Term (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symtegration.Integration.Factor
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- $setup
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify.RecursiveHeuristic

-- | Integrate a single term, separating out the constant factor and
-- applying direct methods to the non-constant factor.
--
-- >>> import Symtegration.Integration.Trigonometric qualified as T
-- >>> let f = "a" * sin "x"
-- >>> T.integrate "x" f
-- Nothing
-- >>> let g = integrate [T.integrate] "x" f
-- >>> toHaskell . simplify <$> g
-- Just "a * (negate (cos x))"
--
-- Assumes the expression has had algebraic ring ordering applied.
integrate ::
  -- | Functions for directly integrating the non-constant factor.
  [Text -> Expression -> Maybe Expression] ->
  -- | The variable being integrated over.
  Text ->
  -- | The expression being integrated.
  Expression ->
  -- | The integral, if successful.
  Maybe Expression
integrate fs v e = asum $ map (\f -> (:*:) c <$> f v u) fs
  where
    e' = simplify v e
    (c, u) = factor v e'
