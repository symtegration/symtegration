-- |
-- Module: Symtegration.Differentiation
-- Description: Differentiate mathematical expressions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Differentiate symbolic representations of mathematical expressions.
-- This module does not actually implement differentiation,
-- but is rather a thin wrapper over "Numeric.AD" providing
-- derivatives for 'Expression' with some simplification applied.
module Symtegration.Differentiation (differentiate) where

import Data.Text (Text)
import Numeric.AD.Rank1.Forward
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify
import Symtegration.Symbolic.Simplify.Tidy

-- $setup
-- >>> import Symtegration.Symbolic.Haskell

-- | Differentiates a mathematical expression.
--
-- >>> toHaskell $ differentiate "x" $ "x" ** 2
-- "2 * x"
-- >>> toHaskell $ differentiate "x" $ "a" * sin "x"
-- "a * cos x"
--
-- This uses [Numeric.AD](https://hackage.haskell.org/package/ad).
differentiate ::
  -- | Symbol representing the variable.
  Text ->
  -- | Symbolic representation of the mathematical expression to differentiate.
  Expression ->
  -- | The derivative.
  Expression
differentiate v e = tidy $ simplifyForVariable v $ diff f $ Symbol v
  where
    f = toFunction e assign
    assign x
      | v == x = id
      | otherwise = const $ auto $ Symbol x
