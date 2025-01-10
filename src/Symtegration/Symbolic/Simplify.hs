-- |
-- Module: Symtegration.Symbolic.Simplify
-- Description: Simplifes symbolic representations of mathematical expressions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Supports the simplification of the symbolic representation for a mathematical expression.
-- This is aimed towards making it easier to find common factors for the purpose of integration.
-- It requires the specification of which symbol represents the variable.
module Symtegration.Symbolic.Simplify (simplify, simplifyForVariable) where

import Data.Text (Text)
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify.AlgebraicRingOrder qualified as AlgebraicRingOrder
import Symtegration.Symbolic.Simplify.NumericFolding qualified as NumericFolding
import Symtegration.Symbolic.Simplify.SymbolicFolding qualified as SymbolicFolding

-- $setup
-- >>> import Symtegration.Symbolic.Haskell

-- | Simplifies symbolic representations of mathematical expressions.
-- All addition and multiplication will be associated to the left.
--
-- >>> toHaskell $ simplify $ 1 + "a" * "x" ** 3 + "x"
-- "1 + x + a * x ** 3"
-- >>> toHaskell $ simplify $ "a" ** 143 + "x" + "b" ** 2
-- "x + a ** 143 + b ** 2"
simplify :: Expression -> Expression
simplify = simplifyForVariable ""

-- | Simplifies symbolic representations of mathematical expressions
-- with special consideration for a particular variable.
--
-- All addition and multiplication will be associated to the left.
-- Terms with higher orders of the variable will appear later.
-- The simplification is done with an eye towards making it
-- easier to find common factors.
--
-- >>> toHaskell $ simplifyForVariable "x" $ 1 + "a" * "x" ** 3 + "x"
-- "1 + x + a * x ** 3"
-- >>> toHaskell $ simplifyForVariable "x" $ "a" ** 143 + "x" + "b" ** 2
-- "a ** 143 + b ** 2 + x"
-- >>> toHaskell $ simplifyForVariable "x" $ "a" * "x" + "x" + "b ** 2" + "x" ** 2
-- "b ** 2 + x + a * x + x ** 2"
simplifyForVariable ::
  -- | Symbol for the variable.
  Text ->
  -- | Expression to be simplified.
  Expression ->
  -- | Simplified expression.
  Expression
simplifyForVariable v e
  | e == e' = e
  | otherwise = simplifyForVariable v e' -- Another round.
  where
    e' = f e
    f = NumericFolding.simplify . SymbolicFolding.simplify . AlgebraicRingOrder.order v
