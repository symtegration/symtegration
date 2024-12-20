-- |
-- Module: Symtegration.Symbolic.Simplify
-- Description: Simplifes symbolic representations of mathematical expressions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify where

import Data.Text (Text)
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify.AlgebraicRingOrder qualified as AlgebraicRingOrder
import Symtegration.Symbolic.Simplify.NumericFolding qualified as NumericFolding
import Symtegration.Symbolic.Simplify.SymbolicFolding qualified as SymbolicFolding

-- | Simplifies symbolic representations of mathematical expressions.
simplify :: Text -> Expression -> Expression
simplify v e
  | e == e' = e
  | otherwise = simplify v e' -- Another round.
  where
    e' = f e
    f = NumericFolding.simplify . SymbolicFolding.simplify . AlgebraicRingOrder.order v
