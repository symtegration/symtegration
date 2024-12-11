-- |
-- Module: Symtegration
-- Description: Library for symbolic integration of mathematical expressions.
-- Maintainer: dev@chungyc.org
--
-- Symtegration is a library for symbolic integration of mathematical expressions.
--
-- For symbolic differentiation, use automatic differentiation.
--
-- >>> import Numeric.AD
-- >>> diff (\x -> x + 1) ("x" :: Expression)
-- Number 1
-- >>> toHaskellText $ simplify $ diff (\x -> x ** 3 + 1) ("x" :: Expression)
-- "3 * (x ** 2)"
module Symtegration
  ( -- * Symbolic representation
    Expression,
    evaluate,
    fractionalEvaluate,

    -- * Simplification
    simplify,

    -- * Conversion
    toHaskellText,
  )
where

import Symtegration.Symbolic (Expression, evaluate, fractionalEvaluate)
import Symtegration.Symbolic.Haskell (toHaskellText)
import Symtegration.Symbolic.Simplify.RecursiveHeuristic (simplify)
