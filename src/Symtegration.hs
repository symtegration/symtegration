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
module Symtegration
  ( -- * Symbolic representation
    Expression,
    evaluate,
  )
where

import Symtegration.Symbolic
