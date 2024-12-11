-- |
-- Module: Symtegration
-- Description: Library for symbolic integration of mathematical expressions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Symtegration is a library for symbolic integration of mathematical expressions.
-- For normal use, this is the only module which needs to be loaded.
-- Other modules are used for finer control over what happens,
-- or for supporting the work that yet other modules do.
--
-- For example, with \(\int (4x^3 + 1) \, dx = x^4 + x\):
--
-- >>> import Symtegration
-- >>> toHaskell <$> integrate "x" (4 * "x" ** 3 + 1)
-- Just "(x ** 4) + x"
--
-- For another example, with \(\int (xz+y) \, dz = \frac{xz^2}{2} + yz\):
--
-- >>> import Symtegration
-- >>> toHaskell <$> integrate "z" ("x" * "z" + "y")
-- Just "(x / 2) * (z ** 2) + y * z"
--
-- For symbolic differentiation, use [automatic differentiation](https://hackage.haskell.org/package/ad).
-- For example,
--
-- >>> import Numeric.AD
-- >>> diff (\x -> x + 1) ("x" :: Expression)
-- Number 1
-- >>> toHaskell $ simplify $ diff (\x -> x ** 3 + 1) ("x" :: Expression)
-- "3 * (x ** 2)"
module Symtegration
  ( -- * Symbolic representation
    Expression,

    -- * Integration
    integrate,

    -- * Computation
    evaluate,
    fractionalEvaluate,
    toFunction,

    -- * Conversion
    toHaskell,
    toLaTeX,

    -- * Simplification

    -- | When using only this module, explicitly simplifying mathematical expressions
    -- should usually not be necessary, since the exported functions automatically
    -- simplify results as appropriate.  One may want to explicitly simplify
    -- mathematical expressions when used with other packages, however,
    -- such as when using [Numeric.AD](https://hackage.haskell.org/package/ad)
    -- for differentiation.
    simplify,
  )
where

import Data.Text (Text)
import Symtegration.Integration qualified as Integration
import Symtegration.Symbolic (Expression, evaluate, fractionalEvaluate, toFunction)
import Symtegration.Symbolic.Haskell (toHaskell)
import Symtegration.Symbolic.LaTeX (toLaTeX)
import Symtegration.Symbolic.Simplify.RecursiveHeuristic (simplify)

-- |
-- Returns the indefinite integral of a mathematical expression given
-- its symbolic representation.  It will return 'Nothing' if it is
-- unable to derive an integral.  The indefinite integral will be
-- simplified to a certain extent.
--
-- For example, with \(\int (4x^3 + 1) \, dx = x^4 + x\)
-- where all the coefficients are numbers:
--
-- >>> toHaskell <$> integrate "x" (4 * "x" ** 3 + 1)
-- Just "(x ** 4) + x"
--
-- It can also return indefinite integrals when the coefficients
-- are symbolic, as with \(\int (xz+y) \, dz = \frac{xz^2}{2} + yz\):
--
-- >>> toHaskell <$> integrate "z" ("x" * "z" + "y")
-- Just "(x / 2) * (z ** 2) + y * z"
integrate ::
  -- | The symbol representing the variable being integrated over.
  Text ->
  -- | The mathematical expression being integrated.
  Expression ->
  -- | The indefinite integral, if derived.
  Maybe Expression
integrate var expr = simplify <$> Integration.integrate var expr
