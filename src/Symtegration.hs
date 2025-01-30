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
-- Just "x + x ** 4"
--
-- For another example, with \(\int (xz+y) \, dz = \frac{xz^2}{2} + yz\):
--
-- >>> import Symtegration
-- >>> toHaskell <$> integrate "z" ("x" * "z" + "y")
-- Just "y * z + 1 / 2 * x * z ** 2"
module Symtegration
  ( -- * Symbolic representation
    Expression,

    -- * Integration
    integrate,

    -- * Differentiation
    differentiate,

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
    -- directly for differentiation.
    simplify,
    tidy,
  )
where

import Data.Text (Text)
import Symtegration.Differentiation (differentiate)
import Symtegration.Integration qualified as Integration
import Symtegration.Symbolic (Expression, evaluate, fractionalEvaluate, toFunction)
import Symtegration.Symbolic.Haskell (toHaskell)
import Symtegration.Symbolic.LaTeX (toLaTeX)
import Symtegration.Symbolic.Simplify (simplify, simplifyForVariable)
import Symtegration.Symbolic.Simplify.Tidy (tidy)

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
-- Just "x + x ** 4"
--
-- It can also return indefinite integrals when the coefficients
-- are symbolic, as with \(\int (xz+y) \, dz = \frac{xz^2}{2} + yz\):
--
-- >>> toHaskell <$> integrate "z" ("x" * "z" + "y")
-- Just "y * z + 1 / 2 * x * z ** 2"
--
-- === __Definite integrals__
--
-- If the indefinite integral \(F = \int f(x) \, dx\) is continuous,
-- then the definite integral is
--
-- \[ \int_a^b f(x) \, dx = F(b) - F(a) \]
--
-- This is /not/ true in general if \(F\) is not continuous in the integral interval.
-- Care must be taken when computing a definite integral from an indefinite integral
-- which is not continuous.  For example, an indefinite integral such as the following
--
-- \[ \int f \, dx = F = \sum_{\alpha \mid 4 \alpha^2 + 1 = 0} \alpha \log (x^3 +2 \alpha x^2 - 3x - 4\alpha) \]
--
-- uses complex logarithms, where \(\alpha = \pm \frac{i}{2}\) and there are discontinuities at \(x=-\sqrt{2}\) and \(x=\sqrt{2}\).
--
-- Definite integrals for such cases can be handled by integrating over continuous intervals separately.
-- For example,
--
-- \[ \int_1^2 f \, dx = \left( F(2) - \lim_{x \rightarrow \sqrt{2}^+} F(x) \right) + \left( \lim_{x \rightarrow \sqrt{2}^-} F(x) - F(1) \right) \]
--
-- Symtegration will return real function integrals if it can,
-- but may return complex function integrals instead if it is unable to.
integrate ::
  -- | The symbol representing the variable being integrated over.
  Text ->
  -- | The mathematical expression being integrated.
  Expression ->
  -- | The indefinite integral, if derived.
  Maybe Expression
integrate var expr = tidy . simplifyForVariable var <$> Integration.integrate var expr
