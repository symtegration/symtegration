-- |
-- Module: Symtegration
-- Description: Library for symbolic integration of mathematical expressions.
-- Maintainer: dev@chungyc.org
--
-- Symtegration is a library for symbolic integration of mathematical expressions.
-- For normal use, this is the only module which needs to be loaded.
-- Other modules are used for finer control over what happens,
-- or for supporting the work that yet other modules do.
--
-- For symbolic differentiation, use [automatic differentiation](https://hackage.haskell.org/package/ad).
-- For example,
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

    -- * Integration
    integrate,

    -- * Simplification
    simplify,

    -- * Conversion
    toHaskellText,
  )
where

import Data.Text (Text)
import Symtegration.Integration qualified as Integration
import Symtegration.Symbolic (Expression, evaluate, fractionalEvaluate)
import Symtegration.Symbolic.Haskell (toHaskellText)
import Symtegration.Symbolic.Simplify.RecursiveHeuristic (simplify)

-- |
-- Return the indefinite integral of a mathematical expression given
-- its symbolic representation.  It will return 'Nothing' if it is
-- unable to derive an integral.  The indefinite integral will be
-- simplified to a certain extent.
--
-- For example, with \(\int (4x^3 + 1) \, dx = x^4 + x\)
-- where all the coefficients are numbers:
--
-- >>> toHaskellText <$> integrate "x" (4 * "x" ** 3 + 1)
-- Just "(x ** 4) + x"
--
-- It can also return indefinite integrals when the coefficients
-- are symbolic, as with \(\int (xz+y) \, dz = \frac{xz^2}{2} + yz\):
--
-- >>> toHaskellText <$> integrate "z" ("x" * "z" + "y")
-- Just "((x / 2) * (z ** 2)) + (y * z)"
integrate :: Text -> Expression -> Maybe Expression
integrate var expr = simplify <$> Integration.integrate var expr
