-- |
-- Module: Symtegration.Integration.Parts
-- Description: Integration by parts.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Parts (integrate) where

import Control.Applicative (asum, (<|>))
import Data.Text (Text)
import Symtegration.Differentiation
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- $setup
-- >>> import Symtegration.Integration.Powers qualified as Powers
-- >>> import Symtegration.Integration.Trigonometric qualified as Trigonometric
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify

-- | Integrates by substitution.
--
-- Specifically, if for
--
-- \[ \int f g \, dx \]
--
-- it is the case that we can find \(F = \int f \, dx\) and \(\int F \frac{dg}{dx} \, dx\),
-- then we can derive the integral as
--
-- \[ \int f g \, dx = F g - \int F \frac{dg}{dx} \, dx \]
--
-- >>> let directMethods = [Powers.integrate, Trigonometric.integrate]
-- >>> toHaskell . simplify <$> integrate directMethods "x" ("x" * cos "x")
-- Just "x * sin x + cos x"
integrate ::
  -- | Integration algorithms to try on the parts.
  [Text -> Expression -> Maybe Expression] ->
  -- | Symbol for the variable.
  Text ->
  -- | Expression to integrate.
  Expression ->
  -- | Integral, if derived.
  Maybe Expression
integrate fs v (x :*: y) = integrate' fs v x y <|> integrate' fs v y x
integrate _ _ _ = Nothing

-- | The actual work of integrating by parts, except it tries the parts in only one order.
integrate' ::
  -- | Integration algorithms to try on the parts.
  [Text -> Expression -> Maybe Expression] ->
  -- | Symbol for the variable.
  Text ->
  -- | The part to be integrated.
  Expression ->
  -- | The part to be differentiated.
  Expression ->
  -- | Integral, if derived.
  Maybe Expression
integrate' fs v x y = do
  ix <- integrate'' x
  iixdy <- integrate'' $ simplifyForVariable v $ ix * differentiate v y
  return $ ix * y - iixdy
  where
    integrate'' z = asum $ map (\f -> f v z) fs
