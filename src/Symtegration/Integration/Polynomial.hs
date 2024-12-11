-- |
-- Module: Symtegration.Integration.Polynomial
-- Description: Symbolically integrates polynomials.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Polynomial (rationalIntegrate, symbolicIntegrate) where

import Data.Text (Text)
import Symtegration.Polynomial qualified as Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic

-- | Symbolically integrate a polynomial with rational coefficients.
rationalIntegrate :: Text -> Expression -> Maybe Expression
rationalIntegrate var expr = do
  p <- fromExpression (forVariable var) expr
  let ps = Polynomial.integrate p :: IndexedPolynomial
  return $ toExpression var toRationalCoefficient ps

-- | Symbolically integrate a polynomial where the coefficients
-- themselves also involve symbolic representation.
symbolicIntegrate :: Text -> Expression -> Maybe Expression
symbolicIntegrate var expr = do
  p <- fromExpression (withSymbolicCoefficients (forVariable var)) expr
  let ps = Polynomial.integrate p :: IndexedSymbolicPolynomial
  return $ toExpression var toSymbolicCoefficient ps
