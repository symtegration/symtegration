module Symtegration.Integration.Polynomial (rationalIntegrate, symbolicIntegrate) where

import Data.Text (Text)
import Symtegration.Polynomial qualified as Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic

rationalIntegrate :: Text -> Expression -> Maybe Expression
rationalIntegrate var expr = do
  p <- fromExpression (forVariable var) expr
  let ps = Polynomial.integrate p :: IndexedPolynomial
  return $ toExpression var toRationalCoefficient ps

symbolicIntegrate :: Text -> Expression -> Maybe Expression
symbolicIntegrate var expr = do
  p <- fromExpression (withSymbolicCoefficients (forVariable var)) expr
  let ps = Polynomial.integrate p :: IndexedSymbolicPolynomial
  return $ toExpression var toSymbolicCoefficient ps
