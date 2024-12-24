-- |
-- Module: Symtegration.Integration
-- Description: Symbolically integrates mathematical expressions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symtegration.Integration.Polynomial qualified as Polynomial
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Substitution qualified as Substitution
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- |
-- Return the indefinite integral of a mathematical expression given
-- its symbolic representation.  It will return 'Nothing' if it is
-- unable to derive an integral.  This will not apply any simplification.
integrate :: Text -> Expression -> Maybe Expression
integrate v e =
  asum
    [ Polynomial.rationalIntegrate v e',
      Polynomial.symbolicIntegrate v e',
      Powers.integrate v e',
      Trigonometric.integrate v e',
      Substitution.integrate [Powers.integrate, Trigonometric.integrate] v e'
    ]
  where
    e' = simplify v e
