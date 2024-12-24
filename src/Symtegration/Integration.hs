-- |
-- Module: Symtegration.Integration
-- Description: Symbolically integrates mathematical expressions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symtegration.Integration.Exponential qualified as Exponential
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Substitution qualified as Substitution
import Symtegration.Integration.Sum qualified as Sum
import Symtegration.Integration.Term qualified as Term
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- |
-- Return the indefinite integral of a mathematical expression given
-- its symbolic representation.  It will return 'Nothing' if it is
-- unable to derive an integral.  This will not apply any simplification.
integrate :: Text -> Expression -> Maybe Expression
integrate v e = asum $ map (\f -> f v e') withTermSum
  where
    e' = simplify v e

-- | Functions which directly integrate.
base :: [Text -> Expression -> Maybe Expression]
base = [Powers.integrate, Exponential.integrate, Trigonometric.integrate]

-- | Includes integration of a term using other integration functions.
withTerm :: [Text -> Expression -> Maybe Expression]
withTerm = base ++ [Term.integrate base, Substitution.integrate base]

-- | Includes integration of a sum of terms.
withTermSum :: [Text -> Expression -> Maybe Expression]
withTermSum = withTerm ++ [Sum.integrate withTerm]
