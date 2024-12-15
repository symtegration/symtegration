module Symtegration.Integration (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symtegration.Integration.Polynomial qualified as Polynomial
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Term qualified as Term
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify.RecursiveHeuristic

-- |
-- Return the indefinite integral of a mathematical expression given
-- its symbolic representation.  It will return 'Nothing' if it is
-- unable to derive an integral.
integrate :: Text -> Expression -> Maybe Expression
integrate var expr =
  asum $
    [f var expr | f <- directIntegrations]
      <> [Term.integrate directIntegrations var expr]

-- | List of direct integration functions.
-- These integrate expressions directly without using other integration functions.
directIntegrations :: [Text -> Expression -> Maybe Expression]
directIntegrations =
  [ Polynomial.rationalIntegrate,
    Polynomial.symbolicIntegrate,
    \v -> Powers.integrate v . simplify,
    Trigonometric.integrate
  ]
