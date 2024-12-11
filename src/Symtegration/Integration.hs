module Symtegration.Integration (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symtegration.Integration.Polynomial qualified as Polynomial
import Symtegration.Symbolic

-- |
-- Return the indefinite integral of a mathematical expression given
-- its symbolic representation.  It will return 'Nothing' if it is
-- unable to derive an integral.
integrate :: Text -> Expression -> Maybe Expression
integrate var expr =
  asum
    [ Polynomial.rationalIntegrate var expr,
      Polynomial.symbolicIntegrate var expr
    ]
