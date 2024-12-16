-- |
-- Module: Symtegration.Integration.Term
-- Description: Integrates individual terms in an expression separately.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Term (integrate) where

import Data.Foldable (asum)
import Data.Text (Text)
import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify.RecursiveHeuristic

-- | Integrate term by term, using direct methods on each term.
--
-- >>> import Symtegration.Integration.Polynomial qualified as P
-- >>> import Symtegration.Integration.Trigonometric qualified as T
-- >>> let f = "x" + sin "x"
-- >>> P.rationalIntegrate "x" f
-- Nothing
-- >>> T.integrate "x" f
-- Nothing
-- >>> let g = integrate [P.rationalIntegrate, T.integrate] "x" f
-- >>> toHaskell . simplify <$> g
-- Just "(1 / 2) * (x ** 2) + (negate (cos x))"
integrate ::
  -- | Functions for directly integrating each term.
  [Text -> Expression -> Maybe Expression] ->
  -- | The variable being integrated over.
  Text ->
  -- | The expression being integrated.
  Expression ->
  -- | The integral, if successful.
  Maybe Expression
integrate fs v (Negate' x) =
  UnaryApply Negate <$> integrate fs v x
integrate fs v (x :-: y) =
  integrate fs v (x :+: Negate' y)
integrate fs v (x@(_ :+: _) :+: y@(_ :+: _)) =
  BinaryApply Add <$> integrate fs v x <*> integrate fs v y
integrate fs v (x@(_ :+: _) :+: y) =
  BinaryApply Add <$> integrate fs v x <*> asum [f v y | f <- fs]
integrate fs v (x :+: y@(_ :+: _)) =
  BinaryApply Add <$> asum [f v x | f <- fs] <*> integrate fs v y
integrate fs v (x :+: y) =
  BinaryApply Add <$> asum [f v x | f <- fs] <*> asum [f v y | f <- fs]
integrate _ _ _ = Nothing
