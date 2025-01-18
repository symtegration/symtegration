-- |
-- Module: Symtegration.Integration.Powers
-- Description: Integration of arbitrary powers of a variable.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Powers (integrate) where

import Data.Text (Text)
import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify

-- | Integrates powers of a variable.
-- In other words, expressions of the form \(x^c\),
-- where \(c\) is a constant.
--
-- >>> toHaskell . simplify <$> integrate "x" "x"
-- Just "1 / 2 * x ** 2"
-- >>> toHaskell . simplify <$> integrate "x" ("x" ** (1/2))
-- Just "(2 * x ** (3 / 2)) / 3"
-- >>> toHaskell . simplify <$> integrate "x" ("x" ** (-1))
-- Just "log x"
integrate :: Text -> Expression -> Maybe Expression
integrate v (1 :/: Symbol s) =
  integrate v $ Symbol s :**: Number (-1)
integrate v (x :**: (Negate' (Number n :/: Number m))) =
  integrate v $ x :**: (Number (-n) :/: Number m)
integrate v (x :**: (Negate' (Number n))) =
  integrate v $ x :**: Number (-n)
integrate v e@(Number _) = Just $ e :*: Symbol v
integrate v e@(Symbol v')
  | v == v' = Just $ (Number 1 :/: Number 2) :*: (e :**: 2)
  | otherwise = Just $ e :*: Symbol v
integrate v (x@(Symbol s) :**: Number n)
  | s == v, -1 <- n = Just $ Log' x
  | s == v = Just $ (x :**: Number (n + 1)) :/: Number (n + 1)
  | otherwise = Nothing
integrate _ (_ :**: (_ :/: Number 0)) = Nothing
integrate v (x@(Symbol s) :**: y@(Number _ :/: Number _))
  | s == v = Just $ (x :**: (y :+: 1)) :/: (y :+: 1)
  | otherwise = Nothing
integrate _ _ = Nothing
