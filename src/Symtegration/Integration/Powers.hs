-- |
-- Module: Symtegration.Integration.Powers
-- Description: Integration of arbitrary powers of a variable.
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Powers (integrate) where

import Data.Text (Text)
import Symtegration.Symbolic

-- | Integrates powers of a variable.
-- In other words, expressions of the form \(x^c\),
-- where \(c\) is a constant.
integrate :: Text -> Expression -> Maybe Expression
integrate v (x :**: (Negate' (Number n))) = integrate v $ x :**: Number (-n)
integrate v (x@(Symbol s) :**: Number n)
  | s == v, -1 <- n = Just $ Log' x
  | s == v = Just $ (x :**: Number (n + 1)) :/: Number (n + 1)
  | otherwise = Nothing
integrate _ (_ :**: (_ :/: Number 0)) = Nothing
integrate v (x@(Symbol s) :**: y@(Number _ :/: Number _))
  | s == v = Just $ (x :**: (y :+: 1)) :/: (y :+: 1)
  | otherwise = Nothing
integrate _ _ = Nothing
