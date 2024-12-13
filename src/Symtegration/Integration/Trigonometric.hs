-- |
-- Module: Symtegration.Integration.Trigonometric
-- Description: Basic integration of trigonometric functions.
-- Maintainer: dev@chungyc.org
--
-- Supports basic integration of trigonometric functions.
-- Trigonometric functions on just the variable and
-- those multiplied by a constant can be integrated.
-- This does not support the integration of anything else,
-- even if it is trivial like integrating a constant.
module Symtegration.Integration.Trigonometric (integrate) where

import Data.Text (Text)
import Symtegration.Symbolic

integrate :: Text -> Expression -> Maybe Expression
integrate _ (Number _) = Nothing
integrate _ (Symbol _) = Nothing
integrate v (Negate' x) = UnaryApply Negate <$> integrate v x
integrate v (Sin' (Symbol s))
  | s == v = Just $ Cos' (Symbol s)
  | otherwise = Nothing
integrate v (Cos' (Symbol s))
  | s == v = Just $ Negate' $ Sin' (Symbol s)
  | otherwise = Nothing
integrate v (Tan' (Symbol s))
  | s == v = Just $ Negate' $ Log' (Cos' (Symbol s))
  | otherwise = Nothing
integrate _ _ = Nothing
