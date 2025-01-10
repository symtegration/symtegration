-- |
-- Module: Symtegration.Integration.Exponential
-- Description: Basic integration of exponential and logarithmic functions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Supports basic integration of exponential and logarithmic functions.
-- This does not support the integration of anything else,
-- even if it is trivial like integrating a constant.
module Symtegration.Integration.Exponential (integrate) where

import Data.Text (Text)
import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic.Haskell

-- | Integrates exponential and logarithmic functions required by the 'Floating' type class.
--
-- >>> toHaskell <$> integrate "x" (exp "x")
-- Just "exp x"
-- >>> toHaskell <$> integrate "x" (log "x")
-- Just "(x * log x) - x"
integrate :: Text -> Expression -> Maybe Expression
integrate _ (Number _) = Nothing
integrate _ (Symbol _) = Nothing
integrate v e@(Exp' (Symbol s))
  | v == s = Just e
  | otherwise = Nothing
integrate v (Log' e@(Symbol s))
  | v == s = Just $ (e :*: Log' e) :-: e
  | otherwise = Nothing
integrate v e@(Number n :**: Symbol s)
  | v == s = Just $ c :*: e
  | otherwise = Nothing
  where
    c = Number 1 :/: Log' (Number n)
integrate v (LogBase' (Number n) (Symbol s))
  | v == s = fmap (\x -> x :/: Log' (Number n)) $ integrate v $ Log' (Symbol s)
integrate _ _ = Nothing
