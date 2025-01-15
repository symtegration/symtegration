-- |
-- Module: Symtegtarion.Symbolic.Simplify.Tidy
-- Description: Tidy up a simplified mathematical expression.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.Tidy (tidy) where

import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic
-- >>> import Symtegration.Symbolic.Haskell

-- | Tidies up expressions for nicer output.
--
-- Assumes that other simplifications have been applied first.
-- In fact, it may undo changes that made other simplifications easier.
--
-- ==== __What is tidied up__
--
-- This section shows examples of what this function tidies up.
--
-- >>> toHaskell $ tidy $ "x" + negate "y"
-- "x - y"
--
-- >>> toHaskell $ tidy $ "x" + Number (-2) * "y"
-- "x - 2 * y"
--
-- >>> toHaskell $ tidy $ Number (-1) / Number 2
-- "negate (1 / 2)"
--
-- >>> toHaskell $ tidy $ Number (-1) * "x"
-- "negate x"
--
-- >>> toHaskell $ tidy $ (-"x") * "y"
-- "negate (x * y)"
--
-- >>> toHaskell $ tidy $ "x" * (-"y")
-- "negate (x * y)"
--
-- >>> toHaskell $ tidy $ (-"x") * (-"y")
-- "x * y"
--
-- >>> toHaskell $ tidy $ "x" + ((-"y") + "z")
-- "x - y + z"
tidy :: Expression -> Expression
tidy (UnaryApply func x) = unary $ UnaryApply func $ tidy x
tidy (BinaryApply func x y) = binary $ BinaryApply func (tidy x) (tidy y)
tidy e = e

unary :: Expression -> Expression
unary e = e

binary :: Expression -> Expression
binary (x :+: (Negate' y)) = x :-: y
binary (Number (-1) :*: x) = Negate' x
binary e@(Number n :*: x)
  | n < 0 = Negate' (Number (-n) :*: x)
  | otherwise = e
binary e@(Number n :/: x)
  | n < 0 = Negate' (Number (-n) :/: x)
  | otherwise = e
binary (Negate' x :*: Negate' y) = x :*: y
binary (Negate' x :*: y) = Negate' $ x :*: y
binary (x :*: Negate' y) = Negate' $ x :*: y
binary (x :+: (Negate' y :+: z)) = (x :-: y) :+: z
binary e = e
