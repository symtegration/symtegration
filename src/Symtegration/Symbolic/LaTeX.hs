-- |
-- Module: Symtegration.Symbolic.LaTeX
-- Description: Converts a symbolic representation of a mathematical expression into equivalent LaTeX text.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Support for converting symbolic representations of mathematical expressions
-- into equivalent LaTeX text.
module Symtegration.Symbolic.LaTeX (toLaTeX) where

import Data.Text (Text)
import Symtegration.Symbolic
import TextShow (showt)

-- | Converts an 'Expression' into an equivalent LaTeX expression.
--
-- >>> toLaTeX $ exp 5
-- "e^{5}"
--
-- Symbols are included without quotation.
--
-- >>> toLaTeX $ exp "x"
-- "e^{x}"
-- >>> toLaTeX $ "x" + 4 * sin "y"
-- "x + 4 \\sin y"
toLaTeX :: Expression -> Text
toLaTeX (Number n) = showt n
toLaTeX (Symbol s) = s
toLaTeX (UnaryApply func x) = unary func x
toLaTeX (x@(_ :*: _) :+: y@(_ :*: _)) = toLaTeX x <> " + " <> toLaTeX y
toLaTeX (x@(_ :*: _) :+: y@(_ :+: _)) = toLaTeX x <> " + " <> toLaTeX y
toLaTeX (x@(_ :+: _) :+: y@(_ :*: _)) = toLaTeX x <> " + " <> toLaTeX y
toLaTeX (x :+: y@(_ :*: _)) = asArg x <> " + " <> toLaTeX y
toLaTeX (x@(_ :*: _) :+: y) = toLaTeX x <> " + " <> asArg y
toLaTeX (x@(_ :+: _) :+: y@(_ :+: _)) = toLaTeX x <> " + " <> toLaTeX y
toLaTeX (x@(_ :+: _) :+: y) = toLaTeX x <> " + " <> asArg y
toLaTeX (x :+: y@(_ :+: _)) = asArg x <> " + " <> toLaTeX y
toLaTeX (x@(_ :*: Number _) :*: y@(Number _ :*: _)) = toLaTeX x <> " \\times " <> toLaTeX y
toLaTeX (x@(Number _) :*: y@(Number _ :*: _)) = toLaTeX x <> " \\times " <> toLaTeX y
toLaTeX (x@(_ :*: Number _) :*: y@(Number _)) = toLaTeX x <> " \\times " <> toLaTeX y
toLaTeX (x@(Abs' _) :*: y@(Symbol _)) = toLaTeX x <> " " <> toLaTeX y
toLaTeX (x@(Signum' _) :*: y@(Symbol _)) = toLaTeX x <> " " <> toLaTeX y
toLaTeX (x@(Exp' _) :*: y@(Symbol _)) = toLaTeX x <> " " <> toLaTeX y
toLaTeX (x@(UnaryApply _ _) :*: y@(Symbol _)) = par (toLaTeX x) <> " " <> toLaTeX y
toLaTeX (x@(LogBase' _ _) :*: y@(Symbol _)) = par (toLaTeX x) <> " " <> toLaTeX y
toLaTeX (x@(_ :*: _) :*: y@(_ :*: _)) = toLaTeX x <> " " <> toLaTeX y
toLaTeX (x :*: y@(_ :*: _)) = asArg x <> " " <> toLaTeX y
toLaTeX (x@(_ :*: _) :*: y) = toLaTeX x <> " " <> asArg y
toLaTeX (BinaryApply func x y) = binary func x y

-- | Converts unary functions into LaTeX.
unary :: UnaryFunction -> Expression -> Text
unary Negate x = "-" <> asArg x
unary Abs x = "\\left\\lvert " <> toLaTeX x <> " \\right\\rvert"
unary Signum x = "\\mathrm{signum}" <> par (toLaTeX x)
unary Exp x = "e^" <> brace (toLaTeX x)
unary Log x = "\\log " <> asArg x
unary Sqrt x = "\\sqrt" <> brace (toLaTeX x)
unary Sin x = "\\sin " <> asArg x
unary Cos x = "\\cos " <> asArg x
unary Tan x = "\\tan " <> asArg x
unary Asin x = "\\sin^{-1} " <> asArg x
unary Acos x = "\\cos^{-1} " <> asArg x
unary Atan x = "\\tan^{-1} " <> asArg x
unary Sinh x = "\\sinh " <> asArg x
unary Cosh x = "\\cosh " <> asArg x
unary Tanh x = "\\tanh " <> asArg x
unary Asinh x = "\\sinh^{-1} " <> asArg x
unary Acosh x = "\\cosh^{-1} " <> asArg x
unary Atanh x = "\\tanh^{-1} " <> asArg x

-- | Converts binary functions into LaTeX.
binary :: BinaryFunction -> Expression -> Expression -> Text
binary Add x y = asArg x <> " + " <> asArg y
binary Multiply x y@(Number _) = asArg x <> " \\times " <> asArg y
binary Multiply x y = asArg x <> " " <> asArg y
binary Subtract x y = asArg x <> " - " <> asArg y
binary Divide x y = "\\frac" <> brace (toLaTeX x) <> brace (toLaTeX y)
binary Power x y = asArg x <> "^" <> brace (toLaTeX y)
binary LogBase x y = "\\log_" <> brace (toLaTeX x) <> asArg y

asArg :: Expression -> Text
asArg e@(Number n) | n >= 0 = toLaTeX e | otherwise = par $ toLaTeX e
asArg e@(Symbol _) = toLaTeX e
asArg e@(Negate' _) = par $ toLaTeX e
asArg e@(UnaryApply _ _) = toLaTeX e
asArg e@(_ :/: _) = toLaTeX e
asArg e@(Number _ :**: _) = par $ toLaTeX e
asArg e@(_ :**: _) = toLaTeX e
asArg e = par $ toLaTeX e

par :: Text -> Text
par s = "\\left(" <> s <> "\\right)"

brace :: Text -> Text
brace s = "{" <> s <> "}"
