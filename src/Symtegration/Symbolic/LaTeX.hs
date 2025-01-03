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
toLaTeX (Symbol "pi") = "\\pi"
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
unary Log x = "\\log " <> asNamedFunctionArg x
unary Sqrt x = "\\sqrt" <> brace (toLaTeX x)
unary Sin x = "\\sin " <> asNamedFunctionArg x
unary Cos x = "\\cos " <> asNamedFunctionArg x
unary Tan x = "\\tan " <> asNamedFunctionArg x
unary Asin x = "\\sin^{-1} " <> asNamedFunctionArg x
unary Acos x = "\\cos^{-1} " <> asNamedFunctionArg x
unary Atan x = "\\tan^{-1} " <> asNamedFunctionArg x
unary Sinh x = "\\sinh " <> asNamedFunctionArg x
unary Cosh x = "\\cosh " <> asNamedFunctionArg x
unary Tanh x = "\\tanh " <> asNamedFunctionArg x
unary Asinh x = "\\sinh^{-1} " <> asNamedFunctionArg x
unary Acosh x = "\\cosh^{-1} " <> asNamedFunctionArg x
unary Atanh x = "\\tanh^{-1} " <> asNamedFunctionArg x

-- | Converts binary functions into LaTeX.
binary :: BinaryFunction -> Expression -> Expression -> Text
binary Add x y = asArg x <> " + " <> asArg y
binary Multiply x y@(Number _) = asArg x <> " \\times " <> asArg y
binary Multiply x y = asArg x <> " " <> asArg y
binary Subtract x y = asArg x <> " - " <> asArg y
binary Divide x y = "\\frac" <> brace (toLaTeX x) <> brace (toLaTeX y)
binary Power x y = asArg x <> "^" <> brace (toLaTeX y)
binary LogBase x y = "\\log_" <> brace (toLaTeX x) <> asNamedFunctionArg y

asArg :: Expression -> Text
asArg e@(Number n) | n >= 0 = toLaTeX e | otherwise = par $ toLaTeX e
asArg e@(Symbol _) = toLaTeX e
asArg e@(Negate' _) = par $ toLaTeX e
asArg e@(UnaryApply _ _) = toLaTeX e
asArg e@(_ :/: _) = toLaTeX e
asArg e@(Number _ :**: _) = par $ toLaTeX e
asArg e@(_ :**: _) = toLaTeX e
asArg e = par $ toLaTeX e

-- For arguments to named functions such as "sin" which do not always delimit their arguments.
-- E.g., it is preferred that "1 + sin x" be "1 + sin x" and not "1 + (sin x)",
-- but we want "cos (sin x)" to be "cos (sin x)" and not "cos sin x".
asNamedFunctionArg :: Expression -> Text
asNamedFunctionArg e@(Exp' _) = asArg e
asNamedFunctionArg e@(Abs' _) = asArg e
asNamedFunctionArg e@(Sqrt' _) = asArg e
asNamedFunctionArg e@(UnaryApply _ _) = par $ toLaTeX e
asNamedFunctionArg e@(LogBase' _ _) = par $ toLaTeX e
asNamedFunctionArg e = asArg e

par :: Text -> Text
par s = "\\left(" <> s <> "\\right)"

brace :: Text -> Text
brace s = "{" <> s <> "}"
