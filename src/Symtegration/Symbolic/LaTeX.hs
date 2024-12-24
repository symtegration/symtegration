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
toLaTeX :: Expression -> Text
toLaTeX (Number n) = showt n
toLaTeX (Symbol s) = s
toLaTeX (UnaryApply func x) = unary func x
toLaTeX (BinaryApply func x y) = binary func x y

-- | Converts unary functions into LaTeX.
unary :: UnaryFunction -> Expression -> Text
unary Negate x = "-" <> par (toLaTeX x)
unary Abs x = "\\left\\lvert " <> toLaTeX x <> " \\right\\rvert"
unary Signum x = "\\mathrm{signum}" <> par (toLaTeX x)
unary Exp x = "e^{" <> toLaTeX x <> "}"
unary Log x = "\\log" <> par (toLaTeX x)
unary Sqrt x = "\\sqrt{" <> toLaTeX x <> "}"
unary Sin x = "\\sin" <> par (toLaTeX x)
unary Cos x = "\\cos" <> par (toLaTeX x)
unary Tan x = "\\tan" <> par (toLaTeX x)
unary Asin x = "\\arcsin" <> par (toLaTeX x)
unary Acos x = "\\arccos" <> par (toLaTeX x)
unary Atan x = "\\arctan" <> par (toLaTeX x)
unary Sinh x = "\\sinh" <> par (toLaTeX x)
unary Cosh x = "\\cosh" <> par (toLaTeX x)
unary Tanh x = "\\tanh" <> par (toLaTeX x)
unary Asinh x = "\\sinh^{-1}" <> par (toLaTeX x)
unary Acosh x = "\\cosh^{-1}" <> par (toLaTeX x)
unary Atanh x = "\\tanh^{-1}" <> par (toLaTeX x)

-- | Converts binary functions into LaTeX.
binary :: BinaryFunction -> Expression -> Expression -> Text
binary Add x y = par (toLaTeX x) <> " + " <> par (toLaTeX y)
binary Multiply x y = par (toLaTeX x) <> " \\times " <> par (toLaTeX y)
binary Subtract x y = par (toLaTeX x) <> " - " <> par (toLaTeX y)
binary Divide x y = "\\frac{" <> toLaTeX x <> "}{" <> toLaTeX y <> "}"
binary Power x y = par (toLaTeX x) <> "^{" <> toLaTeX y <> "}"
binary LogBase x y = "\\log_{" <> toLaTeX x <> "}" <> par (toLaTeX y)

par :: Text -> Text
par s = "\\left(" <> s <> "\\right)"
