-- |
-- Module: Symtegration.Symbolic.LaTeX
-- Description: Converts a symbolic representation of a mathematical expression into equivalent LaTeX text.
-- Maintainer: dev@chungyc.org
--
-- Support for converting symbolic representations of mathematical expressions
-- into equivalent LaTeX text.
module Symtegration.Symbolic.LaTeX (toLaTeX) where

import Data.Text (Text)
import Data.Text qualified as Text
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
toLaTeX (Symbol s)
  | LT <- Text.compareLength s 2 = s
  | otherwise = "\\mathrm{" <> s <> "}"
toLaTeX (UnaryApply func x) = unary func x
toLaTeX (BinaryApply func x y) = binary func x y

-- | Converts unary functions into LaTeX.
unary :: UnaryFunction -> Expression -> Text
unary Negate x = "-\\left(" <> toLaTeX x <> "\\right)"
unary Abs x = "\\left\\lvert " <> toLaTeX x <> " \\right\\rvert"
unary Signum x = "\\mathrm{signum}\\left(" <> toLaTeX x <> "\\right)"
unary Exp x = "e^{" <> toLaTeX x <> "}"
unary Log x = "\\log\\left(" <> toLaTeX x <> "\\right)"
unary Sqrt x = "\\sqrt{" <> toLaTeX x <> "}"
unary Sin x = "\\sin\\left(" <> toLaTeX x <> "\\right)"
unary Cos x = "\\cos\\left(" <> toLaTeX x <> "\\right)"
unary Tan x = "\\tan\\left(" <> toLaTeX x <> "\\right)"
unary Asin x = "\\arcsin\\left(" <> toLaTeX x <> "\\right)"
unary Acos x = "\\arccos\\left(" <> toLaTeX x <> "\\right)"
unary Atan x = "\\arctan\\left(" <> toLaTeX x <> "\\right)"
unary Sinh x = "\\sinh\\left(" <> toLaTeX x <> "\\right)"
unary Cosh x = "\\cosh\\left(" <> toLaTeX x <> "\\right)"
unary Tanh x = "\\tanh\\left(" <> toLaTeX x <> "\\right)"
unary Asinh x = "\\sinh^{-1}\\left(" <> toLaTeX x <> "\\right)"
unary Acosh x = "\\cosh^{-1}\\left(" <> toLaTeX x <> "\\right)"
unary Atanh x = "\\tanh^{-1}\\left(" <> toLaTeX x <> "\\right)"

-- | Converts binary functions into LaTeX.
binary :: BinaryFunction -> Expression -> Expression -> Text
binary Add x y = "\\left(" <> toLaTeX x <> "\\right) + \\left(" <> toLaTeX y <> "\\right)"
binary Multiply x y = "\\left(" <> toLaTeX x <> "\\right) \\times \\left(" <> toLaTeX y <> "\\right)"
binary Subtract x y = "\\left(" <> toLaTeX x <> "\\right) - \\left(" <> toLaTeX y <> "\\right)"
binary Divide x y = "\\frac{" <> toLaTeX x <> "}{" <> toLaTeX y <> "}"
binary Power x y = "\\left(" <> toLaTeX x <> "\\right)^{" <> toLaTeX y <> "}"
binary LogBase x y = "\\log_{" <> toLaTeX x <> "}\\left(" <> toLaTeX y <> "\\right)"
