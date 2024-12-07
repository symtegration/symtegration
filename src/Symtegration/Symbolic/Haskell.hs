-- |
-- Module: Symtegration.Symbolic.Haskell
-- Description: Converts a symbolic representation of a mathematical expression into equivalent Haskell code.
-- Maintainer: dev@chungyc.org
--
-- Support for converting symbolic representations of mathematical expressions
-- into equivalent Haskell code.
module Symtegration.Symbolic.Haskell
  ( toText,
    -- * Support functions
    getUnaryFunctionText,
    getBinaryFunctionText,
  )
where

import Data.Text
import Symtegration.Symbolic
import TextShow (showt)

-- $setup
-- >>> import Symtegration.Symbolic

-- | Converts an 'Expression' into an equivalent Haskell expression.
--
-- >>> toText $ BinaryApply Add (Number 1) (Number 3)
-- "1 + 3"
-- >>> toText $ 1 + 3
-- "1 + 3"
--
-- Symbols are included without quotation.
--
-- >>> toText $ ("x" + "y") * 4
-- "(x + y) * 4"
toText :: Expression -> Text
toText (Number n) = showt n
toText (Symbol t) = t
toText (UnaryApply fun x) = funcText <> " " <> asArg x
  where
    funcText = getUnaryFunctionText fun
toText (BinaryApply LogBase x y) = funcText <> " " <> asArg x <> " " <> asArg y
  where
    funcText = getBinaryFunctionText LogBase
toText (BinaryApply op x y) = asArg x <> " " <> opText <> " " <> asArg y
  where
    opText = getBinaryFunctionText op

-- | Converts an 'Expression' to Haskell code appropriate for use as an argument.
-- In other words, show numbers and symbols as is, while surrounding everything
-- else in parentheses.
asArg :: Expression -> Text
asArg x@(Number _) = toText x
asArg x@(Symbol _) = toText x
asArg x = "(" <> toText x <> ")"

-- | Returns the corresponding Haskell function name.
getUnaryFunctionText :: UnaryFunction -> Text
getUnaryFunctionText Negate = "negate"
getUnaryFunctionText Abs = "abs"
getUnaryFunctionText Signum = "signum"
getUnaryFunctionText Exp = "exp"
getUnaryFunctionText Log = "log"
getUnaryFunctionText Sqrt = "sqrt"
getUnaryFunctionText Sin = "sin"
getUnaryFunctionText Cos = "cos"
getUnaryFunctionText Tan = "tan"
getUnaryFunctionText Asin = "asin"
getUnaryFunctionText Acos = "acos"
getUnaryFunctionText Atan = "atan"
getUnaryFunctionText Sinh = "sinh"
getUnaryFunctionText Cosh = "cosh"
getUnaryFunctionText Tanh = "tanh"
getUnaryFunctionText Asinh = "asinh"
getUnaryFunctionText Acosh = "acosh"
getUnaryFunctionText Atanh = "atanh"

-- | Returns the corresponding Haskell function name.
--
-- For binary operators, it will be the infix form.
-- In other words, @"+"@ will be returned for 'Add', not @"(+)"@.
getBinaryFunctionText :: BinaryFunction -> Text
getBinaryFunctionText Add = "+"
getBinaryFunctionText Multiply = "*"
getBinaryFunctionText Subtract = "-"
getBinaryFunctionText Divide = "/"
getBinaryFunctionText Power = "**"
getBinaryFunctionText LogBase = "logBase"
