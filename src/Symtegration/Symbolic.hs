module Symtegration.Symbolic
  ( Symbol (..),
    Expression (..),
    UnaryFunction (..),
    BinaryFunction (..),
  )
where

import Data.Ratio
import Data.Text

data Symbol = Variable Text | Constant Text | Number Integer
  deriving (Eq, Ord, Show, Read)

data Expression
  = Term Symbol
  | UnaryApply UnaryFunction Expression
  | BinaryApply BinaryFunction Expression Expression
  deriving (Eq, Ord, Show, Read)

data UnaryFunction
  = Negate
  | Abs
  | Signum
  | Exp
  | Log
  | Sqrt
  | Sin
  | Cos
  | Tan
  | Asin
  | Acos
  | Atan
  | Sinh
  | Cosh
  | Tanh
  | Asinh
  | Acosh
  | Atanh
  deriving (Eq, Ord, Show, Read)

data BinaryFunction
  = Add
  | Multiply
  | Subtract
  | Divide
  | Power
  | LogBase
  deriving (Eq, Ord, Show, Read)

instance Num Expression where
  (+) = BinaryApply Add
  (-) = BinaryApply Subtract
  (*) = BinaryApply Multiply
  negate = UnaryApply Negate
  abs = UnaryApply Abs
  signum = UnaryApply Signum
  fromInteger = Term . Number

instance Fractional Expression where
  (/) = BinaryApply Divide
  recip = BinaryApply Divide 1
  fromRational q = BinaryApply Divide n d
    where
      n = Term $ Number $ numerator q
      d = Term $ Number $ denominator q

instance Floating Expression where
  pi = Term $ Constant "pi"
  exp = UnaryApply Exp
  log = UnaryApply Log
  sqrt = UnaryApply Sqrt
  (**) = BinaryApply Power
  logBase = BinaryApply LogBase
  sin = UnaryApply Sin
  cos = UnaryApply Cos
  tan = UnaryApply Tan
  asin = UnaryApply Asin
  acos = UnaryApply Acos
  atan = UnaryApply Atan
  sinh = UnaryApply Sinh
  cosh = UnaryApply Cosh
  tanh = UnaryApply Tanh
  asinh = UnaryApply Asinh
  acosh = UnaryApply Acosh
  atanh = UnaryApply Atanh
