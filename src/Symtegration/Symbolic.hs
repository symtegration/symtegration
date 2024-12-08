-- |
-- Module: Symtegration.Symbolic
-- Description: Library for symbolically representing mathematical expressions.
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic
  ( -- * Representation
    Expression (..),
    UnaryFunction (..),
    BinaryFunction (..),

    -- * Computation
    getUnaryFunction,
    getBinaryFunction,
    evaluate,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ratio
import Data.String (IsString, fromString)
import Data.Text
import GHC.Generics (Generic)

-- | Symbolic representation of a mathematical expression.
--
-- >>> 2 :: Expression
-- Number 2
-- >>> "x" :: Expression
-- Symbol "x"
-- >>> 2 + sin "x" :: Expression
-- BinaryApply Add (Number 2) (UnaryApply Sin (Symbol "x"))
data Expression
  = -- | Represents a concrete number.
    Number Integer
  | -- | Represents a symbol, which could either be a variable or a constant.
    Symbol Text
  | -- | Represents the application of an unary function.
    UnaryApply UnaryFunction Expression
  | -- | Represents the application of a binary function.
    BinaryApply BinaryFunction Expression Expression
  deriving (Eq, Ord, Show, Read, Generic)

-- | Symbolic representation for unary functions.
data UnaryFunction
  = -- | 'negate'
    Negate
  | -- | 'abs'
    Abs
  | -- | 'signum'
    Signum
  | -- | 'exp'
    Exp
  | -- | 'log'
    Log
  | -- | 'sqrt'
    Sqrt
  | -- | 'sin'
    Sin
  | -- | 'cos'
    Cos
  | -- | 'tan'
    Tan
  | -- | 'asin'
    Asin
  | -- | 'acos'
    Acos
  | -- | 'atan'
    Atan
  | -- | 'sinh'
    Sinh
  | -- | 'cosh'
    Cosh
  | -- | 'tanh'
    Tanh
  | -- | 'asinh'
    Asinh
  | -- | 'acosh'
    Acosh
  | -- | 'atanh'
    Atanh
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

-- | Symbolic representation for binary functions.
data BinaryFunction
  = -- | '(+)'
    Add
  | -- | '(*)'
    Multiply
  | -- | '(-)'
    Subtract
  | -- | '(/)'
    Divide
  | -- | '(**)'
    Power
  | -- | 'logBase'
    LogBase
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance IsString Expression where
  fromString = Symbol . fromString

instance Num Expression where
  (+) = BinaryApply Add
  (-) = BinaryApply Subtract
  (*) = BinaryApply Multiply
  negate = UnaryApply Negate
  abs = UnaryApply Abs
  signum = UnaryApply Signum
  fromInteger = Number

instance Fractional Expression where
  (/) = BinaryApply Divide
  fromRational q = BinaryApply Divide n d
    where
      n = Number $ numerator q
      d = Number $ denominator q

instance Floating Expression where
  pi = Symbol "pi"
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

-- | Returns a function corresponding to the symbolic representation of an unary function.
--
-- >>> (getUnaryFunction Cos) pi == (cos pi :: Double)
-- True
getUnaryFunction :: (Floating a) => UnaryFunction -> (a -> a)
getUnaryFunction Negate = negate
getUnaryFunction Abs = abs
getUnaryFunction Signum = signum
getUnaryFunction Exp = exp
getUnaryFunction Log = log
getUnaryFunction Sqrt = sqrt
getUnaryFunction Sin = sin
getUnaryFunction Cos = cos
getUnaryFunction Tan = tan
getUnaryFunction Asin = asin
getUnaryFunction Acos = acos
getUnaryFunction Atan = atan
getUnaryFunction Sinh = sinh
getUnaryFunction Cosh = cosh
getUnaryFunction Tanh = tanh
getUnaryFunction Asinh = asinh
getUnaryFunction Acosh = acosh
getUnaryFunction Atanh = atanh

-- | Returns a function corresponding to the symbolic representation of a binary function.
--
-- >>> (getBinaryFunction Add) 2 5 == (2 + 5 :: Double)
-- True
getBinaryFunction :: (Floating a) => BinaryFunction -> (a -> a -> a)
getBinaryFunction Add = (+)
getBinaryFunction Multiply = (*)
getBinaryFunction Subtract = (-)
getBinaryFunction Divide = (/)
getBinaryFunction Power = (**)
getBinaryFunction LogBase = logBase

-- | Calculates the value for a mathematical expression for a given assignment of values to symbols.
--
-- For example, when \(x=5\), then \(2x+1=11\).
--
-- >>> import Data.Map qualified as Map
-- >>> evaluate (2 * "x" + 1) (Map.singleton "x" 5)
-- Just 11.0
--
-- All symbols except for @"pi"@ in a mathematical expression must be assigned a value.
-- Otherwise, a value cannot be computed.
--
-- >>> evaluate (2 * "x" + 1) Map.empty
-- Nothing
--
-- The symbol @"pi"@ is always used to represent \(\pi\),
-- and any assignment to @"pi"@ will be ignored.
-- For example, the following is \(\pi - \pi\), not \(100 - \pi\).
--
-- >>> evaluate ("pi" - pi) (Map.singleton "pi" 100)
-- Just 0.0
evaluate ::
  (Floating a) =>
  -- | Mathematical expression to evaluate.
  Expression ->
  -- | Map of symbols to concrete values.
  Map Text a ->
  -- | Evaluation result.
  Maybe a
evaluate (Number n) _ = Just $ fromInteger n
evaluate (Symbol "pi") _ = Just pi
evaluate (Symbol x) m = Map.lookup x m
evaluate (UnaryApply fun expr) m = fmap f v
  where
    f = getUnaryFunction fun
    v = evaluate expr m
evaluate (BinaryApply fun expr1 expr2) m = f <$> v1 <*> v2
  where
    f = getBinaryFunction fun
    v1 = evaluate expr1 m
    v2 = evaluate expr2 m
