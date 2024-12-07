module Symtegration.Symbolic
  ( Expression (..),
    UnaryFunction (..),
    BinaryFunction (..),
    getUnaryFunction,
    getBinaryFunction,
    evaluate,
  )
where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Ratio
import Data.String (IsString, fromString)
import Data.Text

-- |
--
-- >>> import Numeric.AD
-- >>> diff (\x -> x + 1) ("x" :: Expression)
-- Number 1
data Expression
  = Number Integer
  | Symbol Text
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

getBinaryFunction :: (Floating a) => BinaryFunction -> (a -> a -> a)
getBinaryFunction Add = (+)
getBinaryFunction Multiply = (*)
getBinaryFunction Subtract = (-)
getBinaryFunction Divide = (/)
getBinaryFunction Power = (**)
getBinaryFunction LogBase = logBase

-- |
--
-- >>> import Data.Map qualified as M
-- >>> evaluate (2 * "x" + 1) M.empty
-- Nothing
-- >>> evaluate (2 * "x" + 1) (M.singleton "x" 5)
-- Just 11.0
evaluate :: (Floating a) => Expression -> Map Text a -> Maybe a
evaluate (Number n) _ = Just $ fromInteger n
evaluate (Symbol "pi") _ = Just pi
evaluate (Symbol x) m = M.lookup x m
evaluate (UnaryApply fun expr) m = fmap f v
  where
    f = getUnaryFunction fun
    v = evaluate expr m
evaluate (BinaryApply fun expr1 expr2) m = f <$> v1 <*> v2
  where
    f = getBinaryFunction fun
    v1 = evaluate expr1 m
    v2 = evaluate expr2 m
