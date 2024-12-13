{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module: Symtegration.Symbolic
-- Description: Library for symbolically representing mathematical expressions.
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic
  ( -- * Representation
    Expression (..),
    UnaryFunction (..),
    BinaryFunction (..),

    -- * Manipulation
    substitute,

    -- * Computation
    evaluate,
    fractionalEvaluate,
    getUnaryFunction,
    getBinaryFunction,

    -- * Pattern synonyms

    -- | Pattern synonyms are defined to make it more convenient to pattern match on 'Expression'.

    -- ** Unary functions
    pattern Negate',
    pattern Abs',
    pattern Signum',
    pattern Exp',
    pattern Log',
    pattern Sqrt',
    pattern Sin',
    pattern Cos',
    pattern Tan',
    pattern Asin',
    pattern Acos',
    pattern Atan',
    pattern Sinh',
    pattern Cosh',
    pattern Tanh',
    pattern Asinh',
    pattern Acosh',
    pattern Atanh',

    -- ** Binary functions
    pattern (:+:),
    pattern (:*:),
    pattern (:-:),
    pattern (:/:),
    pattern (:**:),
    pattern LogBase',
  )
where

import Data.Ratio
import Data.String (IsString, fromString)
import Data.Text
import GHC.Generics (Generic)
import TextShow (TextShow)
import TextShow.Generic (FromGeneric (..))

-- $setup
-- >>> :set -XLambdaCase
-- >>> import Symtegration

-- | Symbolic representation of a mathematical expression.
-- It is an instance of the 'Num', 'Fractional', and 'Floating' type classes,
-- so normal Haskell expressions can be used, although the expressions
-- are limited to using the functions defined by these type classses.
-- The type is also an instance of the 'IsString' type class,
-- so symbols can be expressed as Haskell string with the @OverloadedStrings@ extension.
-- The structure of these values is intended to be visible.
--
-- >>> 2 :: Expression
-- Number 2
-- >>> "x" :: Expression
-- Symbol "x"
-- >>> 2 + sin "x" :: Expression
-- BinaryApply Add (Number 2) (UnaryApply Sin (Symbol "x"))
--
-- A somewhat more concise representation can be obtained using 'Symtegration.toHaskellText':
--
-- >>> toHaskellText $ 2 * "y" + sin "x"
-- "2 * y + (sin x)"
data Expression
  = -- | Represents a concrete number.
    Number Integer
  | -- | Represents a symbol, which could either be a variable or a constant.
    Symbol Text
  | -- | Represents the application of an unary function.
    UnaryApply UnaryFunction Expression
  | -- | Represents the application of a binary function.
    BinaryApply BinaryFunction Expression Expression
  deriving
    ( -- | Structural equality, not semantic equality.
      -- E.g., @"a" - "a" /= 0@.
      Eq,
      Show,
      Read,
      Generic
    )
  deriving (TextShow) via FromGeneric Expression

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
  deriving (Eq, Enum, Bounded, Show, Read, Generic)
  deriving (TextShow) via FromGeneric UnaryFunction

pattern Negate', Abs', Signum', Exp', Log', Sqrt', Sin', Cos', Tan', Asin', Acos', Atan', Sinh', Cosh', Tanh', Asinh', Acosh', Atanh' :: Expression -> Expression
pattern Negate' x = UnaryApply Negate x
pattern Abs' x = UnaryApply Abs x
pattern Signum' x = UnaryApply Signum x
pattern Exp' x = UnaryApply Exp x
pattern Log' x = UnaryApply Log x
pattern Sqrt' x = UnaryApply Sqrt x
pattern Sin' x = UnaryApply Sin x
pattern Cos' x = UnaryApply Cos x
pattern Tan' x = UnaryApply Tan x
pattern Asin' x = UnaryApply Asin x
pattern Acos' x = UnaryApply Acos x
pattern Atan' x = UnaryApply Atan x
pattern Sinh' x = UnaryApply Sinh x
pattern Cosh' x = UnaryApply Cosh x
pattern Tanh' x = UnaryApply Tanh x
pattern Asinh' x = UnaryApply Asinh x
pattern Acosh' x = UnaryApply Acosh x
pattern Atanh' x = UnaryApply Atanh x

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
  deriving (Eq, Enum, Bounded, Show, Read, Generic)
  deriving (TextShow) via FromGeneric BinaryFunction

pattern (:+:), (:*:), (:-:), (:/:), (:**:), LogBase' :: Expression -> Expression -> Expression
pattern x :+: y = BinaryApply Add x y
pattern x :*: y = BinaryApply Multiply x y
pattern x :-: y = BinaryApply Subtract x y
pattern x :/: y = BinaryApply Divide x y
pattern x :**: y = BinaryApply Power x y
pattern LogBase' x y = BinaryApply LogBase x y

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

-- | Substitute the symbols with the corresponding expressions they are mapped to.
-- The symbols will be replaced as is; there is no special treatment if the
-- expression they are replaced by also contains the same symbol.
--
-- >>> toHaskellText $ substitute ("x" + "y") (\case "x" -> Just ("a" * "b"); "y" -> Just 4)
-- "a * b + 4"
substitute ::
  -- | Expression to apply substitution.
  Expression ->
  -- | Maps symbols to expressions they are to be substituted with.
  (Text -> Maybe Expression) ->
  -- | Expression with substitution applied.
  Expression
substitute e@(Number _) _ = e
substitute e@(Symbol s) f
  | (Just x) <- f s = x
  | otherwise = e
substitute (UnaryApply func x) f = UnaryApply func (substitute x f)
substitute (BinaryApply func x y) f = BinaryApply func (substitute x f) (substitute y f)

-- | Calculates the value for a mathematical expression for a given assignment of values to symbols.
--
-- For example, when \(x=5\), then \(2x+1=11\).
--
-- >>> import Data.Map qualified as Map
-- >>> evaluate (2 * "x" + 1) (\case "x" -> Just 5)
-- Just 11.0
--
-- All symbols except for @"pi"@ in a mathematical expression must be assigned a value.
-- Otherwise, a value cannot be computed.
--
-- >>> evaluate (2 * "x" + 1) (const Nothing)
-- Nothing
--
-- The symbol @"pi"@ is always used to represent \(\pi\),
-- and any assignment to @"pi"@ will be ignored.
-- For example, the following is \(\pi - \pi\), not \(100 - \pi\).
--
-- >>> evaluate ("pi" - pi) (\case "x" -> Just 100)
-- Just 0.0
evaluate ::
  (Floating a) =>
  -- | Mathematical expression to evaluate.
  Expression ->
  -- | Maps symbols to concrete values.
  (Text -> Maybe a) ->
  -- | Evaluation result.
  Maybe a
evaluate (Number n) _ = Just $ fromInteger n
evaluate (Symbol "pi") _ = Just pi
evaluate (Symbol x) m = m x
evaluate (UnaryApply fun expr) m = fmap f v
  where
    f = getUnaryFunction fun
    v = evaluate expr m
evaluate (BinaryApply fun expr1 expr2) m = f <$> v1 <*> v2
  where
    f = getBinaryFunction fun
    v1 = evaluate expr1 m
    v2 = evaluate expr2 m

-- |
-- Evaluates a mathematical expression with only operations available to 'Fractional' values.
-- In particular, this allows exact evaluations with 'Rational' values.
-- 'Nothing' will be returned if a function not supported by all 'Fractional' values
-- is used by the mathematical expression.
--
-- As an exception, the '(**)' operator is allowed with constant integer exponents,
-- even though '(**)' is not a function applicable to all 'Fractional' types.
--
-- For example,
--
-- >>> let p = 1 / (3 * "x"**5 - 2 * "x" + 1) :: Expression
-- >>> fractionalEvaluate p (\case "x" -> Just (2 / 7 :: Rational))
-- Just (16807 % 7299)
--
-- Compare against 'evaluate', which cannot even use 'Rational' computations
-- because 'Rational' is not an instance of the 'Floating' type class:
--
-- >>> evaluate p (\case "x" -> Just (2 / 7 :: Double))
-- Just 2.3026441978353196
fractionalEvaluate ::
  (Eq a, Fractional a) =>
  -- | Mathematical expression to evaluate.
  Expression ->
  -- | Maps symbols to concrete values.
  (Text -> Maybe a) ->
  -- | Evaluation result.
  Maybe a
fractionalEvaluate (Number n) _ = Just $ fromInteger n
fractionalEvaluate (Symbol x) m = m x
fractionalEvaluate (Negate' x) m = negate <$> fractionalEvaluate x m
fractionalEvaluate (Abs' x) m = abs <$> fractionalEvaluate x m
fractionalEvaluate (Signum' x) m = signum <$> fractionalEvaluate x m
fractionalEvaluate (x :+: y) m = (+) <$> fractionalEvaluate x m <*> fractionalEvaluate y m
fractionalEvaluate (x :-: y) m = (-) <$> fractionalEvaluate x m <*> fractionalEvaluate y m
fractionalEvaluate (x :*: y) m = (*) <$> fractionalEvaluate x m <*> fractionalEvaluate y m
fractionalEvaluate (x :/: y) m
  | Just 0 <- y' = Nothing
  | otherwise = (/) <$> x' <*> y'
  where
    x' = fractionalEvaluate x m
    y' = fractionalEvaluate y m
fractionalEvaluate (x :**: (Number n)) m = (^^ n) <$> fractionalEvaluate x m
fractionalEvaluate _ _ = Nothing
