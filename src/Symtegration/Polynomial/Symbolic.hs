-- |
-- Module: Symtegration.Polynomial.Symbolic
-- Description: Conversion between data structures storing general mathematical expressions and those specialized for storing polynomials.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.Symbolic
  ( -- * Converting expression to polynomial
    fromExpression,
    forVariable,
    withSymbolicCoefficients,

    -- * Converting polynomial to expression
    toExpression,
    toRationalCoefficient,
    toSymbolicCoefficient,
  )
where

import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Symtegration.Polynomial
import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration
-- >>> import Symtegration.Polynomial
-- >>> import Symtegration.Polynomial.Indexed

-- | Converts an 'Expression' into a 'Polynomial'.
-- 'Nothing' will be returned if the conversion is not possible.
--
-- Specify the symbol representing the variable for the polynomial with 'forVariable'.
-- For example,
--
-- >>> fromExpression (forVariable "x") (("x" + 4) ** 3) :: Maybe IndexedPolynomial
-- Just x^3 + 12x^2 + 48x + 64
--
-- By default, symbols other than the variable for the polynomial are not allowed.
-- To use symbols representing constants, use 'withSymbolicCoefficients' as well.
-- Note that the polynomial type the expression is being converted into
-- must be able to handle symbolic mathematical expressions for the coefficients.
-- For example,
--
-- >>> let expr = ("a" + "b") * "x" + "c" :: Expression
-- >>> let (Just p) = fromExpression (withSymbolicCoefficients (forVariable "x")) expr :: Maybe IndexedSymbolicPolynomial
-- >>> toHaskell $ simplify $ coefficient p 1
-- "a + b"
--
-- The expressions which can be converted must only use 'negate', '(+)', '(*)', '(-)',
-- '(/)' with only numbers, coefficients which do not contain the variable,
-- '(**)' with a non-negative integral exponent, and expressions formed thereof.
fromExpression ::
  (Polynomial p e c, Num (p e c), Fractional c) =>
  (Text -> Maybe (p e c), Expression -> Maybe c) ->
  Expression ->
  Maybe (p e c)
fromExpression _ (Number n) = Just $ fromInteger n
fromExpression (cf, _) (Symbol x) = cf x
fromExpression t (Negate' x) = negate <$> fromExpression t x
fromExpression t (x :+: y) = (+) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :*: y) = (*) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :-: y) = (-) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :**: (Number n))
  | n >= 0 = (^ n) <$> fromExpression t x
  | otherwise = Nothing
fromExpression _ (_ :**: _) = Nothing
fromExpression _ (_ :/: Number 0) = Nothing
fromExpression _ (Number n :/: Number m) = Just $ scale r 1
  where
    r = fromInteger n / fromInteger m
fromExpression (_, eval) e
  | Just e' <- eval e = Just $ scale e' 1
  | otherwise = Nothing

-- | Specifies the symbol representing the variable for 'fromExpression'.
forVariable ::
  (Polynomial p e c, Num (p e c), Fractional c) =>
  Text ->
  (Text -> Maybe (p e c), Expression -> Maybe c)
forVariable v = (fromSymbol, toCoefficient)
  where
    fromSymbol s
      | v == s = Just $ power 1
      | otherwise = Nothing

    toCoefficient (Symbol _) = Nothing
    toCoefficient (Number n) = Just $ fromInteger n
    toCoefficient (Negate' x) = negate <$> toCoefficient x
    toCoefficient (Abs' x) = abs <$> toCoefficient x
    toCoefficient (Signum' x) = signum <$> toCoefficient x
    toCoefficient (x :+: y) = (+) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (x :*: y) = (*) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (x :-: y) = (-) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (x :/: y) = (/) <$> toCoefficient x <*> toCoefficient y
    toCoefficient (x :**: (Number n)) = (^^ n) <$> toCoefficient x
    toCoefficient _ = Nothing

-- | Specifies that non-variable symbols are allowed for 'fromExpression'.
-- The coefficients will be represented by 'Expression' values.
withSymbolicCoefficients ::
  (Polynomial p e Expression, Num (p e Expression), Integral e) =>
  (Text -> Maybe (p e Expression), Expression -> Maybe Expression) ->
  (Text -> Maybe (p e Expression), Expression -> Maybe Expression)
withSymbolicCoefficients (fromSymbol, _) = (fromSymbol', toCoefficient)
  where
    fromSymbol' s = Just $ fromMaybe (scale (Symbol s) 1) (fromSymbol s)

    toCoefficient e@(Symbol s)
      | Nothing <- fromSymbol s = Just e
      | otherwise = Nothing
    toCoefficient e@(Number _) = Just e
    toCoefficient (UnaryApply func x) = UnaryApply func <$> toCoefficient x
    toCoefficient (BinaryApply func x y) = BinaryApply func <$> x' <*> y'
      where
        x' = toCoefficient x
        y' = toCoefficient y

-- | Converts a 'Polynomial' into an 'Expression'.
-- The symbol which will represent the variable is the first argument.
--
-- How the coefficients are converted must also be specified.
-- To evaluate the coefficients to an exact rational number,
-- use 'toRationalCoefficient'.  For example,
--
-- >>> let (Just p) = fromExpression (forVariable "x") (3 * "x"**4 + 1) :: Maybe IndexedPolynomial
-- >>> toHaskell $ simplify $ toExpression "x" toRationalCoefficient p
-- "1 + 3 * (x ** 4)"
--
-- To evaluate the coefficients symbolically, use 'toSymbolicCoefficient'.
--
-- >>> let (Just p) = fromExpression (withSymbolicCoefficients (forVariable "x")) (("a"+"b") * "x"**4 + 1) :: Maybe IndexedSymbolicPolynomial
-- >>> toHaskell $ simplify $ toExpression "x" toSymbolicCoefficient p
-- "1 + (x ** 4) * (a + b)"
toExpression :: (Polynomial p e c) => Text -> (c -> Expression) -> p e c -> Expression
toExpression x cf p = getSum $ foldTerms convert p
  where
    convert 0 c = Sum $ cf c
    convert e c = Sum $ cf c * xp
      where
        xp = Symbol x ** Number (fromIntegral e)

-- | Specifies that coefficients are numbers for 'toExpression'.
toRationalCoefficient :: (Real c) => c -> Expression
toRationalCoefficient c
  | d == 1 = Number n
  | otherwise = Number n :/: Number d
  where
    r = toRational c
    n = fromInteger $ numerator r
    d = fromInteger $ denominator r

-- | Specifies that coefficients are symbolic for 'toExpression'.
toSymbolicCoefficient :: Expression -> Expression
toSymbolicCoefficient = id
