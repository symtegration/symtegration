{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module: Symtegration.Polynomial.Rational
-- Description: Representation for rational functions.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Provides a representation for rational functions,
-- which are functions of ratios for two polynomials.
-- Note that these are /not/ functions of rational numbers.
module Symtegration.Polynomial.Rational
  ( Function,
    fromPolynomial,
    fromPolynomials,
    toPolynomial,
    pattern Function,
  )
where

import Control.DeepSeq (NFData)
import Data.Text (unpack)
import GHC.Generics (Generic)
import Symtegration.Polynomial
import TextShow

-- $setup
-- >>> import Symtegration.Polynomial
-- >>> import Symtegration.Polynomial.Indexed

-- | Represents a rational function.
--
-- Values can be constructed with 'fromPolynomial' and 'fromPolynomials',
-- while the numerator and denominator can be obtained by pattern matching with 'Function'.
-- Internally, common factors will be canceled out and the denominator will be monic.
--
-- >>> fromPolynomial $ power 1 :: Function IndexedPolynomial
-- Function (x) (1)
-- >>> fromPolynomials (power 2 + 1) (power 3 + 1) :: Function IndexedPolynomial
-- Function (x^2 + 1) (x^3 + 1)
-- >>> let (Function p q) = fromPolynomials (power 2) (power 1 + 1) :: Function IndexedPolynomial
-- >>> (p, q)
-- (x^2,x + 1)
-- >>> fromPolynomials (4 * power 2 - 4) (2 * power 1 - 2 :: IndexedPolynomial)
-- Function (2x + 2) (1)
--
-- The type is an instance of the 'Fractional' type class, so values can be added,
-- subtracted, multiplied, and divided.  Their values can also be given as
-- integer literals as well.
--
-- >>> let p = fromPolynomials (power 2 + 1) (power 3 + 1) :: Function IndexedPolynomial
-- >>> let q = fromPolynomials (power 1) (power 2 - 1) :: Function IndexedPolynomial
-- >>> p + q
-- Function (2x^3 + (-2)x^2 + 2x + (-1)) (x^4 + (-1)x^3 + x + (-1))
-- >>> p * q
-- Function (x^3 + x) (x^5 + (-1)x^3 + x^2 + (-1))
-- >>> 0 :: Function IndexedPolynomial
-- Function (0) (1)
-- >>> 5 :: Function IndexedPolynomial
-- Function (5) (1)
--
-- One could import this module with an alias so that @Rational.Function@ can be
-- used as the name of the type for rational functions.
--
-- >>> import Symtegration.Polynomial.Rational as Rational
-- >>> fromPolynomials (power 1) (power 2 + 1) :: Rational.Function IndexedPolynomial
-- Function (x) (x^2 + 1)
data Function a = F a a deriving (Generic, NFData)

{-# COMPLETE Function #-}

-- | Pattern synonym for matching the numerator and denominator of a rational function.
-- This would be equivalent to a data constructor,
-- except it cannot be used for constructing a rational function.
--
-- >>> let (Function p q) = fromPolynomials (power 2) (power 1 + 1) :: Function IndexedPolynomial
-- >>> p
-- x^2
-- >>> q
-- x + 1
pattern Function :: a -> a -> Function a
pattern Function x y <- F x y

instance (Show a, TextShow a) => Show (Function a) where
  show = unpack . showt

instance (TextShow a) => TextShow (Function a) where
  showb (F x y) = "Function (" <> showb x <> ") (" <> showb y <> ")"

instance (Eq a, Num a) => Eq (Function a) where
  (F x 0) == (F y 0) = x == y
  (F x y) == (F u v) = x * v == y * u

instance (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) => Num (Function (p e c)) where
  (F x y) + (F u v) = fromPolynomials (x * v + u * y) (y * v)
  (F x y) - (F u v) = fromPolynomials (x * v - u * y) (y * v)
  (F x y) * (F u v) = fromPolynomials (x * u) (y * v)
  abs = id
  signum = const 1
  fromInteger n = fromPolynomial $ fromInteger n

instance (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) => Fractional (Function (p e c)) where
  (F x y) / (F u v) = fromPolynomials (x * v) (y * u)
  fromRational x = fromPolynomial $ scale (fromRational x) 1

-- | Returns an equivalent rational function from a given polynomial.
--
-- >>> fromPolynomial (power 2 + 1 :: IndexedPolynomial)
-- Function (x^2 + 1) (1)
fromPolynomial :: (Polynomial p e c, Num (p e c)) => p e c -> Function (p e c)
fromPolynomial x = F x 1

-- | Returns a rational function with the ratio of two polynomials.
--
-- >>> fromPolynomials (power 1) (power 2 + 1 :: IndexedPolynomial)
-- Function (x) (x^2 + 1)
fromPolynomials ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  -- | Numerator \(a\).
  p e c ->
  -- | Denominator \(d\).
  p e c ->
  -- | Rational function \(\frac{a}{d}\).
  Function (p e c)
fromPolynomials x y = F x'' y''
  where
    -- Cancel out common factors.
    g = greatestCommonDivisor x y
    (x', _) = x `divide` g
    (y', _) = y `divide` g

    -- Make the denominator monic.
    (x'', y'')
      | 0 <- y' = (x', y')
      | 1 <- y' = (x', y')
      | otherwise = (scale (1 / leadingCoefficient y') x', scale (1 / leadingCoefficient y') y')

-- | Returns an equivalent polynomial from a rational function if possible.
--
-- It returns the numerator if the denominator is a constant.
--
-- >>> toPolynomial $ fromPolynomials (2 * power 1 + 2) (2 :: IndexedPolynomial)
-- Just x + 1
--
-- It returns nothing if the rational function is not equivalent to a polynomial.
--
-- >>> toPolynomial $ fromPolynomials 1 (power 1 :: IndexedPolynomial)
-- Nothing
toPolynomial :: (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) => Function (p e c) -> Maybe (p e c)
toPolynomial (F x 1) = Just x
toPolynomial (F x y)
  | 0 <- degree y, y /= 0 = Just $ scale (1 / leadingCoefficient y) x
  | otherwise = Nothing
