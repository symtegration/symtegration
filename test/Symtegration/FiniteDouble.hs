-- |
-- Description: A variant of 'Double' without infinities or multiple zeroes.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- 'FiniteDouble' is a variant of 'Double' which avoids sensitivities
-- which result in what would otherwise be equivalent mathematical functions
-- result in significantly different results.  Basically, it ensures that
-- any finite value resulting from a calculation on finite values does
-- not involve any infinities during the calculation.
--
-- A value of NaN compares equal to any other NaN, which makes it possible
-- to check whether two supposedly equivalent functions both return NaN.
--
-- The functions operating on 'FiniteDouble' are not allowed to return
-- infinities, which prevents seemingly equivalent functions from returning
-- completely different results.  For the same reason, only a positive zero
-- is allowed to be returned.
--
-- These are examples of seemingly equivalent functions which can return
-- significantly different finite results, which 'FiniteDouble' prevents:
--
-- *   @(atan (m / (0 * z)))@ and @(atan (m / 0))@
--
-- *   @d / (0 - (cosh (exp ((logBase f c) / (e * 0)))))@ and
--     @d / (0 - (cosh (exp ((logBase f c) / 0))))@
--
-- *   @tanh (s ** (f / (0 * k)))@ and @tanh (s ** (f / 0))@
module Symtegration.FiniteDouble (FiniteDouble (..), isFinite, Exact (..), Near (..)) where

import Test.QuickCheck

-- | A variant of 'Double' which only allows finite
newtype FiniteDouble = FiniteDouble Double

instance Show FiniteDouble where
  show (FiniteDouble x) = show x

instance Eq FiniteDouble where
  (FiniteDouble x) == (FiniteDouble y)
    | isNaN x && isNaN y = True
    | otherwise = x == y

instance Ord FiniteDouble where
  (FiniteDouble x) <= (FiniteDouble y)
    | isNaN x && isNaN y = True
    | otherwise = x <= y

instance Num FiniteDouble where
  (+) = binOp (+)
  (*) = binOp (*)
  (-) = binOp (-)
  abs = unaryOp abs
  signum = unaryOp signum
  fromInteger n = FiniteDouble $ fromInteger n

instance Fractional FiniteDouble where
  (/) = binOp (/)
  fromRational q = FiniteDouble $ fromRational q

instance Floating FiniteDouble where
  pi = FiniteDouble pi
  exp = unaryOp exp
  log = unaryOp log
  sin = unaryOp sin
  cos = unaryOp cos
  asin = unaryOp asin
  acos = unaryOp acos
  atan = unaryOp atan
  sinh = unaryOp sinh
  cosh = unaryOp cosh
  asinh = unaryOp asinh
  acosh = unaryOp acosh
  atanh = unaryOp atanh

instance Real FiniteDouble where
  toRational (FiniteDouble x) = toRational x

instance RealFrac FiniteDouble where
  properFraction (FiniteDouble x) = (n, FiniteDouble f)
    where
      (n, f) = properFraction x

instance RealFloat FiniteDouble where
  floatRadix (FiniteDouble x) = floatRadix x
  floatDigits (FiniteDouble x) = floatDigits x
  floatRange (FiniteDouble x) = floatRange x
  decodeFloat (FiniteDouble x) = decodeFloat x
  encodeFloat x y = FiniteDouble $ encodeFloat x y
  isNaN (FiniteDouble x) = isNaN x || isInfinite x
  isInfinite _ = False
  isDenormalized (FiniteDouble x) = isDenormalized x
  isNegativeZero (FiniteDouble x) = isNegativeZero x
  isIEEE (FiniteDouble x) = isIEEE x

instance Arbitrary FiniteDouble where
  arbitrary = FiniteDouble <$> arbitrary
  shrink (FiniteDouble x) = FiniteDouble <$> shrink x

-- | Returns whether a number is finite and not a NaN.
isFinite :: FiniteDouble -> Bool
isFinite (FiniteDouble x)
  | isNaN x = False
  | isInfinite x = False
  | otherwise = True

binOp :: (Double -> Double -> Double) -> FiniteDouble -> FiniteDouble -> FiniteDouble
binOp op (FiniteDouble x) (FiniteDouble y)
  | isInfinite v = FiniteDouble nan
  | -0 <- v = FiniteDouble 0
  | otherwise = FiniteDouble v
  where
    v = x `op` y
    nan = 0 / 0

unaryOp :: (Double -> Double) -> FiniteDouble -> FiniteDouble
unaryOp op (FiniteDouble x)
  | isInfinite v = FiniteDouble nan
  | -0 <- v = FiniteDouble 0
  | otherwise = FiniteDouble v
  where
    v = op x
    nan = 0 / 0

-- | Wrapper type over 'FiniteDouble' so that the same return values are compared as equal.
-- In other words, a NaN compared to a NaN will be considered equal.
-- Used for comparing that two implementations apply the exact same operations.
newtype Exact = Exact FiniteDouble deriving (Show)

instance Eq Exact where
  (Exact x) == (Exact y)
    | isNaN x && isNaN y = True
    | otherwise = x == y

-- | Wrapper type for comparing whether 'FiniteDouble' values are close enough.
-- Intended for testing whether two supposedly equivalent functions return
-- values which are close enough.
newtype Near = Near FiniteDouble deriving (Show)

instance Eq Near where
  (==) (Near (FiniteDouble x)) (Near (FiniteDouble y))
    | isNaN x && isNaN y = True
    | isInfinite x || isInfinite y = x == y
    | x == 0 || y == 0 || x == (-0) || y == (-0) = x - y < threshold
    | otherwise = (x - y) / y < threshold
    where
      threshold = 0.01
