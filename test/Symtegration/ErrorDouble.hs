-- |
-- Description: Floating-point numbers with error ranges.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Floating-point numbers with error bars.
-- Basically each number is a pair of 'Double' values denoting a range.
-- These are used to determine whether an expression is too sensitive
-- to small divergences in floating-point computations.  By avoiding such
-- expressions, one can avoid situations where a mathematically equivalent
-- reformulation of a mathematical expression can end up with vastly different results.
module Symtegration.ErrorDouble (DoubleWithError, includeError, relativeError) where

import Data.Foldable1 qualified as Foldable1
import Data.List.NonEmpty (NonEmpty (..))
import Prelude hiding (maximum, minimum)

-- | Floating-point number with a range of values simulating floating-point divergences.
newtype DoubleWithError = DE (Double, Double) deriving (Eq, Ord)

-- | Relative size of error to introduce to an individual 'Double' value.
errorSize :: Double
errorSize = 1e-8

-- | Add some error to a 'Double' value.
includeError :: Double -> DoubleWithError
includeError x
  | u <= v = DE (u, v)
  | otherwise = DE (v, u)
  where
    u = x * (1 - errorSize)
    v = x * (1 + errorSize)

-- | The relative size of error present in a 'DoubleWithError' value.
relativeError :: DoubleWithError -> Double
relativeError (DE (u, 0)) = abs u
relativeError (DE (0, v)) = abs v
relativeError (DE (u, v)) = abs (u - v) / (abs u + abs v)

binOp :: (Double -> Double -> Double) -> DoubleWithError -> DoubleWithError -> DoubleWithError
binOp f (DE (u, v)) (DE (u', v')) = DE (Foldable1.minimum bounds, Foldable1.maximum bounds)
  where
    bounds = f u u' :| [f u v', f v u', f v' u']

unOp :: (Double -> Double) -> DoubleWithError -> DoubleWithError
unOp f (DE (u, v)) = DE (min u' v', max u' v')
  where
    u' = f u
    v' = f v

instance Num DoubleWithError where
  (+) = binOp (+)
  (-) = binOp (-)
  (*) = binOp (*)
  negate = unOp negate
  abs = unOp abs
  signum = unOp signum
  fromInteger = includeError . fromInteger

instance Fractional DoubleWithError where
  (/) = binOp (/)
  recip = unOp recip
  fromRational = includeError . fromRational

instance Floating DoubleWithError where
  pi = includeError pi
  exp = unOp exp
  log = unOp log
  (**) = binOp (**)
  logBase = binOp logBase
  sin = unOp sin
  cos = unOp cos
  tan = unOp tan
  asin = unOp asin
  acos = unOp acos
  atan = unOp atan
  sinh = unOp sinh
  cosh = unOp cosh
  tanh = unOp tanh
  asinh = unOp asinh
  acosh = unOp acosh
  atanh = unOp atanh
