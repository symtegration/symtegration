-- |
-- Description: Numbers used for testing whether two expressions may be semantically equivalent.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- When trying to compare two expressions as to whether they are semantically equivalent,
-- actually computing a value with concrete numbers should result in similar results.
-- This is much easier than trying to semantically prove that two symbolic representations
-- are equivalent, unless one has high confidence that the prover is correct and complete.
-- Many tests for Symtegration uses this approach to test that two mathematical expressions
-- are at least not semantically different.
--
-- There are a number of caveats with this approach.  One is that we use floating-point
-- computations to reliably compare the computation results when evaluating expressions,
-- and this involves floating-point error.  For this reason, we include estimated error
-- bounds in our computations.
--
-- Some computations involve infinities or "not a number", and to prevent certain operations
-- turning them back into finite numbers, such results will always be "not a number".
-- This avoids situations such as positive zeros becoming positive infinities
-- and negative zeroes becoming negative infinities, resulting in huge divergences
-- despite starting out with practically the same values.  Tests should use 'isFinite'
-- to check that an approximation is finite and meaningfully comparable before
-- using it in tests.
--
-- For some operators or functions, slight changes can have massive divergences in results.
-- In other cases, being forced to have a single result when mathematically there are many
-- results can result in significant divergences in results as well.  In these cases,
-- we force the computation to return a "not a number", so that tests can avoid
-- these cases when testing the equivalence between two symbolic representations of
-- mathematical expressions.
--
-- Complex numbers are algebraically closed, so it is not anticipated that further
-- extensions into what number domain is used for computations will be needed.
module Symtegration.Approximate
  ( Approximate,
    approximate,
    approximateWithError,
    center,
    isFinite,
  )
where

import Data.Complex
import Data.Foldable1 qualified as Foldable1
import Data.List.NonEmpty (NonEmpty (..))
import Test.QuickCheck

-- | Number type approximating a number with a central point and an estimated error bound.
-- Only real numbers may be approximated explicitly,
-- but computations will be done on the complex number domain.
data Approximate = Approximate (Complex Double) Double
  deriving (Show)

-- | Approximate a real number with some error.
approximate :: Double -> Approximate
approximate x
  | abs x <= zeroThreshold = Approximate (x :+ 0) implicitError
  | otherwise = Approximate (x :+ 0) (implicitError * abs x)

-- | Implicit error to include when approximating real numbers.
implicitError :: Double
implicitError = 1e-5

-- | Threshold to be considered practically zero.
zeroThreshold :: Double
zeroThreshold = 1e-8

-- | Approximate a real number with an explicitly specified error.
approximateWithError :: Double -> Double -> Approximate
approximateWithError x = Approximate (x :+ 0)

-- | Return the central point of the approximated number.
-- I.e., discards the estimated error bound.
center :: Approximate -> Complex Double
center (Approximate z _) = z

-- | Returns whether the number is a concrete finite number.
-- I.e., not infinite and not "not a number".
isFinite :: Approximate -> Bool
isFinite (Approximate (x :+ y) err) = finite x && finite y && finite err
  where
    finite z = not (isNaN x || isInfinite z)

-- | "Not A Number".
notNumber :: Approximate
notNumber = Approximate (0 / 0) (0 / 0)

-- | Candidate complex numbers to use in complex number operations for
-- estimating error bounds.  The distance to the complex number result farthest
-- from the computed central point will be used as the estimated error bound.
candidates :: Approximate -> [Complex Double]
-- Keep it real if it is a real number.
candidates (Approximate (x :+ 0) err) = [x :+ 0, (x - err) :+ 0, (x + err) :+ 0]
-- Use eight equidistant corners and the value itself.
candidates (Approximate z err) = z : [z + c | c <- corners]
  where
    corners =
      [err :+ 0, (-err) :+ 0, 0 :+ err, 0 :+ (-err)]
        ++ map (* sqrt (1 / 2)) [err :+ err, err :+ (-err), (-err) :+ err, (-err) :+ (-err)]

unaryOp :: (Complex Double -> Complex Double) -> Approximate -> Approximate
unaryOp op v@(Approximate z@(zx :+ zy) _)
  | isNaN zx || isNaN zy = notNumber
  | isInfinite zx || isInfinite zy = notNumber
  | isNaN x || isNaN y = notNumber
  | isInfinite x || isInfinite y = notNumber
  | otherwise = Approximate w err
  where
    w@(x :+ y) = op z
    err = Foldable1.maximum $ minimal :| [magnitude (w' - w) | z' <- candidates v, let w' = op z']

    -- Some minimal error to prevent error bounds from whithering away.
    -- E.g., with exp (-10000).
    minimal
      | magnitude w <= zeroThreshold = implicitError
      | otherwise = magnitude w * implicitError

binaryOp :: (Complex Double -> Complex Double -> Complex Double) -> Approximate -> Approximate -> Approximate
binaryOp op x@(Approximate u@(ux :+ uy) _) y@(Approximate v@(vx :+ vy) _)
  | isNaN ux || isNaN uy || isNaN vx || isNaN vy = notNumber
  | isInfinite ux || isInfinite uy || isInfinite vx || isInfinite vy = notNumber
  | isNaN r || isNaN s = notNumber
  | isInfinite r || isInfinite s = notNumber
  | otherwise = Approximate w err
  where
    w@(r :+ s) = op u v
    errors = [magnitude (w' - w) | x' <- candidates x, y' <- candidates y, let w' = op x' y']
    err = Foldable1.maximum $ minimal :| errors

    -- Some minimal error to prevent error bounds from whithering away.
    -- E.g., with 0.1 ** 10000.
    minimal
      | magnitude w <= zeroThreshold = implicitError
      | otherwise = magnitude w * implicitError

-- | Two 'Approximate' values are considered equal if they are one of:
--
-- * both practically zero by having zero within their estimated bounds
--
-- * the central point of one value is within the other's estimated bounds
--
-- * either is "not a number"
instance Eq Approximate where
  (Approximate x xerr) == (Approximate y yerr)
    | magnitude x <= xerr && magnitude y <= yerr = True -- both practically zero
    | otherwise = not diffx || not diffy
    where
      -- The converses will return true if any value is "not a number".
      diffx = difference > xerr * tolerance
      diffy = difference > yerr * tolerance
      difference = magnitude $ x - y
      tolerance = 10

instance Num Approximate where
  (+) = binaryOp (+)
  (-) = binaryOp (-)
  (*) = binaryOp (*)
  abs = unaryOp abs
  signum z@(Approximate w err)
    | magnitude w <= err = notNumber -- signum is squirrelly at 0
    | otherwise = 0 + unaryOp signum z -- Add zero to re-introduce error.
  fromInteger = approximate . fromInteger
  negate = unaryOp negate

instance Fractional Approximate where
  fromRational = approximate . fromRational
  recip = unaryOp recip
  (/) = binaryOp (/)

instance Floating Approximate where
  pi = approximate pi

  exp = unaryOp exp

  log z@(Approximate (x :+ y) err)
    | x <= 0, abs y <= err = notNumber -- log is squirrelly on (-infinity, 0]
    | otherwise = unaryOp log z

  sqrt z@(Approximate (x :+ y) err)
    | x < 0, abs y <= err = notNumber -- sqrt is squirrelly on (-infinity, 0)
    | otherwise = unaryOp sqrt z

  0 ** 0 = notNumber -- squirrelly
  z@(Approximate (x :+ y) err) ** w@(Approximate (_ :+ 0) _)
    | x < 0, abs y <= err = notNumber -- (**) is squirrelly on (-infinity, 0) for non-integer exponents
    | otherwise = binaryOp (**) z w
  -- Non-real exponents are squirrelly, especially when nested like (x ** z) ** w.
  _ ** _ = notNumber

  logBase b@(Approximate (e :+ f) berr) z@(Approximate (x :+ y) zerr)
    | e <= 0, abs f <= berr = notNumber -- log is squirrelly on (-infinity, 0]
    | x <= 0, abs y <= zerr = notNumber -- log is squirrelly on (-infinity, 0]
    | otherwise = binaryOp logBase b z

  sin = unaryOp sin
  cos = unaryOp cos
  tan = unaryOp tan

  -- Avoid issues with multiple results by restricting inverse trigonometric functions
  -- to using real number arguments with real number results.

  asin z@(Approximate (x :+ 0) _)
    | -1 <= x, x <= 1 = unaryOp asin z
    | otherwise = notNumber
  asin _ = notNumber

  acos z@(Approximate (x :+ 0) _)
    | -1 <= x, x <= 1 = unaryOp acos z
    | otherwise = notNumber
  acos _ = notNumber

  atan z@(Approximate (_ :+ 0) _) = unaryOp atan z
  atan _ = notNumber

  cosh = unaryOp cosh
  sinh = unaryOp sinh
  tanh = unaryOp tanh

  acosh z@(Approximate (x :+ 0) _)
    | x >= 1 = unaryOp acosh z
    | otherwise = notNumber
  acosh _ = notNumber

  asinh z@(Approximate (_ :+ 0) _) = unaryOp asinh z
  asinh _ = notNumber

  atanh z@(Approximate (x :+ 0) _)
    | -1 < x, x < 1 = unaryOp atanh z
    | otherwise = notNumber
  atanh _ = notNumber

instance Arbitrary Approximate where
  arbitrary = approximate <$> arbitrary
  shrink (Approximate z _) = [approximate (realPart z') | z' <- shrink z]
