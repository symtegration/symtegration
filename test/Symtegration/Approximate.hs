-- |
-- Description: Numbers used for testing whether two expressions may be semantically equivalent.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
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

data Approximate = Approximate (Complex Double) Double
  deriving (Show)

approximate :: Double -> Approximate
approximate 0 = Approximate 0 implicitError
approximate x = Approximate (x :+ 0) (implicitError * abs x)

implicitError :: Double
implicitError = 1e-5

approximateWithError :: Double -> Double -> Approximate
approximateWithError x = Approximate (x :+ 0)

center :: Approximate -> Complex Double
center (Approximate z _) = z

isFinite :: Approximate -> Bool
isFinite (Approximate (x :+ y) err) = finite x && finite y && finite err
  where
    finite z = not (isNaN x || isInfinite z)

notNumber :: Approximate
notNumber = Approximate (0 / 0) (0 / 0)

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
    err = Foldable1.maximum $ 0 :| [magnitude (w' - w) | z' <- candidates v, let w' = op z']

binaryOp :: (Complex Double -> Complex Double -> Complex Double) -> Approximate -> Approximate -> Approximate
binaryOp op x@(Approximate u@(ux :+ uy) _) y@(Approximate v@(vx :+ vy) _)
  | isNaN ux || isNaN uy || isNaN vx || isNaN vy = notNumber
  | isInfinite ux || isInfinite uy || isInfinite vx || isInfinite vy = notNumber
  | isNaN r || isNaN s = notNumber
  | isInfinite r || isInfinite s = notNumber
  | otherwise = Approximate w err
  where
    w@(r :+ s) = op u v
    err = Foldable1.maximum $ 0 :| [magnitude (w' - w) | x' <- candidates x, y' <- candidates y, let w' = op x' y']

instance Eq Approximate where
  (Approximate x xerr) == (Approximate y yerr)
    | magnitude x <= xerr && magnitude y <= yerr = True -- both practically zero
    | otherwise = not diffx || not diffy
    where
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
