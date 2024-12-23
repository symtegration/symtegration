-- |
-- Module: Symtegration.Symbolic.Simplify.NumericFolding
-- Description: Constant folding of numeric constants.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This merges numeric terms as much as it can to simplify expressions.
-- Simplifications are finitely equivalent; i.e., any calculation with
-- finite inputs should result in the equivalent finite input.
-- The changes will also be exact, and no numeric constant will be replaced
-- by an approximate floating-point number.
module Symtegration.Symbolic.Simplify.NumericFolding (simplify) where

import Symtegration.Symbolic

-- $setup
-- >>> import Symtegration.Symbolic.Haskell

-- | Simplifies computations involving numeric constants.
-- Basically, it computes as much as it can as long as any change is exact.
--
-- >>> toHaskell $ simplify $ 1 + 4
-- "5"
-- >>> toHaskell $ simplify $ 8 ** (1/3)
-- "2"
-- >>> toHaskell $ simplify $ 7 ** (1/3)
-- "7 ** (1 / 3)"
-- >>> toHaskell $ simplify $ 5 * 10 * "x"
-- "50 * x"
--
-- It will replace subtraction by addition and square roots by powers of \(\frac{1}{2}\).
simplify :: Expression -> Expression
simplify e@(Number _) = e
simplify e@(Symbol _) = e
simplify (UnaryApply func x) =
  unary $ UnaryApply func $ simplify x
simplify (BinaryApply func x y) =
  binary $ BinaryApply func (simplify x) (simplify y)

-- | Simplify computations involving numeric constants in unary expressions.
-- The arguments should already have been simplified.
unary :: Expression -> Expression
unary (Negate' (Number n)) = Number (-n)
unary (Abs' (Number n)) = Number $ abs n
unary (Signum' (Number n)) = Number $ signum n
unary (Exp' x) = simplifyExp x
unary (Log' x) = simplifyLog x
unary (Sqrt' x) = simplify $ x :**: (Number 1 :/: Number 2)
unary (Sin' x) = simplifySin x
unary (Cos' x) = simplifyCos x
unary (Tan' x) = simplifyTan x
unary e = e

-- | Simplify computations involving numeric constants in binary expressions.
-- The arguments should already have been simplified.
binary :: Expression -> Expression
-- Fold addition.
binary (Number 0 :+: x) = x
binary (x :+: Number 0) = x
binary (Number n :+: Number m) = Number (n + m)
binary ((Number n :/: Number m) :+: Number k) = reduceRatio (n + m * k) m
binary (Number n :+: (Number m :/: Number k)) = reduceRatio (n * k + m) k
binary ((Number n :/: Number m) :+: (Number k :/: Number l)) = reduceRatio (n * l + k * m) (m * l)
binary ((x :+: Number n) :+: Number m) = Number (n + m) :+: x
binary ((Number n :+: x) :+: Number m) = Number (n + m) :+: x
binary (Number n :+: (x :+: Number m)) = Number (n + m) :+: x
binary (Number n :+: (Number m :+: x)) = Number (n + m) :+: x
-- Fold multiplication.
binary (Number 0 :*: _) = Number 0
binary (_ :*: Number 0) = Number 0
binary (Number 1 :*: x) = x
binary (x :*: Number 1) = x
binary (Number n :*: Number m) = Number (n * m)
binary (Number n :*: (Number m :/: Number k)) = reduceRatio (n * m) k
binary ((Number n :/: Number m) :*: Number k) = reduceRatio (n * k) m
binary ((Number n :/: Number m) :*: (Number k :/: Number l)) = reduceRatio (n * k) (m * l)
binary ((x :*: Number n) :*: Number m) = Number (n * m) :*: x
binary ((Number n :*: x) :*: Number m) = Number (n * m) :*: x
binary (Number n :*: (x :*: Number m)) = Number (n * m) :*: x
binary (Number n :*: (Number m :*: x)) = Number (n * m) :*: x
binary e@(Number n :*: (x :/: Number m)) | m /= 0, m == n = x | otherwise = e
binary e@((x :/: Number n) :*: Number m) | n /= 0, m == n = x | otherwise = e
-- Subtractions are turned into addition.
binary (x :-: y) = simplify $ x :+: Negate' y
-- Fold division.
binary (x :/: (y :/: z)) = simplify $ (x :*: z) :/: y
binary ((x :/: y) :/: z) = simplify $ x :/: (y :*: z)
binary (Number n :/: Number m) = reduceRatio n m
-- Fold exponentiation.
binary e@(Number 0 :**: Number 0) = e
binary (Number _ :**: Number 0) = Number 1
binary (Number 1 :**: _) = Number 1
binary (Number n :**: Number m)
  | m >= 0 = Number (n ^ m)
  | otherwise = Number 1 :/: Number (n ^ (-m))
binary ((Number n :/: Number m) :**: Number k)
  | k >= 0 = Number (n ^ k) :/: Number (m ^ k)
  | otherwise = Number (m ^ (-k)) :/: Number (n ^ (-k))
binary e@(Number n :**: (Number m :/: Number k))
  | (Just l) <- root n k, m >= 0 = Number (l ^ m)
  | (Just l) <- root n k, m < 0 = 1 :/: Number (l ^ (-m))
  | otherwise = e
binary e@((Number n :/: Number m) :**: (Number k :/: Number l))
  | (Just n', Just m') <- (root n l, root m l) = (Number n' :/: Number m') :**: Number k
  | otherwise = e
-- Turn LogBase into Log.
binary (LogBase' b x) = simplify $ Log' x :/: Log' b
binary e = e

-- | Simplify integer ratios.  Basically turns them into integers if possible,
-- and if not, reduce the fractions so that the denominator and numerator
-- do not have a common factor.
reduceRatio :: Integer -> Integer -> Expression
reduceRatio n 0 = Number n :/: Number 0
reduceRatio n 1 = Number n
reduceRatio n m
  | m == d = Number (n `div` m)
  | otherwise = Number (n `div` d) :/: Number (m `div` d)
  where
    d = gcd n m

-- | Compute the integer root to the given power.
-- I.e., find \(m\) such that \(m^k = n\)
root ::
  -- | Number \(n\) whose root we want.
  Integer ->
  -- | The power \(k\) of the root.
  Integer ->
  -- | The root \(m\).
  Maybe Integer
root 0 _ = Just 0
root 1 _ = Just 1
root n k
  | k < 0 = Nothing
  | GT <- compare n 0 = search n 1 n
  | LT <- compare n 0, odd k = (* (-1)) <$> search (-n) 1 (-n)
  | otherwise = Nothing
  where
    search m low hi
      | low >= hi, c /= EQ = Nothing
      | EQ <- c = Just mid
      | GT <- c = search m low (mid - 1)
      | LT <- c = search m (mid + 1) hi
      where
        mid = (low + hi) `div` 2
        c = compare (mid ^ k) m

-- | Simplify an exponential of Euler's number.  I.e., simplify \(e^X\).
-- Only the exponent is given as an argument, while the return value is
-- the full simplified expression.
simplifyExp :: Expression -> Expression
simplifyExp (Number 0) = Number 1
simplifyExp (Log' x) = x
simplifyExp e = Exp' e

-- | Simplify a logarithm.  I.e., simplify \(log X\).
-- Only the parameter \(X\) is given as an argument, while the return value is
-- the full simplified expression.
simplifyLog :: Expression -> Expression
simplifyLog (Number 1) = Number 0
simplifyLog (Exp' x) = x
simplifyLog e = Log' e

-- | Simplify a sine.  I.e., simplify \(\sin X\).
-- Only the parameter \(X\) is given as an argument, while the return value is
-- the full simplified expression.
simplifySin :: Expression -> Expression
simplifySin (Number 0) = 0
simplifySin (Number _ :*: Pi') = 0
simplifySin (Pi' :*: Number _) = 0
simplifySin ((Number n :/: 2) :*: Pi')
  | even n = 0
  | odd ((n - 1) `div` 2) = 1
  | otherwise = -1
simplifySin (Pi' :*: (Number n :/: 2))
  | even n = 0
  | odd ((n - 1) `div` 2) = 1
  | otherwise = -1
simplifySin e = Sin' e

-- | Simplify a cosine.  I.e., simplify \(\cos X\).
-- Only the parameter \(X\) is given as an argument, while the return value is
-- the full simplified expression.
simplifyCos :: Expression -> Expression
simplifyCos (Number 0) = 1
simplifyCos (Number n :*: Pi') | even n = 1 | odd n = -1
simplifyCos (Pi' :*: Number n) | even n = 1 | odd n = -1
-- Any 2k/2 would have been simplified to k already.
simplifyCos ((Number _ :/: 2) :*: Pi') = 0
simplifyCos (Pi' :*: (Number _ :/: 2)) = 0
simplifyCos e = Cos' e

-- | Simplify a tangent.  I.e., simplify \(\tan X\).
-- Only the parameter \(X\) is given as an argument, while the return value is
-- the full simplified expression.
simplifyTan :: Expression -> Expression
simplifyTan (Number 0) = 0
simplifyTan e = Tan' e
