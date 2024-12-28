-- |
-- Module: Symtegration.Polynomial
-- Description: Polynomials for Symtegration.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This modules defines a type class that concrete types representing polynomials
-- should be an instance of.  It includes important algorithms operating on
-- polynomials.  In particular, algorithms for polynomial division and
-- the extended Euclidean algorithm are included.
module Symtegration.Polynomial
  ( -- * Polynomials
    Polynomial (..),
    monic,

    -- * Algorithms
    divide,
    extendedEuclidean,
    diophantineEuclidean,
    greatestCommonDivisor,
    differentiate,
    squarefree,
  )
where

import Data.Monoid (Sum (..))

-- $setup
-- >>> import Data.Ratio ((%))
-- >>> import Symtegration.Polynomial.Indexed

-- | Polynomials must support the operations specified in this type class.
-- All powers must be non-negative.
class (Integral e, Num c) => Polynomial p e c where
  -- | Returns the degree of a given polynomial.
  --
  -- The following returns 9 for the highest term in \(3x^9 + 2x^4 + x\):
  --
  -- >>> degree (3 * power 9 + 2 * power 4 + power 1 :: IndexedPolynomial)
  -- 9
  degree :: p e c -> e

  -- | Returns the coefficient for the term with the given power.
  --
  -- The following returns 4 from the \(4x^3\) term in \(x^4 + 4x^3 + 3\):
  --
  -- >>> coefficient (power 4 + 4 * power 3 + 3 :: IndexedPolynomial) 3
  -- 4 % 1
  coefficient :: p e c -> e -> c

  -- | Returns the leading coefficient.
  --
  -- The following returns 6 from the \(6x^3\) term in \(6x^3 + 2x^2\):
  --
  -- >>> leadingCoefficient (6 * power 3 + 2 * power 2 :: IndexedPolynomial)
  -- 6 % 1
  --
  -- The leading coefficient is never zero unless the polynomial itself is zero.
  leadingCoefficient :: p e c -> c

  -- | Returns the polynomial without the leading term.
  --
  -- >>> deleteLeadingTerm (2 * power 3 + power 1 + 2 :: IndexedPolynomial)
  -- x + 2
  deleteLeadingTerm :: p e c -> p e c

  -- | Fold the terms, i.e., the powers and coefficients, using the given monoid.
  -- Only terms with non-zero coefficients will be folded.
  -- Folding is ordered from higher to lower terms.
  --
  -- For example with \(3x^5 - 2x + 7\),
  --
  -- >>> foldTerms (\e c -> show (e, c)) (3 * power 5 - 2 * power 1 + 7 :: IndexedPolynomial)
  -- "(5,3 % 1)(1,(-2) % 1)(0,7 % 1)"
  foldTerms :: (Monoid m) => (e -> c -> m) -> p e c -> m

  -- | Multiplies a polynomial by a scalar.
  --
  -- The following divides \(6x + 2\) by 2:
  --
  -- >>> scale (1 % 2) (6 * power 1 + 2 :: IndexedPolynomial)
  -- 3x + 1
  scale :: c -> p e c -> p e c

  -- | Returns a single term with the variable raised to the given power.
  --
  -- The following is equivalent to \(x^5\):
  --
  -- >>> power 5 :: IndexedPolynomial
  -- x^5
  power :: e -> p e c

-- | Scale the polynomial so that its leading coefficient is one.
--
-- >>> monic $ 4 * power 2 + 4 * power 1 + 4 :: IndexedPolynomial
-- x^2 + x + 1
--
-- The exception is when the polynomial is zero.
--
-- >>> monic 0 :: IndexedPolynomial
-- 0
monic :: (Polynomial p e c, Eq c, Fractional c) => p e c -> p e c
monic p
  | leadingCoefficient p == 0 = p
  | otherwise = scale (1 / leadingCoefficient p) p

-- | Polynomial division.  It returns the quotient polynomial and the remainder polynomial.
--
-- For example, dividing \(p = x^3-12x^2-42\) by \(q = x^2 - 2x + 1\)
-- returns \(x-10\) as the quotient and \(-21x-32\) as the remainder,
-- since \(p = (x-10)q -21x - 32\):
--
-- >>> let p = power 3 - 12 * power 2 - 42 :: IndexedPolynomial
-- >>> let q = power 2 - 2 * power 1 + 1 :: IndexedPolynomial
-- >>> divide p q
-- (x + (-10),(-21)x + (-32))
divide ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  -- | Dividend polynomial being divided.
  p e c ->
  -- | Divisor polynomial dividing the dividend.
  p e c ->
  -- | Quotient and remainder.
  (p e c, p e c)
divide p q = go 0 p
  where
    go quotient remainder
      | remainder /= 0, delta >= 0 = go (quotient + t) (remainder' - qt')
      | otherwise = (quotient, remainder)
      where
        delta = degree remainder - degree q
        t = scale (leadingCoefficient remainder / leadingCoefficient q) $ power delta
        -- remainder and q * t will have the same leading coefficients.
        -- Subtract them without the leading terms.
        -- Not necessary for purely numeric coefficients,
        -- but guarantees the cancellation of the leading terms when coefficients are symbolic.
        remainder' = deleteLeadingTerm remainder
        qt' = deleteLeadingTerm $ q * t

-- | The extended Euclidean algorithm.  For polynomials \(p\) and \(q\),
-- it returns the greatest common divisor between \(p\) and \(q\).
-- It also returns \(s\) and \(t\) such that \(sp+tq = \gcd(p,q)\).
--
-- For example, for \(p=2x^5-2x\) and \(q=x^4-2x^2+1\), it is the case
-- that \(\gcd(p,q)=-x^2+1\) and \((-\frac{1}{4}x) p + (\frac{1}{2}x^2 + 1) q = -x^2+1\):
--
-- >>> let p = 2 * power 5 - 2 * power 1 :: IndexedPolynomial
-- >>> let q = power 4 - 2 * power 2 + 1 :: IndexedPolynomial
-- >>> extendedEuclidean p q
-- (((-1) % 4)x,(1 % 2)x^2 + 1,(-1)x^2 + 1)
extendedEuclidean ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  -- | Polynomial \(p\).
  p e c ->
  -- | Polynomial \(q\).
  p e c ->
  -- | \(s\), \(t\), and \(\gcd(p,q)\).
  (p e c, p e c, p e c)
extendedEuclidean u v = descend u v 1 0 0 1
  where
    descend g 0 s t _ _ = (s, t, g)
    descend a b a1 a2 b1 b2 = descend b r b1 b2 r1 r2
      where
        (q, r) = divide a b
        r1 = a1 - q * b1
        r2 = a2 - q * b2

-- | Solves \(sa + tb = c\) for given polynomials \(a\), \(b\), and \(c\).
-- It will be the case that either \(s=0\) or
-- the degree of \(s\) will be less than the degree of \(b\).
--
-- >>> let a = power 4 - 2 * power 3 - 6 * power 2 + 12 * power 1 + 15 :: IndexedPolynomial
-- >>> let b = power 3 + power 2 - 4 * power 1 - 4 :: IndexedPolynomial
-- >>> let c = power 2 - 1 :: IndexedPolynomial
-- >>> diophantineEuclidean a b c
-- Just (((-1) % 5)x^2 + (4 % 5)x + ((-3) % 5),(1 % 5)x^3 + ((-7) % 5)x^2 + (16 % 5)x + (-2))
--
-- If there is no such \((s,t)\), then 'Nothing' is returned.
diophantineEuclidean ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  -- | Polynomial \(a\).
  p e c ->
  -- | Polynomial \(b\).
  p e c ->
  -- | Polynomial \(c\).
  p e c ->
  -- | \((s,t)\) such that \(sa + tb = c\).
  Maybe (p e c, p e c)
diophantineEuclidean a b c
  | r /= 0 = Nothing
  | s' /= 0, degree s' >= degree b = Just (r', t' + q' * a)
  | otherwise = Just (s', t')
  where
    (s, t, g) = extendedEuclidean a b
    (q, r) = divide c g
    s' = q * s
    t' = q * t
    (q', r') = divide s' b

-- | Returns the greatest common divisor btween two polynomials.
--
-- Convenient wrapper over 'extendedEuclidean' which only returns the greatest common divisor.
greatestCommonDivisor ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  -- | Polynomial \(p\).
  p e c ->
  -- | Polynomial \(q\).
  p e c ->
  -- | \(\gcd(p,q)\).
  p e c
greatestCommonDivisor p q = g
  where
    (_, _, g) = extendedEuclidean p q

-- | Returns the derivative of the given polynomial.
--
-- >>> differentiate (power 2 + power 1 :: IndexedPolynomial)
-- 2x + 1
differentiate :: (Polynomial p e c, Num (p e c), Num c) => p e c -> p e c
differentiate p = getSum $ foldTerms diffTerm p
  where
    diffTerm 0 _ = Sum 0
    diffTerm e c = Sum $ scale (fromIntegral e * c) $ power (e - 1)

-- | Returns the squarefree factorization of the given polynomial.
--
-- Specifically, for a polynomial \(p\), find \([p_1, p_2, \ldots, p_n]\) such that
--
-- \[ p = \sum_{k=1}^n p_k^k \]
--
-- where all \(p_k\) are squarefree, i.e., there is no polynomial \(q\) such that \(q^2 = p_k\).
--
-- For example, the squarefree factorization of \(x^8 + 6x^6 + 12x^4 + 8x^2\)
-- is \(x^2 (x^2 + 2)^3\):
--
-- >>> squarefree (power 8 + 6 * power 6 + 12 * power 4 + 8 * power 2 :: IndexedPolynomial)
-- [1,x,x^2 + 2]
squarefree :: (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) => p e c -> [p e c]
squarefree 0 = [0]
squarefree p
  | (x : xs) <- factor u v = scale c x : xs
  | otherwise = [scale c 1]
  where
    c = leadingCoefficient p
    q = scale (1 / c) p
    q' = differentiate q
    g = monic $ greatestCommonDivisor q q'
    (u, _) = q `divide` g
    (v, _) = q' `divide` g
    factor s y
      | z == 0 = [s]
      | otherwise = f : factor s' y'
      where
        z = y - differentiate s
        f = monic $ greatestCommonDivisor s z
        (s', _) = s `divide` f
        (y', _) = z `divide` f
