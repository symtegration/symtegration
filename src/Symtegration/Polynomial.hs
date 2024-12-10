-- |
-- Module: Symtegration.Polynomial
-- Description: Polynomials for Symtegration.
-- Maintainer: dev@chungyc.org
--
-- This modules defines a type class that concrete types representing polynomials
-- should be an instance of.  It includes important algorithms operating on
-- polynomials.  In particular, algorithms for polynomial division and
-- the extended Euclidean algorithm are included.
module Symtegration.Polynomial
  ( -- * Polynomials
    Polynomial (..),

    -- * Algorithms
    divide,
    extendedEuclidean,
  )
where

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
  leadingCoefficient :: p e c -> c

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
      | remainder == 0 = (quotient, remainder)
      | delta < 0 = (quotient, remainder)
      | otherwise = go (quotient + t) (remainder - q * t)
      where
        delta = degree remainder - degree q
        t = scale (leadingCoefficient remainder / leadingCoefficient q) $ power delta

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
