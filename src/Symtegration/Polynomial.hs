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
    mapCoefficients,

    -- * Algorithms
    divide,
    pseudoDivide,
    extendedEuclidean,
    diophantineEuclidean,
    greatestCommonDivisor,
    subresultant,
    differentiate,
    integrate,
    squarefree,
  )
where

import Data.Monoid (Sum (..))

-- $setup
-- >>> import Data.Ratio ((%))
-- >>> import Symtegration.Symbolic
-- >>> import Symtegration.Symbolic.Simplify
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
  -- Folding is ordered from lower to higher terms.
  --
  -- For example with \(3x^5 - 2x + 7\),
  --
  -- >>> foldTerms (\e c -> show (e, c)) (3 * power 5 - 2 * power 1 + 7 :: IndexedPolynomial)
  -- "(0,7 % 1)(1,(-2) % 1)(5,3 % 1)"
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

-- | Maps the coefficients in a polynomial to form another polynomial.
--
-- For example, it can be used to convert a polynomial with 'Rational' coefficients
-- into a polynomial with 'Expression' coefficients.
--
-- >>> let p = 2 * power 1 + 1 :: IndexedPolynomial
-- >>> let q = mapCoefficients fromRational p :: IndexedSymbolicPolynomial
-- >>> simplify $ coefficient q 1
-- Number 2
mapCoefficients ::
  (Polynomial p e c, Polynomial p e c', Num (p e c), Num (p e c')) =>
  (c -> c') ->
  p e c ->
  p e c'
mapCoefficients f p = getSum $ foldTerms convertTerm p
  where
    convertTerm e c = Sum $ scale (f c) (power e)

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

-- | Polynomial pseudo-division.  It returns the pseudo-quotient and pseudo-remainder polynomials.
--
-- Equivalent to \(b^{\delta+1} p\) divided by \(q\),
-- where \(p\) and \(q\) are polynomials with integer coefficients,
-- \(b\) is the leading coefficient of \(q\) and \(\delta=\max(-1, \deg(p) - \deg(q))\).
-- This guarantees the pseudo-quotient and pseudo-remainder exist,
-- even when the quotient and remainder do not when only integer coefficients are allowed.
--
-- For example, with \(p = 3x^3 + x^2 + x + 5\) and \(q = 5x^2 - 3x + 1\),
-- it is the case that \(5^2p = (15x + 14)q + (52x + 111)\):
--
-- >>> let p = 3 * power 3 + power 2 + power 1 + 5 :: IndexedPolynomial
-- >>> let q = 5 * power 2 - 3 * power 1 + 1 :: IndexedPolynomial
-- >>> pseudoDivide p q
-- (15x + 14,52x + 111)
pseudoDivide ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num c) =>
  -- | Dividend polynomial being pseudo-divided.
  p e c ->
  -- | Divisor polynomial pseudo-dividing the dividend.
  p e c ->
  -- | Pseudo-quotient and pseudo-remainder.
  (p e c, p e c)
pseudoDivide p q
  | degree p < degree q = (0, p)
  | otherwise = go (1 + degree p - degree q) 0 p
  where
    b = leadingCoefficient q
    go n quotient remainder
      | remainder /= 0, delta >= 0 = go (n - 1) quotient' remainder'
      | otherwise = (scale (b ^ n) quotient, scale (b ^ n) remainder)
      where
        delta = degree remainder - degree q
        t = scale (leadingCoefficient remainder) (power delta)
        quotient' = scale b quotient + t
        -- Subtract with the leading terms deleted.
        -- The leading terms cancel out numerically,
        -- but guarantee cancellation when the coefficients are symbolic.
        remainder' = deleteLeadingTerm (scale b remainder) - deleteLeadingTerm (t * q)

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

-- | Returns the resultant and the subresultant polynomial remainder sequence for the given polynomials.
--
-- >>> subresultant (power 2 + 1) (power 2 - 1 :: IndexedPolynomial)
-- (4 % 1,[x^2 + 1,x^2 + (-1),(-2),0])
-- >>> subresultant (2 * power 2 - 3 * power 1 + 1) (5 * power 2 + power 1 - 6 :: IndexedPolynomial)
-- (0 % 1,[2x^2 + (-3)x + 1,5x^2 + x + (-6),17x + (-17),0])
-- >>> subresultant (power 3 + 2 * power 2 + 3 * power 1 + 4) (5 * power 2 + 6 * power 1 + 7 :: IndexedPolynomial)
-- (832 % 1,[x^3 + 2x^2 + 3x + 4,5x^2 + 6x + 7,16x + 72,832,0])
--
-- === __Reference__
--
-- See sections 1.4 and 1.5 in
-- [/Symbolic Integration I: Transcendental Functions/](https://doi.org/10.1007/b138171)
-- by Manuel Bronstein for the definition of resultants, subresultants,
-- polynomial remainder sequences, and subresultant polynomial remainder sequences.
subresultant ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num e, Fractional c) =>
  -- | First element in the remainder sequence.
  p e c ->
  -- | Second element in the remainder sequence.
  p e c ->
  -- | The resultant and the subresultant polynomial remainder sequence.
  (c, [p e c])
subresultant p q
  | degree p >= degree q = (resultantFromSequence rs betas, rs)
  | otherwise = ((-1) ^ (degree q * degree p) * resultant, prs)
  where
    (rs, betas) = subresultantRemainderSequence (p, q) gamma beta
    gamma = -1
    beta = (-1) ^ (1 + delta)
    delta = degree p - degree q

    (resultant, prs) = subresultant q p

-- | Derives the subresultant polynomial remainder sequence for 'subresultant'.
-- Constructs \(\gamma_i\), \(\beta_i\), and the remainder sequence as it goes along.
-- Returns the remainder sequence and the sequence of \(\beta_i\).
subresultantRemainderSequence ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num e, Fractional c) =>
  -- | The previous and current remainders in the sequence.
  (p e c, p e c) ->
  -- | \(\gamma_i\) as defined for the subresultant PRS.
  c ->
  -- | \(\beta_i\) as defined for the subresultant PRS.
  c ->
  -- | Polynomial remainder sequence and sequence of \(\beta_i\).
  ([p e c], [c])
subresultantRemainderSequence (rprev, rcurr) gamma beta
  | rcurr /= 0 = (rprev : rs, beta : betas)
  | otherwise = ([rprev, rcurr], [beta])
  where
    (rs, betas) = subresultantRemainderSequence (rcurr, rnext) gamma' beta'
    (_, r) = pseudoDivide rprev rcurr
    rnext = scale (1 / beta) r
    lc = leadingCoefficient rcurr
    delta = degree rprev - degree rcurr
    delta' = degree rcurr - degree rnext
    gamma' = ((-lc) ^ delta) * (gamma ^^ (1 - delta))
    beta' = (-lc) * (gamma' ^ delta')

-- | Constructs the resultant based on the subresultant polynomial remainder sequence
-- and the sequence of \(\beta_i\) used to construct the subresultant PRS.
resultantFromSequence ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Num e, Fractional c) =>
  -- | Subresultant polynomial remainder sequence.
  [p e c] ->
  -- | Sequence of \(\beta_i\) used for deriving the subresultant PRS.
  [c] ->
  -- | Resultant.
  c
resultantFromSequence rs betas = go rs betas 1 1
  where
    go (r : r' : r'' : rs') (beta : betas') c s
      | [] <- rs', degree r' > 0 = 0
      | [] <- rs', degree r == 1 = leadingCoefficient r'
      | [] <- rs' = s * c * leadingCoefficient r' ^ degree r
      | otherwise = go (r' : r'' : rs') betas' c' s'
      where
        s' | odd (degree r), odd (degree r') = -s | otherwise = s
        c' = c * ((beta / (lc ^ (1 + delta))) ^ degree r') * (lc ^ (degree r - degree r''))
        lc = leadingCoefficient r'
        delta = degree r - degree r'
    go _ _ _ _ = 0

-- | Returns the derivative of the given polynomial.
--
-- >>> differentiate (power 2 + power 1 :: IndexedPolynomial)
-- 2x + 1
differentiate :: (Polynomial p e c, Num (p e c), Num c) => p e c -> p e c
differentiate p = getSum $ foldTerms diffTerm p
  where
    diffTerm 0 _ = Sum 0
    diffTerm e c = Sum $ scale (fromIntegral e * c) $ power (e - 1)

-- | Returns the integral of the given polynomial.
--
-- >>> integrate (power 2 + power 1 :: IndexedPolynomial)
-- (1 % 3)x^3 + (1 % 2)x^2
integrate :: (Polynomial p e c, Num (p e c), Fractional c) => p e c -> p e c
integrate p = getSum $ foldTerms integrateTerm p
  where
    integrateTerm e c = Sum $ scale (c / (1 + fromIntegral e)) $ power (e + 1)

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
