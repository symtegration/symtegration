-- |
-- Module: Symtegration.Polynomial.Differential
-- Description: Polynomials with differential fields.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- In differential algebra for a field \(R\), a /derivation/ is a
-- function \(D : R \rightarrow R\) such that the following holds.
--
-- \[
--   \begin{align*}
--   D(a+b) & = D(a) + D(b) \\
--   D(ab) & = aD(b) + bD(a)
--   \end{align*}
-- \]
--
-- Together, \((R, D)\) is a differential field.
-- For a given derivation \(D\), a squarefree polynomial \(p_n\) is called a
-- /normal/ polynomial with respect to \(D\) if \(\gcd(p_n, D(p_n)) = 1\),
-- while a squarefree polynomial \(p_s\) is called a /special/ polynomial
-- with respect to \(D\) if \(\gcd(p_s, D(p_s)) = p_s\).
--
-- The properties of a derivation are reminiscent of those of a differential operator.
-- In fact, \(\frac{d}{dx}\) is a derivation for functions of \(x\).
-- Another example of a derivation is the derivation which maps everything to zero.
-- Derivations provide a pathway to integrating functions using algebraic algorithms.
--
-- This module defines functions on derivations and polynomials which are useful for integration.
-- The functions assume that a given derivation is indeed a derivation;
-- they do not check whether they are indeed so.
module Symtegration.Polynomial.Differential
  ( -- * Algorithms
    canonical,
    splitFactor,
    splitSquarefreeFactor,

    -- * Utility functions
    extend,
    consistent,
  )
where

import Data.Monoid (Sum (..))
import Symtegration.Polynomial
import Symtegration.Polynomial.Rational

-- $setup
-- >>> import Symtegration.Polynomial
-- >>> import Symtegration.Polynomial.Indexed
-- >>> import Symtegration.Polynomial.Rational

-- | Returns the squarefree factors in the splitting factorization of a given polynomial
-- with respect to a given derivation.  Specifically, with derivation \(D\) and polynomial \(p\),
-- it returns a list of squarefree polynomials \((n_i, s_i)\) such that \(n_i\) are normal
-- and \(s_i\) are special, and also that
--
-- \[ p = \left( \prod_{i=1}^n n_i^i \right) \left( \prod_{i=1}^n s_i^i \right) \]
--
-- For example, with derivation \(D=\frac{d}{dx}\), the squarefree factors in the splitting
-- factorization of \(p=x^3 - 3x^2 + 4\)
-- are \(\left( (x+1)^1 (x-2)^2 \right) \cdot \left( 1^1 \times 1^2 \right)\).
--
-- >>> let p = power 3 - 3 * power 2 + 4 :: IndexedPolynomial
-- >>> splitSquarefreeFactor differentiate p
-- [(x + 1,1),(x + (-2),1)]
--
-- In comparison, with derivation \(Dx = x+1\), the squarefree factors in the splitting
-- factorization are \(\left( 1^1 \times (-\frac{1}{3}x+\frac{2}{3})^2\right) \cdot \left( (x+1)^1 \times (-3)^2 \right)\).
--
-- >>> splitSquarefreeFactor (extend (const 0) (power 1 + 1)) p
-- [(1,x + 1),(((-1) % 3)x + (2 % 3),(-3))]
splitSquarefreeFactor ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  -- | Derivation \(D\).
  (p e c -> p e c) ->
  -- | Polynomial \(p\).
  p e c ->
  -- | List of normal and special squarefree factors \((N_i, S_i)\).
  [(p e c, p e c)]
splitSquarefreeFactor derivation p = map split ps
  where
    ps = squarefree p
    split q = (qn, qs)
      where
        qs = greatestCommonDivisor q $ derivation q
        (qn, _) = q `divide` qs

-- | Returns the splitting factorization of a given polynomial with respect to a given derivation.
-- Specifically, with derivation \(D\) and polynomial \(p\), it returns the polynomials
-- \((q_n, q_s)\) such that \(p = q_n q_s\), the squarefree factors of \(q_n\) are normal,
-- and the squarefree factors of \(q_s\) are special.
--
-- For example, with derivation \(D = \frac{d}{dx}\) and \(p = x^3 - 3x^2 + 4\),
-- the splitting factorization is \(p = (x^3 - 3x^2 + 4) \times 1\).
--
-- >>> let p = power 3 - 3 * power 2 + 4 :: IndexedPolynomial
-- >>> splitFactor differentiate p
-- (x^3 + (-3)x^2 + 4,1)
--
-- In comparison, with derivation \(Dx = x + 1\), the splitting factorization
-- is \(p = \left( \frac{x^2 - 4x + 4}{9} \right) \times (9x + 9)\).
--
-- >>> splitFactor (extend (const 0) (power 1 + 1)) p
-- ((1 % 9)x^2 + ((-4) % 9)x + (4 % 9),9x + 9)
splitFactor ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  -- | The derivation \(D\).
  (p e c -> p e c) ->
  -- | The polynomial \(p\).
  p e c ->
  -- | The splitting factorization \((q_n, q_s)\).
  (p e c, p e c)
splitFactor derivation p = compose (1, 1) factors (1 :: Int)
  where
    factors = splitSquarefreeFactor derivation p
    compose split [] _ = split
    compose (pn, ps) ((qn, qs) : fs) n = compose (pn * (qn ^ n), ps * (qs ^ n)) fs (n + 1)

-- | Returns the canonical representation for the ratio of two polynomials given a derivation.
--
-- Specifically, given two polynomials \(a\) and \(d\) and a derivation \(D\), it returns
-- a tuple of polynomials \((p, (b, d_n), (c, d_s))\) such that
--
-- \[ \frac{a}{d} = p + \frac{b}{d_n} + \frac{c}{d_s} \]
--
-- where each squarefree factor \(n\) of \(d_n\) is normal
-- and each squarefree factor \(s\) of \(d_s\) is special.
-- The degree of \(b\) will be less than that of \(d_n\),
-- and the degree of \(c\) will be less than that of \(d_s\).
-- In the canonical representation of \(\frac{a}{d}\),
-- \(p\) is the polynomial part, \(\frac{b}{d_n}\) is the normal part,
-- and \(\frac{c}{d_s}\) is the special part.
--
-- For example, given the derivation \(D = \frac{d}{dx}\),
-- the canonical representation of \(\frac{x^5+x^2+1}{x^3-3x^2+4}\)
-- is \((x^2 + 3x + 9) + \frac{24x^2 - 12x - 35}{x^3 - 3x^2 + 4} + \frac{0}{1}\).
--
-- >>> let a = power 5 + power 2 + 1 :: IndexedPolynomial
-- >>> let d = power 3 - 3 * power 2 + 4 :: IndexedPolynomial
-- >>> canonical differentiate $ fromPolynomials a d
-- (x^2 + 3x + 9,Function (24x^2 + (-12)x + (-35)) (x^3 + (-3)x^2 + 4),Function (0) (1))
--
-- In comparison, given the derivation \(Dx = x + 1\), the canonical representation
-- is \((x^2+3x+9) + \frac{215x-319}{9x^2-36x+36} + \frac{1}{9x+9}\).
--
-- >>> canonical (extend (const 0) (power 1 + 1)) $ fromPolynomials a d
-- (x^2 + 3x + 9,Function ((215 % 9)x + ((-319) % 9)) (x^2 + (-4)x + 4),Function ((1 % 9)) (x + 1))
--
-- ==== __Note for readers of Symbolic Integration I: Transcendental Functions__
--
-- Note that the order of the return value is the polynomial, the normal part, and then the special part.
-- This is unlike the order in [Symbolic Integration I: Transcendental Functions](https://doi.org/10.1007/b138171),
-- where the order is the polynomial, the special part, and then the normal part.
-- A different order is used here to be consistent with other functions.
canonical ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  Function (p e c) ->
  (p e c, Function (p e c), Function (p e c))
canonical _ x@(Function _ 0) = (0, 0, x)
canonical derivation (Function a d)
  | Just (b, c) <- xs = (q, fromPolynomials c dn, fromPolynomials b ds)
  | otherwise = (q, 0, fromPolynomials r d)
  where
    a' = scale (1 / leadingCoefficient d) a
    d' = scale (1 / leadingCoefficient d) d
    (q, r) = a' `divide` d'
    (dn, ds) = splitFactor derivation d'
    xs = diophantineEuclidean dn ds r

-- | Check whether a given function \(D\) is consistent with being a derivation
-- with the two given values \(a\) and \(b\).
--
-- Specifically, it checks that the following are true:
--
-- \[ D(a + b) = D(a) + D(b) \]
--
-- \[ D(ab) = a D(b) + b D(a) \]
--
-- For example,
--
-- >>> consistent differentiate (power 1 - 1) (power 3 :: IndexedPolynomial)
-- True
-- >>> consistent abs (-1) 1
-- False
consistent ::
  (Eq a, Num a) =>
  -- | Potential derivation \(D\).
  (a -> a) ->
  -- | Value \(a\).
  a ->
  -- | Value \(b\).
  a ->
  -- | Whether \(D\) is consistent with being a derivation.
  Bool
consistent derivation a b = additive && productive
  where
    additive = derivation (a + b) == derivation a + derivation b
    productive = derivation (a * b) == a * derivation b + b * derivation a

-- | For a differential field \((F, D)\), \(t\) transcendental over \(F\),
-- and \(w \in F(t)\), returns the unique extension \(\Delta\) of \(D\)
-- such that \(\Delta t = w\) and \((F(t), \Delta)\) is a differential field.
--
-- For example, given the derivation \(Dx=0\) on the rational numbers,
-- a differential field \((\mathbb{Q}(t), \Delta)\) can be obtained
-- where the derivation satisfies \(\Delta t = t + 1\).
--
-- >>> let w = power 1 + 1 :: IndexedPolynomial
-- >>> let derivation = extend (const 0) w
-- >>> derivation 0
-- 0
-- >>> derivation $ power 1
-- x + 1
-- >>> derivation $ power 2 + power 1 + 1
-- 2x^2 + 3x + 1
--
-- As another example, given the derivation \(D = \frac{d}{dx}\), we can
-- derive the unique derivation \(\Delta\) such that \(\Delta t = t^2+1\).
--
-- >>> let w' = power 2 + 1 :: IndexedPolynomialWith IndexedPolynomial
-- >>> let derivation' = extend differentiate w'
-- >>> derivation' 0
-- 0
-- >>> derivation' $ power 1
-- [(0,1),(2,1)]
-- >>> derivation' $ scale (power 2) (power 1)
-- [(0,x^2),(1,2x),(2,x^2)]
extend :: (Polynomial p e c, Num (p e c), Eq c, Num c) => (c -> c) -> p e c -> p e c -> p e c
extend derivation w p = getSum $ foldTerms (\e c -> Sum $ derive e c) p
  where
    -- D(ct^e) = t^e Dc + c D(t^e) = t^e Dc + c e t^(e-1) Dt
    derive e c = scale (derivation c) (power e) + scale (c * fromIntegral e) (power $ e - 1) * w
