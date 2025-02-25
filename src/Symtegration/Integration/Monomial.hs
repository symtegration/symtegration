-- |
-- Module: Symtegration.Integration.Monomial
-- Description: Integration of functions in a monomial extension.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Supports the integration of functions in a monomial extension.
-- Specifically, for a differential field \(k\) with a monomial
-- extension \(k(t)\), attempts to derive an integral for functions
-- of the form
--
-- \[ f(t) = \frac{a(t)}{d(t)} \]
--
-- where \(t\) is a monomial over \(k\), and \(a\) and \(f\) are polynomials of \(t\).
-- Here, a monomial \(t\) with respect to a derivation \(D\) refers to a transcendental
-- over \(k\) for which \(Dt\) is a polynomial of \(t\).  For example, \(t = \tan x\)
-- is a monomial over the real numbers with respect to \(D=\frac{d}{dx}\),
-- since \(\tan x\) is transcendental and \(Dt = t^2 + 1\).
-- It does not mean a polynomial with only one term such as \(c x^n\).
--
-- A function is /simple/ with respect to a derivation \(D\) if it has
-- a denominator \(d\) which is normal, i.e., \(\gcd(d, Dd) = 1\).
-- A function is /reduced/ with respect to a derivation \(D\) if it has
-- a denominator \(d\) which is special, i.e., \(\gcd(d, Dd) = d\).
module Symtegration.Integration.Monomial
  ( hermiteReduce,
    polynomialReduce,
  )
where

import Symtegration.Polynomial
import Symtegration.Polynomial.Differential
import Symtegration.Polynomial.Rational

-- $setup
-- >>> import Symtegration.Polynomial
-- >>> import Symtegration.Polynomial.Differential
-- >>> import Symtegration.Polynomial.Indexed
-- >>> import Symtegration.Polynomial.Rational
-- >>> import Symtegration.Symbolic.Haskell

-- | Hermite reduction for a function in a monomial extension.
--
-- Specifically, for derivation \(D\) and function \(f\),
-- returns \(g\), \(h\), and \(r\) such that
--
-- \[ f = Dg + h + r \]
--
-- where \(h\) is simple and \(r\) is reduced.
--
-- For example, with derivation \(D\) and monomial \(t\) such that \(Dt = t^2 + 1\),
-- it is the case that \(\frac{t^4 + t + 1}{t^2} = Dg + h + r\),
-- where \(g = -\frac{1}{t}\), \(h = \frac{1}{t}\), and \(r = t^2 - 1\).
--
-- >>> let deriv = extend (const 0) (power 2 + 1 :: IndexedPolynomial)
-- >>> hermiteReduce deriv $ fromPolynomials (power 4 + power 1 + 1) (power 2)
-- ([Function ((-1)) (x)],Function (1) (x),Function (x^2 + (-1)) (1))
--
-- Since it is the case that \(\frac{dt}{dx} = t^2 + 1\) for \(t = \tan x\),
-- this implies that
--
-- \[
--   \int \frac{\tan^4 x + \tan x + 1}{\tan^2 x} \, dx =
--   -\frac{1}{\tan x} + \int \left( \frac{1}{\tan x} + \tan^2 x - 1\right) \, dx
-- \]
--
-- \(g\) is returned as a list of functions which sum to \(g\)
-- instead of a single function, because the former could sometimes
-- be simpler to read.
hermiteReduce ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  -- | Derivation \(D\).
  (p e c -> p e c) ->
  -- | Function \(f\).
  Function (p e c) ->
  -- | Hermite reduction \((g, h, r)\) of \(f\).
  ([Function (p e c)], Function (p e c), Function (p e c))
hermiteReduce _ r@(Function _ 0) = ([], 0, r)
hermiteReduce derivation f = (g, h, fromPolynomial (q + p) + s)
  where
    (p, n, s) = canonical derivation f
    (g, h, q) = hermiteReduce' derivation n

-- | Hermite reduction on the normal part of a canonical representation.
-- This does the main work for 'hermiteReduce'.
hermiteReduce' ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  Function (p e c) ->
  ([Function (p e c)], Function (p e c), p e c)
hermiteReduce' derivation f@(Function x y)
  | (Just (g, (a, d))) <- reduce x [] common =
      let (q, r) = a `divide` d
       in (g, fromPolynomials r d, q)
  | otherwise = ([], f, 0) -- Should not be possible.
  where
    common = monic $ greatestCommonDivisor y $ derivation y
    (divisor, _) = y `divide` common
    reduce a g d
      | degree d > 0 = do
          let d' = monic $ greatestCommonDivisor d $ derivation d
          let (d'', _) = d `divide` d'
          let (d''', _) = (divisor * derivation d) `divide` d
          (b, c) <- diophantineEuclidean (-d''') d'' a
          let (b', _) = (derivation b * divisor) `divide` d''
          let a' = c - b'
          let g' = fromPolynomials b d : g
          reduce a' g' d'
      | otherwise = Just (g, (a, divisor))

-- | Polynomial reduction in a monomial extension with the given derivation and polynomial.
-- Specifically, for derivation \(D\) and polynomial \(p\),
-- returns polynomials \(g\) and \(h\) such that
--
-- \[ p = Dg + h \]
--
-- where the degree of \(h\) is less than the degree of \(Dt\),
-- if the latter is larger than 0.
--
-- For example, with derivation \(D\) such that \(Dt = t^2 + 1\),
-- it is the case that \(3t^4 + 3t^3 + 4 = D\left( t^3 + \frac{3}{2}t^2 - 3t \right) - 3t + 7\).
--
-- >>> let derivation = extend (const 0) (power 2 + 1 :: IndexedPolynomial)
-- >>> polynomialReduce derivation $ 3 * power 4 + 3 * power 3 + 4
-- (x^3 + (3 % 2)x^2 + (-3)x,(-3)x + 7)
polynomialReduce ::
  (Polynomial p e c, Num (p e c), Fractional c) =>
  -- | Derivation \(D\).
  (p e c -> p e c) ->
  -- | Polynomial \(p\).
  p e c ->
  -- | Polynomial reduction \((g, h)\).
  (p e c, p e c)
polynomialReduce derivation p
  | delta == 0 = (0, p)
  | degree p < delta = (0, p)
  | otherwise = (q0 + q, r)
  where
    delta = degree $ derivation $ power 1
    lambda = leadingCoefficient $ derivation $ power 1
    m = degree p - delta + 1
    q0 = scale (leadingCoefficient p / (fromIntegral m * lambda)) (power m)

    -- Explicitly delete leading terms so that they are canceled out even for symbolic coefficients.
    (q, r) = polynomialReduce derivation $ deleteLeadingTerm p - deleteLeadingTerm (derivation q0)
