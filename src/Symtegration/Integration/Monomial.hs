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
module Symtegration.Integration.Monomial (hermiteReduce) where

import Symtegration.Polynomial
import Symtegration.Polynomial.Differential

-- $setup
-- >>> import Symtegration.Polynomial
-- >>> import Symtegration.Polynomial.Differential
-- >>> import Symtegration.Polynomial.Indexed
-- >>> import Symtegration.Symbolic.Haskell

-- | Hermite reduction for a function in a monomial extension.
--
-- Specifically, for a derivation \(D\) and function \(f = \frac{a}{d}\),
-- returns \(g\), \(h = \frac{h_n}{h_d}\), and \(r = r_p + \frac{r_n}{r_d}\) such that
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
-- >>> hermiteReduce deriv (power 4 + power 1 + 1) (power 2)
-- ([((-1),x)],(1,x),(x^2 + (-1),(0,1)))
--
-- Since it is the case that \(\frac{dt}{dx} = t^2 + 1\) for \(t = \tan x\),
-- this implies that
--
-- \[
--   \int \frac{\tan^4 x + \tan x + 1}{\tan^2 x} \, dx =
--   -\frac{1}{\tan x} + \int \left( \frac{1}{\tan x} + \tan^2 x - 1\right) \, dx
-- \]
--
-- \(g\) is returned as a list of rational functions which sum to \(g\)
-- instead of a single rational function, because the former could sometimes
-- be simpler to read.
hermiteReduce ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  -- | Derivation \(D\).
  (p e c -> p e c) ->
  -- | Numerator \(a\).
  p e c ->
  -- | Denominator \(d\).
  p e c ->
  -- | Hermite reduction \((g, (h_n, h_d), (r_p, (r_n, r_d)))\) of \(f\).
  ([(p e c, p e c)], (p e c, p e c), (p e c, (p e c, p e c)))
hermiteReduce _ a 0 = ([], (0, 1), (0, (a, 0)))
hermiteReduce derivation a d = (g, h, (q + p, special))
  where
    (p, (b, n), special) = canonical derivation a d
    (g, h, q) = hermiteReduce' derivation b'' n''

    -- Compute form of b/n where denominator is monic
    -- and has coprime numerator and denominator.
    e = greatestCommonDivisor b n
    (b', _) = b `divide` e
    (n', _) = n `divide` e
    c = 1 / leadingCoefficient n'
    b'' = scale c b'
    n'' = scale c n'

-- | Hermite reduction on the normal part of a canonical representation.
-- This does the main work for 'hermiteReduce', and assumes that the
-- numerator and denominator given are coprime and that the latter is monic.
hermiteReduce' ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  p e c ->
  p e c ->
  ([(p e c, p e c)], (p e c, p e c), p e c)
hermiteReduce' derivation x y
  | (Just (g, (a, d))) <- reduce x [] common =
      let (q, r) = a `divide` d
       in (g, (r, d), q)
  | otherwise = ([], (x, y), 0) -- Should not be possible.
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
          let g' = (b, d) : g
          reduce a' g' d'
      | otherwise = Just (g, (a, divisor))
