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
    residueReduce,
  )
where

import Data.List (find)
import Data.Monoid (Sum (..))
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

-- | Resultant reduction of a simple function in a monomial extension.
--
-- Specifically, for a derivation \(D\) on \(k(t)\) and a simple function \(f \in k(t)\),
-- returns logarithmic terms \((s_i, S_i)\) such that
--
-- \[ g = \sum_i \sum_{\alpha \mid s_i(\alpha) = 0} \alpha \log S_i(\alpha, t) \]
--
-- and also whether \(f\) has an elemantary integral.  If \(f\) has an elementary integral,
-- then there is a polynomial \(h\) of the monomial \(t\) such that \(f - Dg = h\).
--
-- For example, for derivation \(D=\frac{d}{dx}\) and simple function \(f = \frac{t^2+2t+1}{t^2-t+1}\),
--
-- >>> residueReduce differentiate $ fromPolynomials (power 2 + 2 * power 1 + 1) (2 * power 2 - 2 * power 1 - 1 :: IndexedPolynomial)
-- Just ([(x^2 + ((-3) % 2)x + ((-3) % 16),[(0,(4 % 9)x + (1 % 3)),(1,((-8) % 9)x + (2 % 3))])],True)
--
-- so that
--
-- \[ g = \sum_{\alpha \mid \alpha^2-\frac{3}{2}\alpha-\frac{3}{16} = 0} \log \left( (\frac{2}{3}-\frac{8}{9}\alpha)t + \frac{4}{9}\alpha+\frac{1}{3} \right) \]
--
-- and \(\int f \, dx\) has an elementary integral, which happens to be
--
-- \[ \int f \, dx = g + \int h \, dx \]
--
-- where \(h = f-Dg\) is a polynomial of the monomial \(t\).
residueReduce ::
  ( Polynomial p e c,
    Eq (p e c),
    Num (p e c),
    Polynomial p e (p e c),
    Eq (p e (p e c)),
    Num (p e (p e c)),
    Polynomial p e (Function (p e c)),
    Eq (p e (Function (p e c))),
    Num (p e (Function (p e c))),
    Eq c,
    Fractional c
  ) =>
  -- | Derivation \(D\).
  (p e c -> p e c) ->
  -- | Function \(f\).
  Function (p e c) ->
  -- | Logarithmic terms \((s_i, S_i)\) and whether an elementary integral for \(\int f \, dx\) exists.
  Maybe ([(p e c, p e (p e c))], Bool)
residueReduce derivation (Function e d) = do
  -- Polynomials of monomial t.
  let (_, a) = e `divide` d

  -- Coefficients are polynomials of z in rational function form.
  let d' = mapCoefficients (\c -> fromPolynomial $ scale c 1) d
  let a' = mapCoefficients (\c -> fromPolynomial $ scale c 1) a
  let zd' = mapCoefficients (\c -> fromPolynomial $ scale c $ power 1) (derivation d)

  let (r', rs')
        | degree (derivation d) <= degree d = subresultant d' (a' - zd')
        | otherwise = subresultant (a' - zd') d'

  -- Polynomial of z.
  r <- toPolynomial r'

  -- Polynomials of t with coefficients which are polynomials of z.
  rs <- mapM (mapCoefficientsM toPolynomial) rs'

  -- Splitting factorization with respect to coefficient lifting.
  -- Polynomials of z with coefficients which are polynomials of t.
  let kderiv = getSum . foldTerms (\ex c -> Sum $ derivation (scale c 1) * power ex)
  let factors = splitSquarefreeFactor kderiv r

  -- Whether there exists an elementary integral for the function of t in field c.
  let elementary = all ((==) 0 . degree . fst) factors

  -- Derive g.  For each term (s, S), each s is a polynomial of z,
  -- and each S is a polynomial of t with coefficients which are polynomials of z.
  let specials = map snd factors
  terms' <- mapM (toTerm rs specials) $ zip [1 ..] specials

  -- Remove terms which will be equal to zero.
  let terms = filter ((/= 1) . snd) terms'

  return (terms, elementary)
  where
    -- Return the logarithmic term corresponding to the given special factor.
    toTerm prs specials (i, s)
      | degree s == 0 = Just (monic s, 1)
      | degree d == i = Just (monic s, mapCoefficients (`scale` 1) d)
      | Just pr <- find ((==) i . degree) prs = derive s pr specials
      | otherwise = Nothing -- Should not be possible.

    -- Derive the logarithmic term given the polynomial remaindder.
    derive s pr specials = do
      -- Switch to a polynomial of z with coefficients which are polynomials of t.
      let pr' = mapCoefficients fromPolynomial $ switchVars pr

      let (logArg', _) = pr' `divide` divisor
      logArg <- mapCoefficientsM toPolynomial logArg'

      -- Return the logarithmic term,
      -- with the argument switched back to a polynomial of t with coefficients of z.
      return (monic s, switchVars logArg)
      where
        -- Polynomials of z.
        factors = squarefree $ leadingCoefficient pr

        -- Polynomial of z with coefficients which are polynomials of t.
        divisor = mapCoefficients (fromPolynomial . flip scale 1) divisor'
          where
            divisor' = product $ map toDivisor $ zip3 [1 ..] factors specials
            toDivisor (j, factor, special) = greatestCommonDivisor factor special ^ (j :: Int)

-- | Given a polynomial with coefficients which are polynomials of a different variable,
-- return an equivalent polynomial of the different variable with coefficients
-- which are polynomials of the original variable.
--
-- For example, a polynomial such as
--
-- \[ (y^2+1)x^3 + (2y^2-y)x + 2y - 1 \]
--
-- would be turned into
--
-- \[ (x^3 + 2x) y^2 - (x-2)y + x^3 - 1 \]
switchVars ::
  (Polynomial p e (p e c), Polynomial p e c, Num (p e (p e c))) =>
  -- | Polynomial of \(v_1\) with coefficients which are polynomials of \(v_2\).
  p e (p e c) ->
  -- | Polynomial of \(v_2\) with coefficients which are polynomials of \(v_1\).
  p e (p e c)
switchVars p = getSum $ foldTerms (\e c -> Sum $ mapCoefficients (\c' -> scale c' $ power e) c) p
