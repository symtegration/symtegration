module Symtegration.Polynomial.Differential
  ( splitFactor,
    splitSquarefreeFactor,
    canonical,

    -- * Utility functions
    extend,
    consistent,
  )
where

import Data.Monoid (Sum (..))
import Symtegration.Polynomial

-- $setup
-- >>> import Symtegration.Polynomial
-- >>> import Symtegration.Polynomial.Indexed

-- |
-- >>> splitSquarefreeFactor differentiate (power 3 - 3 * power 2 + 4 :: IndexedPolynomial)
-- [(x + 1,1),(x + (-2),1)]
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

splitFactor ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  p e c ->
  (p e c, p e c)
splitFactor derivation p = compose (1, 1) factors (1 :: Int)
  where
    factors = splitSquarefreeFactor derivation p
    compose split [] _ = split
    compose (pn, ps) ((qn, qs) : fs) n = compose (pn * (qn ^ n), ps * (qs ^ n)) fs (n + 1)

canonical ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Eq c, Fractional c) =>
  (p e c -> p e c) ->
  p e c ->
  p e c ->
  (p e c, (p e c, p e c), (p e c, p e c))
canonical _ _ 0 = (0, (0, 1), (0, 0))
canonical derivation a d
  | Just (b, c) <- xs = (q, (c, dn), (b, ds))
  | otherwise = (q, (0, 1), (r, d))
  where
    (q, r) = a `divide` d
    (dn, ds) = splitFactor derivation d
    xs = diophantineEuclidean dn ds r

consistent :: (Eq a, Num a) => (a -> a) -> a -> a -> Bool
consistent derivation a b = additive && productive
  where
    additive = derivation (a + b) == derivation a + derivation b
    productive = derivation (a * b) == a * derivation b + b * derivation a

extend :: (Polynomial p e c, Num (p e c), Eq c, Num c) => (c -> c) -> p e c -> p e c -> p e c
extend derivation w p
  | degree p == 0 = scale (derivation $ coefficient p 0) (power 0)
  | otherwise = getSum $ foldTerms (\e c -> Sum $ derive e c) p
  where
    -- D(ct^e) = t^e Dc + c D(t^e) = t^e Dc + c e t^(e-1) Dt
    derive e c = scale (derivation c) (power e) + scale (c * fromIntegral e) (power $ e - 1) * w
