module Symtegration.Polynomial where

import Data.Bitraversable

class (Integral e, Num c, Bitraversable p) => Polynomial p e c where
  degree :: p e c -> e
  coefficient :: p e c -> e -> c
  leadingCoefficient :: p e c -> c
  scale :: c -> p e c -> p e c
  power :: e -> p e c

-- |
-- prop> let (q, r) = polynomialDivide a b in a === b * q + r
polynomialDivide ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  p e c ->
  p e c ->
  (p e c, p e c)
polynomialDivide p q = divide 0 p
  where
    divide quotient remainder
      | remainder == 0 = (quotient, remainder)
      | delta < 0 = (quotient, remainder)
      | otherwise = divide (quotient + t) (remainder - q * t)
      where
        delta = degree remainder - degree q
        t = scale (leadingCoefficient remainder / leadingCoefficient q) $ power delta

-- |
-- prop> let (s, t, g) = extendedEuclidean a b in s * a + t * b === g
extendedEuclidean ::
  (Polynomial p e c, Eq (p e c), Num (p e c), Fractional c) =>
  p e c ->
  p e c ->
  (p e c, p e c, p e c)
extendedEuclidean u v = descend u v 1 0 0 1
  where
    descend g 0 s t _ _ = (s, t, g)
    descend a b a1 a2 b1 b2 = descend b r b1 b2 r1 r2
      where
        (q, r) = polynomialDivide a b
        r1 = a1 - q * b1
        r2 = a2 - q * b2
