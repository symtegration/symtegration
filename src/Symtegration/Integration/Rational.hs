-- |
-- Module: Symtegration.Integration.Rational
-- Description: Integration of rational functions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Integrates rational functions.
-- Rational functions are ratios of two polynomials, not functions of rational numbers.
-- Only rational number coefficients are supported.
module Symtegration.Integration.Rational
  ( -- * Integration
    integrate,

    -- * Support

    -- | Functions and types useful when integrating rational functions.
    hermiteReduce,
    rationalIntegralLogTerms,
    toRationalFunction,
    RationalFunction (..),
  )
where

import Data.List (find)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Symtegration.Polynomial hiding (integrate)
import Symtegration.Polynomial qualified as Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- $setup
-- >>> :set -w
-- >>> import Symtegration.Polynomial hiding (integrate)
-- >>> import Symtegration.Polynomial.Indexed
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify

-- | Integrate a ratio of two polynomials with rational number coefficients.
--
-- For example, \(\int \frac{x^7-24x^4-4x^2+8x-8}{x^8+6x^6+12x^4+8x^2} \, dx = \frac{3}{x^2+2} + \frac{8x^2+4}{x^5+4x^3+4x} + \log x\):
--
-- >>> let p = "x" ** 7 - 24 * "x" ** 4 - 4 * "x" ** 2 + 8 * "x" - 8
-- >>> let q = "x" ** 8 + 6 * "x" ** 6 + 12 * "x" ** 4 + 8 * "x" ** 2
-- >>> toHaskell . simplify <$> integrate "x" (p / q)
-- Just "(3 / (2 + (x ** 2))) + ((4 + 8 * (x ** 2)) / (4 * x + 4 * (x ** 3) + (x ** 5))) + (log x)"
--
-- For another example, \(\int \frac{36}{x^5-2x^4-2x^3+4x^2+x-2} \, dx = \frac{12x+6}{x^2-1} + 4 \log \left( x - 2 \right) - 4 \log \left( x + 1 \right)\):
--
-- >>> let f = 36 / ("x" ** 5 - 2 * "x" ** 4 - 2 * "x" ** 3 + 4 * "x" ** 2 + "x" - 2)
-- >>> toHaskell . simplify <$> integrate "x" f
-- Just "(-4) * (log (8 + 8 * x)) + 4 * (log (16 + (-8) * x)) + ((6 + 12 * x) / ((-1) + (x ** 2)))"
integrate :: Text -> Expression -> Maybe Expression
integrate v e
  | (x :/: y) <- e',
    (Just n) <- fromExpression (forVariable v) x,
    (Just d) <- fromExpression (forVariable v) y =
      integrate' n d
  | otherwise = Nothing
  where
    e' = simplifyForVariable v e
    integrate' n d = (+) reduced . (+) poly <$> logs
      where
        -- Integrals directly from Hermite reduction.
        (g, h) = hermiteReduce $ toRationalFunction n d
        reduced = sum $ map fromRationalFunction g

        -- Integrate polynomials left over from the Hermite reduction.
        RationalFunction numer denom = h
        (q, r) = numer `divide` denom
        poly = toExpression v toRationalCoefficient $ Polynomial.integrate q

        -- Derive the log terms in the integral.
        h' = toRationalFunction r denom
        logTerms = rationalIntegralLogTerms h'
        logs :: Maybe Expression
        logs
          | (Just terms) <- logTerms = sum <$> toMaybeList (map toLog terms)
          | otherwise = Nothing
          where
            toLog (q', s) = do
              roots <- solve q' :: Maybe [Expression]
              let ss = map (\x -> (x, mapCoefficients (toExpr x) s)) roots
              return $ sum $ map (\(x, p) -> x * Log' (toExpression v toSymbolicCoefficient p)) ss
            toExpr x p = getSum $ foldTerms (\e'' c -> Sum $ fromRational c * (x ** Number (fromIntegral e''))) p

        -- Derive the roots for the given polynomial.
        -- Incomplete for now.
        solve :: IndexedPolynomial -> Maybe [Expression]
        solve p
          | degree p == 1 = Just [fromRational $ (-coefficient p 0) / coefficient p 1]
          | degree p == 2 = solveQuadratic (coefficient p 2) (coefficient p 1) (coefficient p 0)
          | otherwise = Nothing

        -- For now, ignores complex roots.
        solveQuadratic :: Rational -> Rational -> Rational -> Maybe [Expression]
        solveQuadratic a b c
          | sq == 0 = Just [fromRational $ (-b) / (2 * a)]
          | sq > 0 =
              Just
                [ ((-fromRational b) + fromRational sq ** (1 / 2)) / (2 * fromRational a),
                  ((-fromRational b) - fromRational sq ** (1 / 2)) / (2 * fromRational a)
                ]
          | otherwise = Nothing
          where
            sq = b * b - 4 * a * c

        fromRationalFunction (RationalFunction u w) = u' / w'
          where
            u' = toExpression v toRationalCoefficient u
            w' = toExpression v toRationalCoefficient w

-- | Represents the ratio of two polynomials with rational number coefficients.
data RationalFunction = RationalFunction IndexedPolynomial IndexedPolynomial
  deriving (Eq)

instance Show RationalFunction where
  show (RationalFunction n d) = "(" <> show n <> ") / (" <> show d <> ")"

-- | The numerator and denominator in the results
-- for '(+)', '(-)', '(*)', and 'negate' will be coprime.
instance Num RationalFunction where
  (RationalFunction x y) + (RationalFunction u v) =
    toRationalFunction (x * v + u * y) (y * v)

  (RationalFunction x y) - (RationalFunction u v) =
    toRationalFunction (x * v - u * y) (y * v)

  (RationalFunction x y) * (RationalFunction u v) =
    toRationalFunction (x * u) (y * v)

  abs = id

  signum 0 = 0
  signum _ = 1

  fromInteger n = RationalFunction (fromInteger n) 1

instance Fractional RationalFunction where
  fromRational q = RationalFunction (scale q 1) 1
  recip (RationalFunction p q) = RationalFunction q p

-- | Form a rational function from two polynomials.
-- The polynomials will be reduced so that the numerator and denominator are coprime.
toRationalFunction ::
  -- | Numerator.
  IndexedPolynomial ->
  -- | Denominator.
  IndexedPolynomial ->
  RationalFunction
toRationalFunction x 0 = RationalFunction x 0
toRationalFunction x y = RationalFunction x' y'
  where
    g = monic $ greatestCommonDivisor x y
    (x', _) = x `divide` g
    (y', _) = y `divide` g

-- | Applies Hermite reduction to a rational function.
-- Returns a list of rational functions whose sums add up to the integral
-- and a rational function which remains to be integrated.
-- Only rational functions with rational number coefficients and
-- where the numerator and denominator are coprime are supported.
--
-- Specifically, for rational function \(x = \frac{A}{D}\),
-- where \(A\) and \(D\) are coprime polynomials, then for return value @(gs, h)@,
-- the sum of @gs@ is equal to \(g\) and @h@ is equal to \(h\) in the following:
--
-- \[ \frac{A}{D} = \frac{dg}{dx} + h \]
--
-- This is equivalent to the following:
--
-- \[ \int \frac{A}{D} \, dx = g + \int h \, dx \]
--
-- If preconditions are satisfied, i.e., \(D \neq 0\) and \(A\) and \(D\) are coprime,
-- then \(h\) will have a squarefree denominator.
--
-- \(g\) is returned as a list of rational functions which sum to \(g\)
-- instead of a single rational function, because the former could sometimes
-- be simpler to read.
hermiteReduce :: RationalFunction -> ([RationalFunction], RationalFunction)
hermiteReduce h@(RationalFunction _ 0) = ([], h)
hermiteReduce h@(RationalFunction x y)
  | (Just z) <- reduce x [] common = z
  | otherwise = ([], h) -- Should never happen, but a fallback if it does.
  where
    common = monic $ greatestCommonDivisor y $ differentiate y
    (divisor, _) = y `divide` common
    reduce a g d
      | degree d > 0 = do
          let d' = monic $ greatestCommonDivisor d $ differentiate d
          let (d'', _) = d `divide` d'
          let (d''', _) = (divisor * differentiate d) `divide` d
          (b, c) <- diophantineEuclidean (-d''') d'' a
          let (b', _) = (differentiate b * divisor) `divide` d''
          let a' = c - b'
          let g' = toRationalFunction b d : g
          reduce a' g' d'
      | otherwise = Just (g, toRationalFunction a divisor)

-- | For rational function \(\frac{A}{D}\), where \(\deg(A) < \deg(D)\),
-- and \(D\) is non-zero, squarefree, and coprime with \(A\),
-- returns the components which form the logarithmic terms of \(\int \frac{A}{D} \, dx\).
-- Specifically, when a list of \((Q_i(t), S_i(t, x))\) is returned,
-- where \(Q_i(t)\) are polynomials of \(t\) and \(S_i(t, x)\) are polynomials of \(x\)
-- with coefficients formed from polynomials of \(t\), then
--
-- \[
-- \int \frac{A}{D} \, dx = \sum_{i=1}^n \sum_{a \in \{t \mid Q_i(t) = 0\}} a \log \left(S_i(a,x)\right)
-- \]
--
-- For example,
--
-- >>> let p = power 4 - 3 * power 2 + 6 :: IndexedPolynomial
-- >>> let q = power 6 - 5 * power 4 + 5 * power 2 + 4 :: IndexedPolynomial
-- >>> let f = toRationalFunction p q
-- >>> let gs = rationalIntegralLogTerms f
-- >>> length <$> gs
-- Just 1
-- >>> fst . head <$> gs
-- Just x^2 + (1 % 4)
-- >>> foldTerms (\e c -> show (e, c) <> " ") . snd . head <$> gs
-- Just "(0,792x^2 + (-16)) (1,(-2440)x^3 + 32x) (2,(-400)x^2 + 7) (3,800x^3 + (-14)x) "
--
-- so it is the case that
--
-- \[
-- \int \frac{x^4-3x^2+6}{x^6-5x^4+5x^2+4} \, dx
-- = \sum_{a \mid a^2+\frac{1}{4} = 0} a \log \left( (800a^3-14a)x^3+(-400a^2+7)x^2+(-2440a^3+32a)x + 792a^2-16 \right)
-- \]
--
-- It may return 'Nothing' if \(\frac{A}{D}\) is not in the expected form.
rationalIntegralLogTerms ::
  RationalFunction ->
  Maybe [(IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial)]
rationalIntegralLogTerms (RationalFunction a d) = do
  -- For A/D, get the resultant and subresultant polynomial remainder sequence
  -- for D and (A - t * D').
  let sa = mapCoefficients fromRational a
  let sd = mapCoefficients fromRational d
  let t = RationalFunction (power 1) 1
  let (resultant, prs) = subresultant sd $ sa - scale t (differentiate sd)

  -- Turn rational functions into polynomials if possible.
  -- When the preconditions are satisfied, these should all be polynomials.
  sd' <- toPolyCoefficients sd
  resultant' <- toPoly resultant
  prs' <- toMaybeList $ map toPolyCoefficients prs :: Maybe [IndexedPolynomialWith IndexedPolynomial]

  -- Derive what make up the log terms in the integral.
  let qs = squarefree resultant' :: [IndexedPolynomial]
  let terms = zipWith (toTerm sd' prs') [1 ..] qs

  -- Ignore log terms which end up being multiples of 0 = log 1.
  return $ filter ((/=) 1 . snd) terms
  where
    toTerm ::
      IndexedPolynomialWith IndexedPolynomial ->
      [IndexedPolynomialWith IndexedPolynomial] ->
      Int ->
      IndexedPolynomial ->
      (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial)
    toTerm sd prs i q
      | degree q == 0 = (q, 1)
      | i == degree d = (q, sd)
      | (Just r) <- find ((==) i . degree) prs = derive q r
      | otherwise = (q, 1)

    derive ::
      IndexedPolynomial ->
      IndexedPolynomialWith IndexedPolynomial ->
      (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial)
    derive q s = (q, s')
      where
        as = squarefree $ leadingCoefficient s
        s' = foldl scalePoly s (zip ([1 ..] :: [Int]) as)
          where
            scalePoly x (j, u) =
              getSum $ foldTerms (reduceTerm (monic $ greatestCommonDivisor u q ^ j)) x
            reduceTerm v e c = Sum $ scale (exactDivide c v) $ power e
            exactDivide u v = r
              where
                (r, _) = u `divide` v

    -- Turn the rational function into a polynomial if possible.
    toPoly :: RationalFunction -> Maybe IndexedPolynomial
    toPoly (RationalFunction p q)
      | degree q == 0 = Just p'
      | otherwise = Nothing
      where
        p' = scale (1 / leadingCoefficient q) p

    -- Turn the rational function coefficients into polynomial coefficients if possible.
    toPolyCoefficients ::
      IndexedPolynomialWith RationalFunction ->
      Maybe (IndexedPolynomialWith IndexedPolynomial)
    toPolyCoefficients p = reconstruct terms
      where
        terms = foldTerms (\e c -> [(e, toPoly c)]) p
        reconstruct [] = Just 0
        reconstruct ((_, Nothing) : _) = Nothing
        reconstruct ((e, Just c) : xs)
          | (Just p') <- reconstruct xs = Just $ scale c (power e) + p'
          | otherwise = Nothing

-- If there are any nothings, then turn the list into nothing.
-- Otherwise, turn it into the list of just the elements.
toMaybeList :: [Maybe a] -> Maybe [a]
toMaybeList [] = Just []
toMaybeList (Nothing : _) = Nothing
toMaybeList (Just x : xs)
  | (Just xs') <- toMaybeList xs = Just (x : xs')
  | otherwise = Nothing
