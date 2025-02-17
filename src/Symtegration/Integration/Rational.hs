-- |
-- Module: Symtegration.Integration.Rational
-- Description: Integration of rational functions.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- Integrates rational functions.
-- Rational functions are ratios of two polynomials, not functions of rational numbers.
-- Only rational number coefficients are supported.
module Symtegration.Integration.Rational
  ( -- * Integration
    integrate,

    -- * Algorithms

    -- | Algorithms used for integrating rational functions.
    hermiteReduce,
    rationalIntegralLogTerms,
    complexLogTermToAtan,
    complexLogTermToRealTerm,

    -- * Types
    RationalFunction,
  )
where

import Data.Foldable (asum)
import Data.List (find, intersect)
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Symtegration.Polynomial hiding (integrate)
import Symtegration.Polynomial qualified as Polynomial
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Rational as Rational
import Symtegration.Polynomial.Solve
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Symtegration.Symbolic.Simplify

-- $setup
-- >>> :set -w
-- >>> import Symtegration.Polynomial hiding (integrate)
-- >>> import Symtegration.Polynomial.Indexed
-- >>> import Symtegration.Polynomial.Rational
-- >>> import Symtegration.Symbolic.Haskell
-- >>> import Symtegration.Symbolic.Simplify

-- | Integrate a ratio of two polynomials with rational number coefficients.
--
-- For example,
--
-- >>> let p = "x" ** 7 - 24 * "x" ** 4 - 4 * "x" ** 2 + 8 * "x" - 8
-- >>> let q = "x" ** 8 + 6 * "x" ** 6 + 12 * "x" ** 4 + 8 * "x" ** 2
-- >>> toHaskell . simplify <$> integrate "x" (p / q)
-- Just "3 / (2 + x ** 2) + (4 + 8 * x ** 2) / (4 * x + 4 * x ** 3 + x ** 5) + log x"
--
-- so that
--
-- \[\int \frac{x^7-24x^4-4x^2+8x-8}{x^8+6x^6+12x^4+8x^2} \, dx = \frac{3}{x^2+2} + \frac{8x^2+4}{x^5+4x^3+4x} + \log x\]
--
-- For another example,
--
-- >>> let f = 36 / ("x" ** 5 - 2 * "x" ** 4 - 2 * "x" ** 3 + 4 * "x" ** 2 + "x" - 2)
-- >>> toHaskell . simplify <$> integrate "x" f
-- Just "(-4) * log (8 + 8 * x) + 4 * log (16 + (-8) * x) + (6 + 12 * x) / ((-1) + x ** 2)"
--
-- so that
--
-- \[\int \frac{36}{x^5-2x^4-2x^3+4x^2+x-2} \, dx = \frac{12x+6}{x^2-1} + 4 \log \left( x - 2 \right) - 4 \log \left( x + 1 \right)\]
--
-- This function will attempt to find a real function integral if it can,
-- but if it cannot, it will try to find an integral which includes complex logarithms.
integrate :: Text -> Expression -> Maybe Expression
integrate v e
  | (x :/: y) <- e',
    (Just n) <- fromExpression (forVariable v) x,
    (Just d) <- fromExpression (forVariable v) y,
    d /= 0 =
      integrate' n d
  | otherwise = Nothing
  where
    e' = simplifyForVariable v e
    integrate' n d = (+) reduced . (+) poly <$> logs
      where
        -- Integrals directly from Hermite reduction.
        (g, h) = hermiteReduce $ fromPolynomials n d
        reduced = sum $ map fromRationalFunction g

        -- Integrate polynomials left over from the Hermite reduction.
        Rational.Function numer denom = h
        (q, r) = numer `divide` denom
        poly = toExpression v toRationalCoefficient $ Polynomial.integrate q

        -- Derive the log terms in the integral.
        h' = fromPolynomials r denom
        logTerms = rationalIntegralLogTerms h'
        logs = asum [realLogs, complexLogs] :: Maybe Expression

        -- Try to integrate into real functions first.
        realLogs
          | (Just terms) <- logTerms = sum <$> mapM (complexLogTermToRealExpression v) terms
          | otherwise = Nothing

        -- If it cannot be integrated into real functions, allow complex logarithms.
        complexLogs
          | (Just terms) <- logTerms = sum <$> mapM (complexLogTermToComplexExpression v) terms
          | otherwise = Nothing

        fromRationalFunction (Rational.Function u w) = u' / w'
          where
            u' = toExpression v toRationalCoefficient u
            w' = toExpression v toRationalCoefficient w

-- | Rational functions which can be integrated by this module.
type RationalFunction = Rational.Function IndexedPolynomial

-- | Applies Hermite reduction to a rational function.
-- Returns a list of rational functions whose sums add up to the integral
-- and a rational function which remains to be integrated.
-- Only rational functions with rational number coefficients and
-- where the numerator and denominator are coprime are supported.
--
-- Specifically, for rational function \(f = \frac{A}{D}\),
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
-- For example,
--
-- >>> let p = power 7 - 24 * power 4 - 4 * power 2 + 8 * power 1 - 8 :: IndexedPolynomial
-- >>> let q = power 8 + 6 * power 6 + 12 * power 4 + 8 * power 2 :: IndexedPolynomial
-- >>> hermiteReduce $ fromPolynomials p q
-- ([Function (3) (x^2 + 2),Function (8x^2 + 4) (x^5 + 4x^3 + 4x)],Function (1) (x))
--
-- so that
--
-- \[\int \frac{x^7-24x^4-4x^2+8x-8}{x^8+6x^6+12x^4+8x^2} \, dx = \frac{3}{x^2+2}+\frac{8x^2+4}{x^5+4x^3+4x}+\int \frac{1}{x} \, dx\]
--
-- \(g\) is returned as a list of rational functions which sum to \(g\)
-- instead of a single rational function, because the former could sometimes
-- be simpler to read.
hermiteReduce :: RationalFunction -> ([RationalFunction], RationalFunction)
hermiteReduce h@(Rational.Function _ 0) = ([], h)
hermiteReduce h@(Rational.Function x y)
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
          let g' = fromPolynomials b d : g
          reduce a' g' d'
      | otherwise = Just (g, fromPolynomials a divisor)

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
-- >>> let f = fromPolynomials p q
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
rationalIntegralLogTerms (Rational.Function a d) = do
  -- For A/D, get the resultant and subresultant polynomial remainder sequence
  -- for D and (A - t * D').
  let sa = mapCoefficients fromRational a
  let sd = mapCoefficients fromRational d
  let t = fromPolynomial $ power 1
  let (resultant, prs) = subresultant sd $ sa - scale t (differentiate sd)

  -- Turn rational functions into polynomials if possible.
  -- When the preconditions are satisfied, these should all be polynomials.
  sd' <- mapCoefficientsM toPolynomial sd
  resultant' <- toPolynomial resultant
  prs' <- mapM (mapCoefficientsM toPolynomial) prs :: Maybe [IndexedPolynomialWith IndexedPolynomial]

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

-- | Given polynomials \(A\) and \(B\),
-- return a sum \(f\) of inverse tangents such that the following is true.
--
-- \[
-- \frac{df}{dx} = \frac{d}{dx} i \log \left( \frac{A + iB}{A - iB} \right)
-- \]
--
-- This allows integrals to be evaluated with only real-valued functions.
-- It also avoids the discontinuities in real-valued indefinite integrals which may result
-- when the integral uses logarithms with complex arguments.
--
-- For example,
--
-- >>> toHaskell $ simplify $ complexLogTermToAtan "x" (power 3 - 3 * power 1) (power 2 - 2)
-- "2 * atan x + 2 * atan ((x + (-3) * x ** 3 + x ** 5) / 2) + 2 * atan (x ** 3)"
--
-- so it is the case that
--
-- \[ \frac{d}{dx} \left( i \log \left( \frac{(x^3-3x) + i(x^2-2)}{(x^3-3x) - i(x^2-2)} \right) \right) =
-- \frac{d}{dx} \left( 2 \tan^{-1} \left(\frac{x^5-3x^3+x}{2}\right) + 2 \tan^{-1} \left(x^3\right) + 2 \tan^{-1} x \right) \]
complexLogTermToAtan ::
  -- | Symbol for the variable.
  Text ->
  -- | Polynomial \(A\).
  IndexedPolynomial ->
  -- | Polynomial \(B\).
  IndexedPolynomial ->
  -- | Sum \(f\) of inverse tangents.
  Expression
complexLogTermToAtan v a b
  | r == 0 = 2 * atan (a' / b')
  | degree a < degree b = complexLogTermToAtan v (-b) a
  | otherwise = 2 * atan (s' / g') + complexLogTermToAtan v d c
  where
    (_, r) = a `divide` b
    (d, c, g) = extendedEuclidean b (-a)
    a' = toExpression v toRationalCoefficient a
    b' = toExpression v toRationalCoefficient b
    g' = toExpression v toRationalCoefficient g
    s' = toExpression v toRationalCoefficient $ a * d + b * c

-- | For the ingredients of a complex logarithm, return the ingredients of an equivalent real function in terms of an indefinite integral.
--
-- Specifically, for polynomials \(\left(R(t), S(t,x)\right)\) such that
--
-- \[
-- \frac{df}{dx} = \frac{d}{dx} \sum_{\alpha \in \{ t \mid R(t) = 0 \}} \left( \alpha \log \left( S(\alpha,x) \right) \right)
-- \]
--
-- then with return value \(\left( \left(P(u,v), Q(u,v)\right), \left(A(u,v,x), B(u,v,x)\right) \right)\),
-- and a return value \(g_{uv}\) from 'complexLogTermToAtan' for \(A(u,v)\) and \(B(u,v)\), the real function is
--
-- \[
-- \frac{df}{dx} = \frac{d}{dx} \left(
-- \sum_{(a,b) \in \{(u,v) \in (\mathbb{R}, \mathbb{R}) \mid P(u,v)=Q(u,v)=0, b > 0\}}
--   \left( a \log \left( A(a,b,x)^2 + B(a,b,x)^2 \right) + b g_{ab}(x) \right)
-- + \sum_{a \in \{t \in \mathbb{R} \mid R(t)=0 \}} \left( a \log (S(a,x)) \right)
-- \right)
-- \]
--
-- The return value are polynomials \(\left( (P,Q), (A,B) \right)\), where
--
-- * \(P\) is a \(u\)-polynomial, i.e., a polynomial with variable \(u\), with coefficients which are \(v\)-polynomials.
--
-- * \(Q\) is a \(u\)-polynomial, with coefficients which are \(v\)-polynomials.
--
-- * \(A\) is an \(x\)-polynomial, with coefficients which are \(u\)-polynomials, which in turn have coefficients with \(v\)-polynomials.
--
-- * \(B\) is an \(x\)-polynomial, with coefficients which are \(u\)-polynomials, which in turn have coefficients with \(v\)-polynomials.
--
-- For example,
--
-- >>> let r = 4 * power 2 + 1 :: IndexedPolynomial
-- >>> let s = power 3 + scale (2 * power 1) (power 2) - 3 * power 1 - scale (4 * power 1) 1 :: IndexedPolynomialWith IndexedPolynomial
-- >>> complexLogTermToRealTerm (r, s)
-- (([(0,(-4)x^2 + 1),(2,4)],[(1,8x)]),([(0,[(1,(-4))]),(1,[(0,(-3))]),(2,[(1,2)]),(3,[(0,1)])],[(0,[(0,(-4)x)]),(2,[(0,2x)])]))
--
-- While the return value may be hard to parse, this means:
--
-- \[
-- \begin{align*}
-- P & = 4u^2 - 4v^2 + 1 \\
-- Q & = 8uv \\
-- A & = x^3 + 2ux^2 - 3x - 4u \\
-- B & = 2vx^2 - 4v
-- \end{align*}
-- \]
complexLogTermToRealTerm ::
  (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial) ->
  ( (IndexedPolynomialWith IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial),
    (IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial), IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial))
  )
complexLogTermToRealTerm (q, s) = ((qp, qq), (sp, sq))
  where
    -- For all of the following, i is the imaginary number.
    -- We use an i polynomial instead of Complex to represent complex numbers
    -- because the Complex a is not an instance of the Num class unless a is
    -- an instance of the RealFloat class.

    -- We use polynomial coefficients to introduce a separate variable.
    -- An alternative would have been to use Expression coefficients,
    -- but this would require a guarantee that we can rewrite an Expression
    -- down to the degree where we can tease apart the real and imaginary parts
    -- in a complex number.

    -- Compute q(u+iv) as an i polynomial with coefficients
    -- of u polynomials with coefficients
    -- of v polynomials with rational coefficients.
    q' = getSum $ foldTerms reduceImaginary $ getSum $ foldTerms fromTerm q
      where
        fromTerm :: Int -> Rational -> Sum (IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial))
        fromTerm e c = Sum $ c' * (u + i * v) ^ e
          where
            c' = scale (scale (scale c 1) 1) 1
        i = power 1
        u = scale (power 1) 1
        v = scale (scale (power 1) 1) 1
    -- q' == qp + i * qq
    (qp, qq) = (coefficient q' 0, coefficient q' 1)

    -- Compute s(u+iv,x) as an i polynomial with coefficients
    -- of x polynomials with coefficients
    -- of u polynomials with coefficients
    -- of v polynomials with rational coefficients.
    s' = getSum $ foldTerms reduceImaginary $ getSum $ foldTerms fromTerm s
      where
        fromTerm :: Int -> IndexedPolynomial -> Sum (IndexedPolynomialWith (IndexedPolynomialWith (IndexedPolynomialWith IndexedPolynomial)))
        fromTerm e c = Sum $ c' * x ^ e
          where
            c' = getSum $ foldTerms fromCoefficient c
            fromCoefficient e' c'' = Sum $ c''' * (u + i * v) ^ e'
              where
                c''' = scale (scale (scale (scale c'' 1) 1) 1) 1
        i = power 1
        x = scale (power 1) 1
        u = scale (scale (power 1) 1) 1
        v = scale (scale (scale (power 1) 1) 1) 1
    -- s' = sp + i * sq
    (sp, sq) = (coefficient s' 0, coefficient s' 1)

    -- For terms in polynomials of i, reduce them to the form x or i*x.
    reduceImaginary :: (Eq a, Num a) => Int -> a -> Sum (IndexedPolynomialWith a)
    reduceImaginary e c = Sum $ case e `mod` 4 of
      0 -> c'
      1 -> c' * i
      2 -> c' * (-1)
      3 -> c' * (-i)
      _ -> 0 -- Not possible.
      where
        i = power 1
        c' = scale c 1

-- | For the ingredients of a complex logarithm, return an equivalent real function in terms of an indefinite integral.
--
-- Specifically, for polynomials \(\left(R(t), S(t,x)\right)\) such that
--
-- \[
-- \frac{df}{dx} = \frac{d}{dx} \sum_{\alpha \in \{ t \mid R(t) = 0 \}} \left( \alpha \log \left( S(\alpha,x) \right) \right)
-- \]
--
-- a symbolic representation for \(f\) will be returned.  See 'complexLogTermToRealTerm' for specifics as to how \(f\) is derived.
complexLogTermToRealExpression ::
  -- | Symbol for the variable.
  Text ->
  -- | Polynomials \(R(t)\) and \(S(t,x)\).
  (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial) ->
  -- | Expression for the real function \(f\).
  Maybe Expression
complexLogTermToRealExpression v (r, s)
  | (Just xys) <- solveBivariatePolynomials p q,
    (Just h) <- f xys,
    (Just zs) <- toRationalList (solve r) =
      Just $ sum h + g zs
  | otherwise = Nothing
  where
    ((p, q), (a, b)) = complexLogTermToRealTerm (r, s)

    f :: [(Rational, Rational)] -> Maybe [Expression]
    f xys = sequence $ do
      (x, y) <- filter ((> 0) . snd) xys
      let flatten'' = mapCoefficients (toExpr (fromRational y) fromRational) -- v-polynomials into Expressions.
      let flatten' = mapCoefficients (toExpr (fromRational x) id . flatten'') -- u-polynomials into Expressions.
      let flatten = toExpr (Symbol v) id . flatten' -- x-polynomials into Expressions.
      -- a and b flattened into Expressions.
      let a' = flatten a
      let b' = flatten b
      -- a and b flattened into x-polynomials with rational number coefficients.
      return $ do
        a'' <- convertCoefficients $ flatten' a
        b'' <- convertCoefficients $ flatten' b
        return $ fromRational x * log (a' * a' + b' * b') + fromRational y * complexLogTermToAtan v a'' b''

    g zs = sum $ do
      z <- zs
      let s' = mapCoefficients (toExpr (fromRational z) fromRational) s
      return $ fromRational z * Log' (toExpression v toSymbolicCoefficient s')

    toRationalList :: Maybe [Expression] -> Maybe [Rational]
    toRationalList Nothing = Nothing
    toRationalList (Just []) = Just []
    toRationalList (Just (x : xs))
      | (Just x'') <- convert (simplify x'), (Just xs'') <- xs' = Just $ x'' : xs''
      | otherwise = Nothing
      where
        x' = simplify x
        xs' = toRationalList $ Just xs

    -- Convert a simplified Expression into a rational number.
    convert (Number n) = Just $ fromIntegral n
    convert (Number n :/: Number m) = Just $ fromIntegral n / fromIntegral m
    convert _ = Nothing

    -- Convert polynomial with Expression coefficients into a polynomial with rational number coefficients.
    convertCoefficients :: IndexedPolynomialWith Expression -> Maybe IndexedPolynomial
    convertCoefficients x = sum . map (\(e, c) -> scale c (power e)) <$> sequence (foldTerms (\e c -> [(e,) <$> convert (simplify c)]) x)

    -- Turns a polynomial into an Expression.
    -- Function h is used to turn the coefficient into an Expression.
    toExpr x h u = getSum $ foldTerms (\e'' c -> Sum $ h c * (x ** Number (fromIntegral e''))) u

-- | From the ingredients of a complex logarithm, return the expression for the complex algorithm.
-- Specifically, for polynomials \(\left(Q(t), S(t,x)\right)\),
-- a symbolic representation for the following will be returned.
--
-- \[
-- \sum_{\alpha \in \{ t \mid Q(t) = 0 \}} \left( \alpha \log \left( S(\alpha,x) \right) \right)
-- \]
complexLogTermToComplexExpression ::
  -- | Symbol for the variable.
  Text ->
  -- | Polynomials \(Q(t)\) and \(S(t,x)\).
  (IndexedPolynomial, IndexedPolynomialWith IndexedPolynomial) ->
  -- | Expression for the logarithm.
  Maybe Expression
complexLogTermToComplexExpression v (q, s) = do
  as <- complexSolve q
  let terms = do
        a <- as
        let s' = mapCoefficients (collapse a) s
        let s'' = toExpression v toSymbolicCoefficient s'
        return $ a * log s''
  return $ sum terms
  where
    -- Collapse a polynomial coefficient of a polynomial into an expression with the variable substituted.
    -- E.g., turn (t+2)x+1 into (3+2)x+1 for t=3.
    collapse a c' = getSum $ foldTerms (\e c -> Sum $ fromRational c * a ** fromIntegral e) c'

-- | Returns the roots for two variables in two polynomials.
--
-- Only supports rational roots.  If not all real roots are rational, then it will return 'Nothing'.
-- Returning all real roots would be preferable, but this is not supported at this time.
--
-- If the function cannot derive the roots otherwise, either, 'Nothing' will be returned as well.
solveBivariatePolynomials ::
  IndexedPolynomialWith IndexedPolynomial ->
  IndexedPolynomialWith IndexedPolynomial ->
  Maybe [(Rational, Rational)]
solveBivariatePolynomials p q = do
  let p' = toRationalFunctionCoefficients p
  let q' = toRationalFunctionCoefficients q
  resultant <- toPolynomial $ fst $ subresultant p' q'
  vs' <- solve resultant
  vs <- mapM (convert . simplify) vs'
  concat <$> mapM solveForU vs
  where
    toRationalFunctionCoefficients = mapCoefficients fromPolynomial

    -- For each v, returns list of (u,v) such that P(u,v)=Q(u,v)=0.
    solveForU :: Rational -> Maybe [(Rational, Rational)]
    solveForU v
      | 0 <- p' = do
          -- Any u will make p'=0 true, so we only need to solve p'.
          u <- map (convert . simplify) <$> solve q'
          map (,v) <$> sequence u
      | 0 <- q' = do
          -- Any u will make q'=0 true, so we only need to solve p'.
          u <- map (convert . simplify) <$> solve p'
          map (,v) <$> sequence u
      | otherwise = do
          up <- map (convert . simplify) <$> solve p'
          uq <- map (convert . simplify) <$> solve q'
          up' <- sequence up
          uq' <- sequence uq
          return $ map (,v) $ up' `intersect` uq'
      where
        p' = mapCoefficients (getSum . foldTerms (\e c -> Sum $ c * v ^ e)) p
        q' = mapCoefficients (getSum . foldTerms (\e c -> Sum $ c * v ^ e)) q

    -- Turn a simplified Expression into a rational number if possible.
    convert :: Expression -> Maybe Rational
    convert (Number n) = Just $ fromIntegral n
    convert (Number n :/: Number m) = Just $ fromIntegral n / fromIntegral m
    convert _ = Nothing
