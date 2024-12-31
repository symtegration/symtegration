-- |
-- Module: Symtegration.Polynomial.Indexed
-- Description: A polynomial representation mapping the power of each term to its coefficient.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.Indexed
  ( IndexedPolynomial,
    IndexedSymbolicPolynomial,
    IndexedPolynomialWith,
  )
where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid (Dual (..))
import Data.Ratio (denominator, numerator)
import Data.Text (unpack)
import Symtegration.Polynomial
import Symtegration.Symbolic
import TextShow

-- | Polynomial representation which maps the power of each term to its coefficient.
-- Exponents are represented with 'Int', while coefficients are represented with 'Rational'.
-- It is an instance of the 'Polynomial' type class.
type IndexedPolynomial = IndexedPolynomialWith Rational

-- | Polynomial representation which maps the power of each term to its coefficient.
-- Exponents are represented with 'Int', while coefficients are represented with 'Expression'.
-- It is an instance of the 'Polynomial' type class.
type IndexedSymbolicPolynomial = IndexedPolynomialWith Expression

-- | Polynomial representation which maps the power of each term to its coefficient.
-- Exponents are represented with 'Int'.  Coefficients have a type as specified by the type parameter.
-- These types are an instance of the 'Polynomial' type class.
type IndexedPolynomialWith a = P Int a

-- | Type with two type parameters so that it can be an instance of 'Polynomial'.
-- The first type parameter is not involved in the data constructor;
-- it is used to set the exponent type for 'Polynomial'.
newtype P a b = P (IntMap b) deriving (Eq)

instance Show (P Int Rational) where
  show = unpack . showt

instance TextShow (P Int Rational) where
  showb (P m)
    | IntMap.null m = "0"
    | otherwise =
        mconcat $
          intersperse " + " $
            map showTerm $
              IntMap.toDescList m
    where
      showTerm (0, 1) = "1"
      showTerm (0, c) = showCoefficient c
      showTerm (1, c) = showCoefficient c <> "x"
      showTerm (e, 1) = "x^" <> showb e
      showTerm (e, c) = showCoefficient c <> "x^" <> showb e
      showCoefficient r
        | 1 <- r = mempty
        | 1 <- denominator r, r > 0 = showb $ numerator r
        | 1 <- denominator r, r < 0 = showbParen True $ showb $ numerator r
        | otherwise = showbParen True $ showb r

instance (Eq a, Num a) => Num (P Int a) where
  (P p) + (P q) = P $ filterNonzero $ IntMap.unionWith (+) p q

  (P p) * (P q) = P $ filterNonzero $ IntMap.foldlWithKey' accumulate IntMap.empty p
    where
      accumulate m e c = IntMap.unionWith (+) m $ multiplyTerm e c
      multiplyTerm e c = IntMap.mapKeysMonotonic (+ e) $ IntMap.map (* c) q

  abs = id
  signum _ = 1
  fromInteger 0 = P IntMap.empty
  fromInteger n = P $ IntMap.singleton 0 $ fromInteger n
  negate (P m) = P $ IntMap.map negate m

-- | Get rid of zero coefficients to ensure that zero coefficients do not cause
-- two polynomials represented by an 'IntMap' are not considered different.
filterNonzero :: (Eq a, Num a) => IntMap a -> IntMap a
filterNonzero = IntMap.filter (/= 0)

instance (Eq a, Num a) => Polynomial P Int a where
  degree (P m) = maybe 0 fst $ IntMap.lookupMax m
  coefficient (P m) k = fromMaybe 0 $ IntMap.lookup k m
  leadingCoefficient (P m) = maybe 0 snd $ IntMap.lookupMax m
  deleteLeadingTerm (P m) = P $ IntMap.deleteMax m
  foldTerms f (P m) = getDual $ IntMap.foldMapWithKey (\k v -> Dual $ f k v) m
  scale 0 _ = P IntMap.empty
  scale x (P m) = P $ IntMap.map (* x) m
  power n = P $ IntMap.singleton (fromIntegral n) 1
