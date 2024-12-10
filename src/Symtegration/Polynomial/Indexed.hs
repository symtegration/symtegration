-- |
-- Module: Symtegration.Polynomial.Indexed
-- Description: A polynomial representation mapping the power of each term to its coefficient.
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.Indexed (IndexedPolynomial) where

import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Symtegration.Polynomial
import TextShow

-- | Polynomial representation which maps the power of each term to its coefficient.
-- Exponents are represented with 'Int', while coefficients are represented with 'Rational'.
-- It is an instance of the 'Polynomial' typeclass.
type IndexedPolynomial = P Int Rational

-- | Type with two type parameters so that it can be an instance of 'Polynomial'.
newtype P a b = P (IntMap b) deriving (Eq)

instance Show (P Int Rational) where
  show (P m)
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
      showTerm (e, 1) = "x^" <> show e
      showTerm (e, c) = showCoefficient c <> "x^" <> show e
      showCoefficient r
        | 1 <- r = mempty
        | 1 <- denominator r, r > 0 = show $ numerator r
        | 1 <- denominator r, r < 0 = "(" <> show (numerator r) <> ")"
        | otherwise = "(" <> show r <> ")"

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
  scale 0 _ = P IntMap.empty
  scale x (P m) = P $ IntMap.map (* x) m
  power n = P $ IntMap.singleton (fromIntegral n) 1
