{-# OPTIONS_GHC -fno-warn-orphans #-}

module Symtegration.Polynomial.Indexed.Arbitrary where

import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Test.QuickCheck (Arbitrary(..), arbitrarySizedNatural)

instance Arbitrary IndexedPolynomial where
  arbitrary = scale <$> arbitrary <*> (power <$> arbitrarySizedNatural)
  shrink p
    | 0 <- degree p = []
    | otherwise = [p - scale c (power k) | k <- [0 .. degree p], let c = coefficient p k, c /= 0]
