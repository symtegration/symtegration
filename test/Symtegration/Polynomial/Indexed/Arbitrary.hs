{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: Generate arbitrary instances of 'IndexedPolynomial'.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.Indexed.Arbitrary where

import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Test.QuickCheck (Arbitrary (..), arbitrarySizedNatural)

instance Arbitrary IndexedPolynomial where
  arbitrary = scale <$> arbitrary <*> (power <$> arbitrarySizedNatural)
  shrink p
    | 0 <- degree p = []
    | otherwise = [p - scale c (power k) | k <- [0 .. degree p], let c = coefficient p k, c /= 0]
