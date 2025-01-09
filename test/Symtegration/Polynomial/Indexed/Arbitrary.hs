{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: Generate arbitrary instances of 'IndexedPolynomial'.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.Indexed.Arbitrary where

import Symtegration.Polynomial
import Symtegration.Polynomial.Indexed
import Test.QuickCheck hiding (scale)

instance Arbitrary IndexedPolynomial where
  arbitrary = sized $ \case
    0 -> frequency [(10, pure (power 1)), (1, scale <$> resize 4 arbitrary <*> pure 1)]
    n ->
      frequency
        [ (1, resize 0 arbitrary),
          (10, resize (n `div` 2) $ (+) <$> arbitrary <*> arbitrary),
          (10, resize (n `div` 2) $ (*) <$> arbitrary <*> arbitrary)
        ]

  shrink p
    | 0 <- degree p = []
    | otherwise = [p - scale c (power k) | k <- [0 .. degree p], let c = coefficient p k, c /= 0]

instance
  (Polynomial p e c, Arbitrary (p e c), Eq (p e c), Num (p e c), Eq c) =>
  Arbitrary (IndexedPolynomialWith (p e c))
  where
  arbitrary = sized $ \case
    0 -> frequency [(10, pure (power 1)), (1, scale <$> resize 4 arbitrary <*> pure 1)]
    n ->
      frequency
        [ (1, resize 0 arbitrary),
          (10, resize (n `div` 2) $ (+) <$> arbitrary <*> arbitrary),
          (10, resize (n `div` 2) $ (*) <$> arbitrary <*> arbitrary)
        ]

  shrink p
    | 0 <- degree p = []
    | otherwise = [p - scale c (power k) | k <- [0 .. degree p], let c = coefficient p k, c /= 0]
