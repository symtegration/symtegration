{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: Generate arbitrary instances of 'Rational.Function IndexedPolynomial'.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Polynomial.Rational.Arbitrary where

import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Symtegration.Polynomial.Rational
import Test.QuickCheck hiding (Function)

instance Arbitrary (Function IndexedPolynomial) where
  arbitrary = sized $ \n ->
    resize (n `div` 10) $
      fromPolynomials <$> arbitrary <*> arbitrary `suchThat` (/= 0)

  shrink (Function p q) = [fromPolynomials p' q' | (p', q') <- shrink (p, q)]
