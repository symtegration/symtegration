-- |
-- Description: Provides a QuickCheck property checking that simplification does not change an expression's semantics.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.Properties (equivalentProperty) where

import Symtegration.Approximate
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Test.Hspec
import Test.QuickCheck

equivalentProperty :: (Expression -> Expression) -> Complete -> Property
equivalentProperty simplify (Complete e m) =
  let e' = simplify e
      v = evaluate e (fmap approximate . assign m)
      v' = evaluate e' (fmap approximate . assign m)
   in maybe False isFinite v && maybe False isFinite v' ==>
        counterexample ("e = " <> show (toHaskell e)) $
          counterexample ("simplify e = " <> show (toHaskell e')) $
            v `shouldBe` v'
