-- |
-- Description: Provides a QuickCheck property checking that simplification does not change an expression's semantics.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.Properties (equivalentProperty) where

import Symtegration.FiniteDouble
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Test.Hspec
import Test.QuickCheck

equivalentProperty :: (Expression -> Expression) -> Complete -> Property
equivalentProperty simplify (Complete e m) =
  let e' = simplify e
      v = evaluate e (assign m)
      v' = evaluate e' (assign m)
   in counterexample ("e = " <> show (toHaskell e)) $
        counterexample ("simplify e = " <> show (toHaskell e')) $
          maybe False isFinite v && maybe False isFinite v' ==>
            fmap Near v' `shouldBe` fmap Near v
