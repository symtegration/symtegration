-- |
-- Description: Tests for Symtegration.Symbolic.Simplify.RecursiveHeuristic
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.RecursiveHeuristicSpec (spec) where

import Symtegration.FiniteDouble
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify.RecursiveHeuristic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = modifyMaxSuccess (* 100) $ parallel $ do
  describe "simplify" $ do
    prop "maintains semantics" $ \(Complete e m) ->
      let e' = simplify e
          v = evaluate e (assign m)
          v' = evaluate e' (assign m)
       in counterexample ("e = " <> show (toHaskell e)) $
            counterexample ("simplify e = " <> show (toHaskell e')) $
              maybe False isFinite v ==>
                fmap Near v' `shouldBe` fmap Near v
