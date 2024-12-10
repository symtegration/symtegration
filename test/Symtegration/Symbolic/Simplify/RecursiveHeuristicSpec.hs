-- |
-- Description: Tests for Symtegration.Symbolic.Simplify.RecursiveHeuristic
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.RecursiveHeuristicSpec (spec) where

import Data.Map qualified as Map
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
    prop "maintains semantics" $
      \(Complete e m) ->
        let e' = simplify e
            v = evaluate e (Map.map FiniteDouble m)
            v' = evaluate e' (Map.map FiniteDouble m)
         in counterexample ("e = " <> show (toHaskellText e)) $
              counterexample ("simplify e = " <> show (toHaskellText e')) $
                maybe False isFinite v ==>
                  fmap Near v' `shouldBe` fmap Near v
