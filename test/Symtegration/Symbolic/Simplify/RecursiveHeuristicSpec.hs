-- |
-- Description: Tests for Symtegration.Symbolic.Simplify.RecursiveHeuristic
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.RecursiveHeuristicSpec (spec) where

import Data.Map qualified as M
import Symtegration.FiniteDouble
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify.RecursiveHeuristic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = modifyMaxSuccess (* 100) $ do
  describe "simplify" $ do
    prop "maintains semantics" $
      \(Complete e m) ->
        let e' = simplify e
            v = evaluate e (M.map FiniteDouble m)
            v' = evaluate e' (M.map FiniteDouble m)
         in counterexample ("e = " <> show (toHaskellText e)) $
              counterexample ("simplify e = " <> show (toHaskellText e')) $
                maybe False isFinite v && maybe False isFinite v' ==>
                  fmap Near v' `shouldBe` fmap Near v

-- | Wrapper type for comparing whether 'Double' return values are close enough.
-- Simplification can change the exact functions applied, so floating-point errors are expected.
newtype Near = Near FiniteDouble deriving (Show)

instance Eq Near where
  (==) (Near (FiniteDouble x)) (Near (FiniteDouble y))
    | isNaN x && isNaN y = True
    | isInfinite x || isInfinite y = x == y
    | x == 0 || y == 0 || x == (-0) || y == (-0) = x - y < threshold
    | otherwise = (x - y) / y < threshold
    where
      threshold = 1e-5
