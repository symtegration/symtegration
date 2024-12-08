-- |
-- Description: Tests for Symtegration.Symbolic.Simplify.RecursiveHeuristic
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.RecursiveHeuristicSpec (spec) where

import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify.RecursiveHeuristic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

{- ORMOLU_DISABLE -}
spec :: Spec
spec = modifyMaxSuccess (* 100) $ do
  describe "simplify" $ do
    prop "maintains semantics" $
      \(Complete e m) ->
        let e' = simplify e
            v = evaluate e m
            v' = evaluate e' m
         in counterexample ("e = " <> show (toHaskellText e)) $
            counterexample ("simplify e = " <> show (toHaskellText e')) $
            isFinite v && isFinite v' ==>
            fmap Near v' `shouldBe` fmap Near v
{- ORMOLU_ENABLE -}

-- | Wrapper type for comparing whether 'Double' return values are close enough.
-- Simplification can change the exact functions applied, so floating-point errors are expected.
newtype Near = Near Double deriving (Show)

instance Eq Near where
  (==) (Near x) (Near y)
    | isNaN x && isNaN y = True
    | isInfinite x || isInfinite y = x == y
    | x == 0 || y == 0 || x == (-0) || y == (-0) = x - y < 1e-5
    | otherwise = (x - y) / y < 1e-5

-- | Returns whether the given value is a finite value.
isFinite :: Maybe Double -> Bool
isFinite Nothing = False
isFinite (Just x)
  | isNaN x = False
  | isInfinite x = False
  | otherwise = True
