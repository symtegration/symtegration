-- |
-- Description: Provides general properties that can be used to testing various integration algorithms.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Properties (antiderivativeProperty) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.AD
import Symtegration.FiniteDouble
import Symtegration.Symbolic
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify.RecursiveHeuristic
import Test.Hspec
import Test.QuickCheck

-- | Tests the property that a function should be consistent
-- with the derivative of its integral.
antiderivativeProperty ::
  (Text -> Expression -> Maybe Expression) ->
  Map Text Double ->
  Expression ->
  Double ->
  Property
antiderivativeProperty integrate m e x =
  not (Map.null m) ==> forAll (elements $ Map.keys m) $ \v -> check (integrate v e) v
  where
    check Nothing _ = label "integration fail" True
    check (Just integrated) v =
      isFinite (FiniteDouble $ f x) && isFinite (FiniteDouble $ f' x) ==>
        label "integration success" $
          counterexample ("derivative = " <> Text.unpack (toHaskell e)) $
            counterexample ("antiderivative = " <> Text.unpack (toHaskell integrated)) $
              counterexample ("simplified derivative = " <> Text.unpack (toHaskell $ simplify e)) $
                counterexample ("simplified antiderivative = " <> Text.unpack (toHaskell $ simplify integrated)) $
                  Near (FiniteDouble (f' x)) `shouldBe` Near (FiniteDouble (f x))
      where
        -- The original function and the derivative of the integral should behave similarly.
        --
        -- These are (Double -> Double).  It seems Numeric.AD does not like FiniteDouble.
        f = toFunction e (replace v)
        f' = diff (toFunction integrated (replaceForDiff v))

    -- Map all but the variable symbol to concrete numbers.
    replace var s
      | s == var = id
      | (Just z) <- Map.lookup s m = const z
      | otherwise = error "unknown symbol"
    replaceForDiff var s
      | s == var = id
      | (Just z) <- Map.lookup s m = const $ auto z
      | otherwise = error "unknown symbol"
