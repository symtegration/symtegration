-- |
-- Description: Provides general properties that can be used to testing various integration algorithms.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.Properties (antiderivativeProperty) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Numeric.AD
import Symtegration.Approximate
import Symtegration.Symbolic
import Symtegration.Symbolic.Haskell
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
      label "integration success" $
        counterexample ("derivative = " <> Text.unpack (toHaskell e)) $
          counterexample ("antiderivative = " <> Text.unpack (toHaskell integrated)) $
            let y = f (approximate x)
                y' = f' (approximate x)
             in isFinite y && isFinite y' ==> y `shouldBe` y'
      where
        -- The original function and the derivative of the integral should behave similarly.
        f = toFunction e (replace v)
        f' = diff (toFunction integrated (replaceForDiff v))

    -- Map all but the variable symbol to concrete numbers.
    replace var s
      | s == var = id
      | (Just z) <- Map.lookup s m = const (approximate z)
      | otherwise = const $ approximate 0
    replaceForDiff var s
      | s == var = id
      | (Just z) <- Map.lookup s m = const $ auto (approximate z)
      | otherwise = const $ auto (approximate 0)
