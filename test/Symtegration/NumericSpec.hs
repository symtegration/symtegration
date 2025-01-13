-- |
-- Description: Tests for Symtegration.Numeric.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.NumericSpec (spec) where

import Symtegration.Numeric
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "root" $ do
    prop "finds root of zero" $ \(Positive e) -> root 0 e `shouldBe` Just 0

    prop "finds root of one" $ \(Positive e) -> root 1 e `shouldBe` Just 1

    prop "finds positive root" $ \(Positive x) (Positive e) ->
      root (x ^ e) e `shouldBe` Just x

    prop "finds negative root for odd power" $ \(Negative x) (Positive e) ->
      odd e ==>
        root (x ^ e) e `shouldBe` Just x

    prop "finds nothing for negative number with even power" $ \(Negative x) (Positive e) ->
      even e ==>
        root x e `shouldBe` Nothing

    prop "finds nothing when there is no integer root" $ \(Positive x) (Positive e) ->
      even e ==>
        root (x ^ e + 1) e `shouldBe` Nothing
