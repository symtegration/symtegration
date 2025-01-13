-- |
-- Description: Tests Symtegration.Symbolic.Simplify.Tidy.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.TidySpec (spec) where

import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Simplify.Properties
import Symtegration.Symbolic.Simplify.Tidy
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "tidy" $ do
    modifyMaxSuccess (* 100) $
      prop "maintains semantics" $
        equivalentProperty tidy

    prop "x + negate y" $
      forAll arbitrarySymbol $ \x ->
        forAll arbitrarySymbol $ \y ->
          tidy (x + negate y) `shouldBe` x - y

    prop "x + (-1) * y" $
      forAll arbitrarySymbol $ \x ->
        forAll arbitrarySymbol $ \y ->
          tidy (x + Number (-1) * y) `shouldBe` x - y

    prop "x + (-n) * y" $ \(Positive n) ->
      n > 1 ==>
        forAll arbitrarySymbol $ \x ->
          forAll arbitrarySymbol $ \y ->
            tidy (x + Number (-n) * y) `shouldBe` x - Number n * y

    prop "(-n) / m" $ \(Positive n) (Positive m) ->
      tidy (Number (-n) / Number m) `shouldBe` negate (Number n / Number m)

    prop "(-n) / x" $ \(Positive n) ->
      forAll arbitrarySymbol $ \x ->
        tidy (Number (-n) / x) `shouldBe` negate (Number n / x)

    prop "(-x) * y" $
      forAll arbitrarySymbol $ \x ->
        forAll arbitrarySymbol $ \y ->
          tidy ((-x) * y) `shouldBe` negate (x * y)

    prop "x * (-y)" $
      forAll arbitrarySymbol $ \x ->
        forAll arbitrarySymbol $ \y ->
          tidy (x * (-y)) `shouldBe` negate (x * y)

    prop "(-x) * (-y)" $
      forAll arbitrarySymbol $ \x ->
        forAll arbitrarySymbol $ \y ->
          tidy ((-x) * (-y)) `shouldBe` x * y

    prop "x + ((-y) + z)" $
      forAll arbitrarySymbol $ \x ->
        forAll arbitrarySymbol $ \y ->
          forAll arbitrarySymbol $ \z ->
            tidy (x + ((-y) + z)) `shouldBe` x - y + z
