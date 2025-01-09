-- |
-- Description: Tests basic integration of trigonometric functions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.TrigonometricSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Properties
import Symtegration.Integration.Trigonometric
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  modifyMaxSuccess (* 10) $
    prop "consistent with derivative of integral" $ \(Trig e) x ->
      antiderivativeProperty integrate (Map.singleton var x) e x

  prop "ignores constant symbols" $ \(Trig e) ->
    forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
      integrate var (substitute e (\x -> if x == var then Just c else Nothing)) `shouldBe` Nothing

newtype Trig = Trig Expression deriving (Eq, Show)

instance Arbitrary Trig where
  arbitrary = Trig <$> elements [f (Symbol var) | f <- candidates]
    where
      candidates =
        [ Sin',
          Cos',
          Tan',
          Asin',
          Acos',
          Atan',
          Sinh',
          Cosh',
          Tanh',
          Asinh',
          Acosh',
          Atanh'
        ]

var :: Text
var = "x"
