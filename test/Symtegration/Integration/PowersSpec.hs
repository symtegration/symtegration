-- |
-- Description: Tests of Symtegration.Integration.Powers
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.PowersSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Powers
import Symtegration.Integration.Properties
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  modifyMaxSuccess (* 10) $ do
    prop "consistent with derivative of integral" $ \(Pow e) x ->
      antiderivativeProperty integrate (Map.singleton var x) e x

  prop "integrates constant symbol" $
    forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
      integrate var c `shouldSatisfy` flip elem (map Just [Symbol var * c, c * Symbol var])

  describe "ignores constants" $ do
    prop "with integer power" $ \n ->
      forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
        integrate var (c :*: Number n) `shouldBe` Nothing

    prop "with fraction" $ \n m ->
      forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
        integrate var (c :*: (Number n :/: Number m)) `shouldBe` Nothing

newtype Pow = Pow Expression deriving (Eq, Show)

instance Arbitrary Pow where
  arbitrary =
    Pow
      <$> frequency
        [ (1, pure $ Symbol var :**: Number (-1)),
          (50, (\n -> Symbol var :**: Number n) <$> arbitrary),
          (50, (\m n -> Symbol var :**: (Number m :/: Number n)) <$> arbitrary <*> arbitrary)
        ]

var :: Text
var = "x"
