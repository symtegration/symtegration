-- |
-- Description: Tests basic integration of exponential and logarithmic functions.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.ExponentialSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Exponential
import Symtegration.Integration.Properties
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  modifyMaxSuccess (* 10) $
    prop "consistent with derivative of integral" $ \(F e) x ->
      antiderivativeProperty integrate (Map.singleton var x) e x

  describe "ignores constants" $ do
    prop "with exponential" $
      forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
        integrate var (exp c) `shouldBe` Nothing

    prop "with logarithm" $
      forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
        integrate var (log c) `shouldBe` Nothing

    prop "with power of number" $ \n ->
      forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
        integrate var (Number n ** c) `shouldBe` Nothing

    prop "with logarithm with base" $ \n ->
      forAll (arbitrarySymbol `suchThat` (/= Symbol var)) $ \c ->
        integrate var (logBase (Number n) c) `shouldBe` Nothing

newtype F = F Expression deriving (Eq, Show)

instance Arbitrary F where
  arbitrary =
    F
      <$> oneof
        [ pure $ Exp' (Symbol var),
          pure $ Log' (Symbol var),
          (:**:) <$> fmap Number arbitrarySizedNatural <*> pure (Symbol var),
          LogBase' <$> fmap Number arbitrarySizedNatural <*> pure (Symbol var)
        ]

var :: Text
var = "x"
