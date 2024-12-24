-- |
-- Description: Tests for Symtegration.Integration.Factor
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.FactorSpec (spec) where

import Data.Text (Text)
import Symtegration.Integration.Factor
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "isConstant" $ do
    prop "for constant expression" $
      forAll genConstant $ \e ->
        isConstant var e `shouldBe` True

    prop "not for non-constant expression" $
      forAll genVariable $ \e ->
        isConstant var e `shouldBe` False

  describe "factor" $ do
    prop "into non-constant and constant factors" $
      forAll genVariable $ \e ->
        factor var e `shouldSatisfy` (\(x, y) -> isConstant var x && not (isConstant var y))

genConstant :: Gen Expression
genConstant = sized $ \case
  0 -> oneof [arbitraryNumber, arbitrarySymbol `suchThat` (/= Symbol var)]
  n ->
    frequency
      [ (1, arbitraryNumber),
        (1, arbitrarySymbol `suchThat` (/= Symbol var)),
        (10, resize (max 0 (n - 1)) $ UnaryApply <$> arbitrary <*> genConstant),
        (10, resize (n `div` 2) $ BinaryApply <$> arbitrary <*> genConstant <*> genConstant)
      ]

genVariable :: Gen Expression
genVariable = sized $ \case
  0 -> pure (Symbol var)
  n ->
    frequency
      [ (1, pure (Symbol var)),
        (10, resize (max 0 (n - 1)) $ UnaryApply <$> arbitrary <*> genVariable),
        (5, resize (n `div` 2) $ BinaryApply <$> arbitrary <*> genVariable <*> genConstant),
        (5, resize (n `div` 2) $ BinaryApply <$> arbitrary <*> genConstant <*> genVariable),
        (5, resize (n `div` 2) $ BinaryApply <$> arbitrary <*> genVariable <*> genVariable)
      ]

var :: Text
var = "x"
