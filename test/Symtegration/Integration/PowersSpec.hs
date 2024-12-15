-- |
-- Description: Tests of Symtegration.Integration.Powers
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.PowersSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Powers
import Symtegration.Integration.Properties
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ modifyMaxSuccess (* 10) $ do
  prop "consistent with derivative of integral" $ \(Pow e) x ->
    antiderivativeProperty integrate (Map.singleton var x) e x

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
