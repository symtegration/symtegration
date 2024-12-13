-- |
-- Description: Tests basic integration of trigonometric functions.
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.TrigonometricSpec (spec) where

import Data.Map qualified as Map
import Test.Hspec
import Test.Hspec.QuickCheck
import Symtegration.Integration.Trigonometric
import Symtegration.Integration.Properties
import Test.QuickCheck
import Symtegration.Symbolic

spec :: Spec
spec = parallel $ do
  prop "consistent with derivative of integral" $ \(Trig e) x ->
    antiderivativeProperty integrate (Map.singleton "x" x) e x

newtype Trig = Trig Expression deriving (Eq, Show)

instance Arbitrary Trig where
  arbitrary = Trig <$> elements [Sin' (Symbol "x"), Cos' (Symbol "x")]
