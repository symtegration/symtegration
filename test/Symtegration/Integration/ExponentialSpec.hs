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
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ modifyMaxSuccess (* 10) $ do
  prop "consistent with derivative of integral" $ \(F e) x ->
    antiderivativeProperty integrate (Map.singleton var x) e x

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
