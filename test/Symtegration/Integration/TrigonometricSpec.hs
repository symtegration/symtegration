-- |
-- Description: Tests basic integration of trigonometric functions.
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.TrigonometricSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Properties
import Symtegration.Integration.Trigonometric
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ modifyMaxSuccess (* 10) $ do
  prop "consistent with derivative of integral" $ \(Trig e) x ->
    antiderivativeProperty integrate (Map.singleton var x) e x

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
