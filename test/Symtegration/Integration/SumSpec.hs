-- |
-- Description: Tests for Symtegration.Integration.Sum
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.SumSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Polynomial qualified as Polynomial
import Symtegration.Integration.Properties
import Symtegration.Integration.Sum
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "integrates and adds by term" $ do
    prop "powers and trigonometric functions mixed" $
      forAll genExpression $ \e x ->
        antiderivativeProperty
          (integrate [Polynomial.rationalIntegrate, Trigonometric.integrate])
          (Map.singleton var x)
          e
          x

-- | Generate an expression which adds polynomials and trigonometric functions together.
genExpression :: Gen Expression
genExpression = sized $ \case
  0 -> oneof leaves
  n ->
    frequency $
      [(1, g) | g <- leaves]
        ++ [ (1, resize (max 0 (n - 1)) $ Negate' <$> genExpression),
             (10, resize (n `div` 2) $ (:+:) <$> genExpression <*> genExpression),
             (10, resize (n `div` 2) $ (:-:) <$> genExpression <*> genExpression)
           ]
  where
    leaves =
      [ Number <$> arbitrary,
        pure $ Symbol var,
        pure $ Sin' $ Symbol var,
        pure $ Cos' $ Symbol var,
        pure $ Tan' $ Symbol var,
        (:+:) (Symbol var) . Number <$> choose (2, 6)
      ]

var :: Text
var = "x"
