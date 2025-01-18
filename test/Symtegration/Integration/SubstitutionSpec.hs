-- |
-- Description: Tests for Symtegration.Integration.Substitution
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.SubstitutionSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Properties
import Symtegration.Integration.Substitution
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "integrates by substitution" $ do
    prop "powers and trigonometric functions mixed" $
      forAll genExpression $ \e x ->
        antiderivativeProperty
          (integrate [Powers.integrate, Trigonometric.integrate])
          (Map.singleton var x)
          e
          x

-- | Generate an expression which combines polynomials and trigonometric functions.
genExpression :: Gen Expression
genExpression = sized $ \case
  0 -> oneof leaves
  n ->
    frequency $
      [(1, g) | g <- leaves]
        ++ [ (1, resize (max 0 (n - 1)) $ Negate' <$> genExpression),
             (10, resize (max 0 (n - 1)) $ Sin' <$> genExpression),
             (10, resize (max 0 (n - 1)) $ Cos' <$> genExpression),
             (10, resize (max 0 (n - 1)) $ Tan' <$> genExpression),
             (10, resize (n `div` 2) $ (:+:) <$> genExpression <*> genExpression),
             (10, resize (n `div` 2) $ (:-:) <$> genExpression <*> genExpression)
           ]
  where
    leaves =
      [ Number <$> arbitrary,
        pure $ Symbol var,
        (:+:) (Symbol var) . Number <$> choose (2, 6)
      ]

var :: Text
var = "x"
