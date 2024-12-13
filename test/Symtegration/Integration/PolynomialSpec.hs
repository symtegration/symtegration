-- |
-- Description: Tests polynomial integration.
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.PolynomialSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Polynomial
import Symtegration.Integration.Properties
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ modifyMaxSuccess (* 10) $ do
  prop "consistent with derivative of integral" $ \(Poly e) x ->
    antiderivativeProperty rationalIntegrate (Map.singleton var x) e x

newtype Poly = Poly Expression deriving (Eq, Show)

instance Arbitrary Poly where
  arbitrary = Poly <$> genPolynomial
  shrink (Poly x) = Poly <$> shrinkPolynomial x

genPolynomial :: Gen Expression
genPolynomial = sized $ \n -> case n of
  0 -> oneof [Number <$> arbitrary, pure $ Symbol var]
  _ ->
    frequency
      [ (1, Number <$> arbitrary),
        (1, pure $ Symbol var),
        (5, resize (max 0 (n - 1)) $ Negate' <$> genPolynomial),
        -- Keep exponents reasonably small.
        (10, resize (max 0 (n - 1)) $ (:**:) <$> genPolynomial <*> (Number <$> choose (0, 4))),
        (50, resize (n `div` 2) $ (:+:) <$> genPolynomial <*> genPolynomial),
        (50, resize (n `div` 2) $ (:*:) <$> genPolynomial <*> genPolynomial),
        (50, resize (n `div` 2) $ (:-:) <$> genPolynomial <*> genPolynomial)
      ]

shrinkPolynomial :: Expression -> [Expression]
shrinkPolynomial (Number _) = []
shrinkPolynomial (Symbol _) = []
shrinkPolynomial (UnaryApply _ x) = [x]
shrinkPolynomial (BinaryApply _ x y) = [x, y]

var :: Text
var = "x"
