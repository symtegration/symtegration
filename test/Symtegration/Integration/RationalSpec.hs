-- |
-- Description: Tests of Symtegration.Integration.Rational.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.RationalSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Properties
import Symtegration.Integration.Rational
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ modifyMaxSuccess (* 10) $ do
  prop "consistent with derivative of integral" $ \(Rat e) x ->
    antiderivativeProperty integrate (Map.singleton var x) e x

newtype Rat = Rat Expression deriving (Eq, Show)

instance Arbitrary Rat where
  arbitrary = do
    p <- arbitrary :: Gen IndexedPolynomial
    q <- arbitrary :: Gen IndexedPolynomial
    let p' = toExpression var toRationalCoefficient p
    let q' = toExpression var toRationalCoefficient q
    return $ Rat $ p' / q'

var :: Text
var = "x"
