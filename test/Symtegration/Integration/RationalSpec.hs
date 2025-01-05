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
import Symtegration.Polynomial hiding (integrate)
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Symtegration.Polynomial.Symbolic
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "integrate" $ modifyMaxSuccess (* 10) $ do
    prop "consistent with derivative of integral" $ \(Rat e) x ->
      antiderivativeProperty integrate (Map.singleton var x) e x

  describe "toRationalFunction" $ do
    prop "has coprime numerator and denominator" $ \(NonZero p) (NonZero q) ->
      let coprime (RationalFunction p' q') =
            degree (greatestCommonDivisor p' q') == 0
       in toRationalFunction p q `shouldSatisfy` coprime

  describe "hermiteReduce" $ do
    prop "h has squarefree denominator" $ \(NonZero p) (NonZero q) ->
      let r@(_, h) = hermiteReduce $ toRationalFunction p q
          RationalFunction _ d = h
       in counterexample (show r) $
            greatestCommonDivisor d (differentiate d) `shouldSatisfy` ((==) 0 . degree)

    prop "adds back to original rational function" $ \(NonZero p) (NonZero q) ->
      let f = toRationalFunction p q
          r@(gs, h) = hermiteReduce $ toRationalFunction p q

          -- Manually derive derivative of g = sum gs.
          RationalFunction x y = sum gs
          x' = y * differentiate x - x * differentiate y
          y' = y * y
          g' = toRationalFunction x' y'

          -- With leading coefficients factored out and numerator and denominator coprime,
          -- the representation of a rational function should be unique.
          rep (RationalFunction u v) =
            (leadingCoefficient u / leadingCoefficient v, RationalFunction (monic u) (monic v))
       in counterexample (show r) $ rep (g' + h) `shouldBe` rep f

-- | For generating arbitrary rational functions with rational number coefficients.
newtype Rat = Rat Expression deriving (Eq, Show)

instance Arbitrary Rat where
  arbitrary = resize 10 $ do
    p <- arbitrary :: Gen IndexedPolynomial
    q <- arbitrary `suchThat` (/= 0) :: Gen IndexedPolynomial
    let p' = toExpression var toRationalCoefficient p
    let q' = toExpression var toRationalCoefficient q
    return $ Rat $ p' / q'

var :: Text
var = "x"
