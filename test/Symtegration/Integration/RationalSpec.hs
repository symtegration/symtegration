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
import Symtegration.Polynomial.Rational as Rational
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

  describe "hermiteReduce" $ do
    prop "h has squarefree denominator" $ \(NonZero p) (NonZero q) ->
      let r@(_, h) = hermiteReduce $ fromPolynomials p q
          Rational.Function _ d = h
       in counterexample (show r) $
            greatestCommonDivisor d (differentiate d) `shouldSatisfy` ((==) 0 . degree)

    prop "adds back to original rational function" $ \(NonZero p) (NonZero q) ->
      let f = fromPolynomials p q
          r@(gs, h) = hermiteReduce $ fromPolynomials p q

          -- Manually derive derivative of g = sum gs.
          Rational.Function x y = sum gs
          x' = y * differentiate x - x * differentiate y
          y' = y * y
          g' = fromPolynomials x' y'

          -- With leading coefficients factored out and numerator and denominator coprime,
          -- the representation of a rational function should be unique.
          rep (Rational.Function u v) =
            (leadingCoefficient u / leadingCoefficient v, fromPolynomials (monic u) (monic v))
       in counterexample (show r) $ rep (g' + h) `shouldBe` rep f

-- | For generating arbitrary rational functions with rational number coefficients.
newtype Rat = Rat Expression deriving (Eq, Show)

instance Arbitrary Rat where
  arbitrary = resize 6 $ do
    p <- arbitrary :: Gen IndexedPolynomial
    q <- arbitrary `suchThat` (/= 0) :: Gen IndexedPolynomial
    let p' = toExpression var toRationalCoefficient p
    let q' = toExpression var toRationalCoefficient q
    return $ Rat $ p' / q'

var :: Text
var = "x"
