module Symtegration.Polynomial.DifferentialSpec (spec) where

import Symtegration.Polynomial
import Symtegration.Polynomial.Differential
import Symtegration.Polynomial.Indexed
import Symtegration.Polynomial.Indexed.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  splitFactorSpec
  splitSquarefreeFactorSpec
  canonicalSpec
  consistentSpec
  extendSpec

splitFactorSpec :: Spec
splitFactorSpec = modifyMaxSize (`div` 4) $ describe "splitFactor" $ do
  prop "are factors" $ \p (Deriv derivation _) ->
    forFactors (splitFactor derivation p) $ \(ps, pn) ->
      ps * pn `shouldBe` p

  prop "squarefree factors of non-special factor are normal" $ \p (Deriv derivation _) ->
    forFactors (splitFactor derivation p) $ \(pn, _) ->
      squarefree pn `shouldSatisfy` all (isNormal derivation)

  prop "special factor is not coprime with derivation" $ \p (Deriv derivation _) ->
    forFactors (splitFactor derivation p) $ \(_, ps) ->
      ps `shouldSatisfy` isSpecial derivation
  where
    forFactors (pn, ps) f =
      counterexample ("normal = " <> show pn) $
        counterexample ("special = " <> show ps) $
          f (pn, ps)

splitSquarefreeFactorSpec :: Spec
splitSquarefreeFactorSpec = modifyMaxSize (`div` 4) $ describe "splitSquarefreeFactor" $ do
  prop "is equivalent to splitFactor" $ \p (Deriv derivation _) ->
    let factors = splitSquarefreeFactor derivation p
     in counterexample (show factors) $
          toFactors factors `shouldBe` splitFactor derivation p

  prop "has squarefree factors" $ \p (Deriv derivation _) ->
    splitSquarefreeFactor derivation p
      `shouldSatisfy` all (\(x, y) -> isSquarefree x && isSquarefree y)
  where
    toFactors fs = compose (1, 1) fs (1 :: Int)
    compose factors [] _ = factors
    compose (pn, ps) ((qn, qs) : fs) n = compose (pn * qn ^ n, ps * qs ^ n) fs (n + 1)

    isSquarefree p = degree (greatestCommonDivisor p p') == 0
      where
        p' = differentiate p

canonicalSpec :: Spec
canonicalSpec = modifyMaxSize (`div` 4) $ describe "canonical" $ do
  prop "adds back to original function" $ \(Rat a d) (Deriv derivation _) ->
    let rep@(p, (b, dn), (c, ds)) = canonical derivation a d
     in counterexample (show rep) $
          (p * dn * ds + b * ds + c * dn, dn * ds) `shouldBe` (a, d)

  prop "numerator has smaller degree in normal part" $ \(Rat a d) (Deriv derivation _) ->
    let rep@(_, (b, dn), _) = canonical derivation a d
     in counterexample (show rep) $
          disjoin
            [ degree b == 0 && degree dn == 0,
              degree b < degree dn
            ]

  prop "numerator has smaller degree in special part" $ \(Rat a d) (Deriv derivation _) ->
    let rep@(_, _, (c, ds)) = canonical derivation a d
     in counterexample (show rep) $
          disjoin
            [ degree c == 0 && degree ds == 0,
              degree c < degree ds
            ]

  prop "denominator for normal part has squarefree factors which are normal" $
    \(Rat a d) (Deriv derivation _) ->
      let rep@(_, (_, dn), _) = canonical derivation a d
       in counterexample (show rep) $
            squarefree dn `shouldSatisfy` all (isNormal derivation)

  prop "denominator for special part is special" $ \(Rat a d) (Deriv derivation _) ->
    let rep@(_, _, (_, ds)) = canonical derivation a d
     in counterexample (show rep) $
          ds `shouldSatisfy` isSpecial derivation

extendSpec :: Spec
extendSpec = modifyMaxSize (`div` 4) $ describe "extend" $ do
  prop "extension is derivation" $ \w a b ->
    consistent (extend differentiate w) a (b :: IndexedPolynomialWith IndexedPolynomial)
      `shouldBe` True

consistentSpec :: Spec
consistentSpec = describe "consistent" $ do
  prop "returns true for differentiation" $ \p q ->
    consistent differentiate p (q :: IndexedPolynomial) `shouldBe` True

  -- D(a+b) /= D(a) + D(b)
  it "returns false for abs" $
    consistent abs (-1 :: Integer) 1 `shouldBe` False

  -- D(ab) /= aD(b) + bD(a)
  it "returns false for multiplication by two" $
    consistent (* 2) 4 (5 :: Integer) `shouldBe` False

-- | Return whether a polynomial is normal in respect to the given derivation.
isNormal :: (IndexedPolynomial -> IndexedPolynomial) -> IndexedPolynomial -> Bool
isNormal derivation p = degree (greatestCommonDivisor p (derivation p)) == 0

-- | Return whether a polynomial is special in respect to the given derivation.
-- Holds even when the polynomial is zero.
isSpecial :: (IndexedPolynomial -> IndexedPolynomial) -> IndexedPolynomial -> Bool
isSpecial derivation p = monic (greatestCommonDivisor p (derivation p)) == monic p

-- | For generating coprime numerators and denominators for non-zero rational functions.
data Rat = Rat IndexedPolynomial IndexedPolynomial deriving (Show)

instance Arbitrary Rat where
  arbitrary = do
    d <- monic <$> arbitrary `suchThat` (/= 0)
    a <- arbitrary `suchThat` ((== 0) . degree . greatestCommonDivisor d)
    return $ Rat a d

-- | Generates an arbitrary derivation.
-- The string is for showing a description of the derivation.
data Deriv = Deriv (IndexedPolynomial -> IndexedPolynomial) String

instance Show Deriv where
  show (Deriv _ s) = show s

instance Arbitrary Deriv where
  arbitrary =
    frequency
      [ (1, pure $ Deriv differentiate "differentiation"),
        (5, extension)
      ]
    where
      extension = do
        w <- arbitrary
        let deriv = extend (const 0) w
        let desc = "extended zero derivation from w = " <> show w
        return $ Deriv deriv desc
