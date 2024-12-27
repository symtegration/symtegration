-- |
-- Description: Tests Symtegration.Symbolic.Simplify.NumericFolding
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Simplify.NumericFoldingSpec (spec) where

import Data.Text (unpack)
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify.NumericFolding
import Symtegration.Symbolic.Simplify.Properties
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "simplify" $ do
    modifyMaxSuccess (* 100) $
      prop "maintains semantics" $
        equivalentProperty simplify

    prop "folds to simple numeric expressions" $
      forAll genNumeric $ \e ->
        let e' = simplify e
         in counterexample ("e = " <> unpack (toHaskell e)) $
              counterexample ("simplify e = " <> unpack (toHaskell e')) $
                e' `shouldSatisfy` simpleNumeric

-- Numeric folding should be able to fold arithmetic on numbers
-- to either an integer or a fraction.
simpleNumeric :: Expression -> Bool
simpleNumeric (Number _) = True
simpleNumeric (Number _ :/: Number _) = True
simpleNumeric _ = False

-- | Generate arbitrary expression involving no symbols and which are
-- guaranteed to reduce exactly to a simple numeric term.
genNumeric :: Gen Expression
genNumeric = sized $ \case
  0 -> arbitraryNumber
  n ->
    frequency
      [ (1, arbitraryNumber),
        (1, resize (max 0 (n - 1)) $ UnaryApply Negate <$> genNumeric),
        ( 1,
          resize (max 0 (n - 1)) $
            BinaryApply Power <$> genNumeric <*> (Number <$> arbitrarySizedNatural `suchThat` (/= 0))
        ),
        ( 10,
          resize (n `div` 2) $
            BinaryApply <$> elements [Add, Multiply, Subtract, Divide] <*> genNumeric <*> genNumeric
        )
      ]
