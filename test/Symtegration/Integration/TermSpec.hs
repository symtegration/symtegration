-- |
-- Description: Tests for Symtegration.Integration.Sum
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.TermSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text, unpack)
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Properties
import Symtegration.Integration.Term
import Symtegration.Integration.Trigonometric qualified as Trigonometric
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Symtegration.Symbolic.Simplify
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "integrates term" $ do
    prop "for constant multiplied by simple term" $ \x ->
      forAll genConstant $ \c ->
        forAll genVariableTerm $ \e ->
          let e' = simplify var $ c :*: e
              fs = [Powers.integrate, Trigonometric.integrate]
           in counterexample ("e' = " <> unpack (toHaskell e')) $
                antiderivativeProperty (integrate fs) (Map.singleton var x) e' x

-- | Expression with no variable.
genConstant :: Gen Expression
genConstant = sized $ \case
  0 -> Number <$> arbitrarySizedNatural
  n ->
    frequency
      [ (1, Number <$> arbitrarySizedNatural),
        (10, resize (max 0 (n - 1)) $ Exp' <$> genConstant),
        (10, resize (max 0 (n - 1)) $ Negate' <$> genConstant),
        (10, resize (n `div` 2) $ (:+:) <$> genConstant <*> genConstant),
        (10, resize (n `div` 2) $ (:*:) <$> genConstant <*> genConstant)
      ]

-- | Variable terms that the basic integration algorithms can integrate.
genVariableTerm :: Gen Expression
genVariableTerm =
  oneof
    [ pure $ Number 1,
      pure $ Symbol var,
      (:**:) (Symbol var) <$> arbitraryNumber,
      pure $ Sin' (Symbol var),
      pure $ Cos' (Symbol var),
      pure $ Tan' (Symbol var)
    ]

var :: Text
var = "x"
