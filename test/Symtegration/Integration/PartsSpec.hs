-- |
-- Description: Tests for Symtegration.Integration.Parts
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Integration.PartsSpec (spec) where

import Data.Map qualified as Map
import Data.Text (Text)
import Symtegration.Integration.Parts
import Symtegration.Integration.Powers qualified as Powers
import Symtegration.Integration.Properties
import Symtegration.Integration.Term qualified as Term
import Symtegration.Symbolic
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "integrates by parts" $ do
    prop "for powers" $
      forAll genParts $ \e x ->
        antiderivativeProperty
          (integrate [Powers.integrate, Term.integrate [Powers.integrate]])
          (Map.singleton var x)
          e
          x

-- | Generate an expression which can be integrated by parts.
genParts :: Gen Expression
genParts = do
  n <- arbitrarySizedNatural
  m <- arbitrarySizedNatural
  -- A product of two powers has some non-negligible chance to be integrated by parts.
  return $ x ** Number n * x ** Number m
  where
    x = Symbol var

var :: Text
var = "x"
