module Symtegration.Symbolic.Simplify.NumericFoldingSpec (spec) where

import Symtegration.Symbolic.Simplify.NumericFolding
import Test.Hspec
import Test.Hspec.QuickCheck
import Symtegration.Symbolic.Simplify.Properties

spec :: Spec
spec = parallel $ do
  describe "simplify" $ do
    modifyMaxSuccess (* 100) $ prop "maintains semantics" $
      equivalentProperty simplify
