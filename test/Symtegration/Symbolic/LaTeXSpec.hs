-- |
-- Description: Tests for Symtegration.Symbolic.LaTeX.
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.LaTeXSpec (spec) where

import Symtegration.Symbolic.LaTeX
import Symtegration.Symbolic.Arbitrary ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ describe "toLaTeX" $ do
  -- There really should be more tests, but for now, just check that it is total.
  prop "is total" $ \e -> total (toLaTeX e)

