-- |
-- Description: Tests for Symtegration.Symbolic.LaTeX.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.LaTeXSpec (spec) where

import Symtegration.Symbolic.Arbitrary ()
import Symtegration.Symbolic.LaTeX
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ describe "toLaTeX" $ do
  -- Full-fledged property-based tests would be nice, but for now,
  -- check only for totality property and test with specific examples instead.
  prop "is total" $ \e -> total (toLaTeX e)

  it "-a" $ toLaTeX (-"a") `shouldBe` "-a"

  it "-(1 + a)" $ toLaTeX (-(1 + "a")) `shouldBe` "-\\left(1 + a\\right)"

  it "1 + 5" $ toLaTeX (1 + 5) `shouldBe` "1 + 5"

  it "1 + 2 + 3" $ toLaTeX (1 + 2 + 3) `shouldBe` "1 + 2 + 3"

  it "x * y + u * v" $ toLaTeX ("x" * "y" + "u" * "v") `shouldBe` "x y + u v"

  it "x + u * v" $ toLaTeX ("x" + "u" * "v") `shouldBe` "x + u v"

  it "(-1) + (-4)" $ toLaTeX ((-1) + (-4)) `shouldBe` "\\left(-1\\right) + \\left(-4\\right)"

  it "sin x + cos y" $ toLaTeX (sin "x" + cos "y") `shouldBe` "\\sin x + \\cos y"

  it "2 * 5" $ toLaTeX (2 * 5) `shouldBe` "2 \\times 5"

  it "2 * 3 * 4 * 6" $ toLaTeX (2 * 3 * 4 * 6) `shouldBe` "2 \\times 3 \\times 4 \\times 6"

  it "sin x * 3" $ toLaTeX (sin "x" * 3) `shouldBe` "\\sin x \\times 3"

  it "(-2) * (-5)" $ toLaTeX ((-2) * (-5)) `shouldBe` "\\left(-2\\right) \\left(-5\\right)"

  it "sin x * cos y" $ toLaTeX (sin "x" * cos "y") `shouldBe` "\\sin x \\cos y"

  it "4 * sin x" $ toLaTeX (4 * sin "x") `shouldBe` "4 \\sin x"

  it "x * y" $ toLaTeX ("x" * "y") `shouldBe` "x y"

  it "x * y ** z" $ toLaTeX ("x" * "y" ** "z") `shouldBe` "x y^{z}"
