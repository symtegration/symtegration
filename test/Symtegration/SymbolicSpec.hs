-- |
-- Description: Tests for Symtegration.SymbolicSpec
-- Maintainer: dev@chungyc.org
module Symtegration.SymbolicSpec where

import Data.Ratio (denominator, numerator)
import Data.String (fromString)
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "Expression from" $ modifyMaxSuccess (`div` 10) $ do
    describe "IsString" $ do
      prop "fromString" $
        \(PrintableString s) -> fromString s `shouldBe` Symbol (fromString s)

    describe "Num" $ do
      prop "+" $
        \(Value x) (Value y) -> x + y `shouldBe` BinaryApply Add x y

      prop "-" $
        \(Value x) (Value y) -> x - y `shouldBe` BinaryApply Subtract x y

      prop "*" $
        \(Value x) (Value y) -> x * y `shouldBe` BinaryApply Multiply x y

      prop "negate" $
        \(Value x) -> negate x `shouldBe` UnaryApply Negate x

      prop "abs" $
        \(Value x) -> abs x `shouldBe` UnaryApply Abs x

      prop "signum" $
        \(Value x) -> signum x `shouldBe` UnaryApply Signum x

      prop "fromInteger" $
        \n -> fromInteger n `shouldBe` Number n

    describe "Fractional" $ do
      prop "/" $
        \(Value x) (Value y) -> x / y `shouldBe` BinaryApply Divide x y

      prop "recip" $
        \(Value x) -> recip x `shouldBe` BinaryApply Divide 1 x

      prop "fromRational" $
        \x ->
          let n = fromInteger $ numerator x
              d = fromInteger $ denominator x
           in fromRational x `shouldBe` BinaryApply Divide n d

    describe "Floating" $ do
      prop "pi" $ pi `shouldBe` Symbol "pi"

      prop "exp" $
        \(Value x) -> exp x `shouldBe` UnaryApply Exp x

      prop "log" $
        \(Value x) -> log x `shouldBe` UnaryApply Log x

      prop "sqrt" $
        \(Value x) -> sqrt x `shouldBe` UnaryApply Sqrt x

      prop "**" $
        \(Value x) (Value y) -> x ** y `shouldBe` BinaryApply Power x y

      prop "logBase" $
        \(Value x) (Value y) -> logBase x y `shouldBe` BinaryApply LogBase x y

      prop "sin" $
        \(Value x) -> sin x `shouldBe` UnaryApply Sin x

      prop "cos" $
        \(Value x) -> cos x `shouldBe` UnaryApply Cos x

      prop "tan" $
        \(Value x) -> tan x `shouldBe` UnaryApply Tan x

      prop "asin" $
        \(Value x) -> asin x `shouldBe` UnaryApply Asin x

      prop "acos" $
        \(Value x) -> acos x `shouldBe` UnaryApply Acos x

      prop "atan" $
        \(Value x) -> atan x `shouldBe` UnaryApply Atan x

      prop "sinh" $
        \(Value x) -> sinh x `shouldBe` UnaryApply Sinh x

      prop "cosh" $
        \(Value x) -> cosh x `shouldBe` UnaryApply Cosh x

      prop "tanh" $
        \(Value x) -> tanh x `shouldBe` UnaryApply Tanh x

      prop "asinh" $
        \(Value x) -> asinh x `shouldBe` UnaryApply Asinh x

      prop "acosh" $
        \(Value x) -> acosh x `shouldBe` UnaryApply Acosh x

      prop "atanh" $
        \(Value x) -> atanh x `shouldBe` UnaryApply Atanh x
