-- |
-- Description: Tests for Symtegration.Symbolic.Haskell
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.HaskellSpec (spec) where

import Data.String (fromString)
import Data.Text (toLower)
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TextShow (showt)

spec :: Spec
spec = parallel $ do
  describe "toHaskellText" $ do
    prop "converts for number" $ \n ->
      toHaskellText (Number n) `shouldBe` showt n

    prop "converts for symbol" $ \(PrintableString s) ->
      toHaskellText (Symbol $ fromString s) `shouldBe` fromString s

    describe "converts for unary function" $ do
      prop "with non-negative number" $ \func (NonNegative n) ->
        toHaskellText (UnaryApply func $ Number n)
          `shouldBe` getUnaryFunctionText func <> " " <> showt n

      prop "with negative number" $ \func (Negative n) ->
        toHaskellText (UnaryApply func $ Number n)
          `shouldBe` getUnaryFunctionText func <> " (" <> showt n <> ")"

      prop "with symbol" $ \func s ->
        toHaskellText (UnaryApply func $ Symbol $ fromString s)
          `shouldBe` getUnaryFunctionText func <> " " <> fromString s

      prop "with compound argument" $ \func (Compound e) ->
        toHaskellText (UnaryApply func e)
          `shouldBe` getUnaryFunctionText func <> " (" <> toHaskellText e <> ")"

    describe "converts for binary function" $ do
      prop "logBase with non-negative numbers" $ \(NonNegative m) (NonNegative n) ->
        toHaskellText (BinaryApply LogBase (Number m) (Number n))
          `shouldBe` "logBase " <> showt m <> " " <> showt n

      prop "logBase with negative numbers" $ \(Negative m) (Negative n) ->
        toHaskellText (BinaryApply LogBase (Number m) (Number n))
          `shouldBe` "logBase (" <> showt m <> ") (" <> showt n <> ")"

      prop "logBase with symbols" $ \s r ->
        toHaskellText (BinaryApply LogBase (Symbol $ fromString s) (Symbol $ fromString r))
          `shouldBe` "logBase " <> fromString s <> " " <> fromString r

      prop "logBase with compound arguments" $ \(Compound e1) (Compound e2) ->
        toHaskellText (BinaryApply LogBase e1 e2)
          `shouldBe` "logBase (" <> toHaskellText e1 <> ") (" <> toHaskellText e2 <> ")"

      prop "operators with non-negative numbers" $ \op (NonNegative m) (NonNegative n) ->
        op /= LogBase ==>
          toHaskellText (BinaryApply op (Number m) (Number n))
            `shouldBe` showt m <> " " <> getBinaryFunctionText op <> " " <> showt n

      prop "operators with negative numbers" $ \op (Negative m) (Negative n) ->
        op /= LogBase ==>
          toHaskellText (BinaryApply op (Number m) (Number n))
            `shouldBe` "(" <> showt m <> ") " <> getBinaryFunctionText op <> " (" <> showt n <> ")"

      prop "operators with symbols" $ \op s r ->
        op /= LogBase ==>
          toHaskellText (BinaryApply op (Symbol $ fromString s) (Symbol $ fromString r))
            `shouldBe` fromString s <> " " <> getBinaryFunctionText op <> " " <> fromString r

      prop "addition with compound arguments" $ \(Compound e1) (Compound e2) ->
        let text1 = toHaskellText e1
            text2 = toHaskellText e2
            par s = "(" <> s <> ")"
            t = toHaskellText $ e1 :+: e2
         in t `shouldBe` case (e1, e2) of
              (_ :+: _, _ :+: _) -> text1 <> " + " <> text2
              (_ :+: _, _) -> text1 <> " + " <> par text2
              (_, _ :+: _) -> par text1 <> " + " <> text2
              _ -> par text1 <> " + " <> par text2

      prop "multiplication with compound arguments" $ \(Compound e1) (Compound e2) ->
        let text1 = toHaskellText e1
            text2 = toHaskellText e2
            par s = "(" <> s <> ")"
            t = toHaskellText $ e1 :*: e2
         in t `shouldBe` case (e1, e2) of
              (_ :*: _, _ :*: _) -> text1 <> " * " <> text2
              (_ :*: _, _) -> text1 <> " * " <> par text2
              (_, _ :*: _) -> par text1 <> " * " <> text2
              _ -> par text1 <> " * " <> par text2

      prop "subtraction with compound arguments" $ \(Compound e1) (Compound e2) ->
        let text1 = toHaskellText e1
            text2 = toHaskellText e2
            par s = "(" <> s <> ")"
            t = toHaskellText $ e1 :-: e2
         in t `shouldBe` case (e1, e2) of
              (_ :+: _, _) -> text1 <> " - " <> par text2
              _ -> par text1 <> " - " <> par text2

      prop "operators with compound arguments" $ \(Compound e1) (Compound e2) ->
        forAll (elements [Divide, Power]) $ \op ->
          let text1 = toHaskellText e1
              text2 = toHaskellText e2
              optext = getBinaryFunctionText op
              par s = "(" <> s <> ")"
              t = toHaskellText (BinaryApply op e1 e2)
           in t `shouldBe` par text1 <> " " <> optext <> " " <> par text2

  -- The UnaryFunction constructors have the same spelling as their corresponding function name.
  describe "correct unary function text" $ do
    mapM_
      ( \func ->
          it ("for " <> show func) $
            getUnaryFunctionText func `shouldBe` toLower (showt func)
      )
      [minBound .. maxBound]

  describe "correct binary function text" $ do
    it "for Add" $ getBinaryFunctionText Add `shouldBe` "+"
    it "for Multiply" $ getBinaryFunctionText Multiply `shouldBe` "*"
    it "for Subtract" $ getBinaryFunctionText Subtract `shouldBe` "-"
    it "for Divide" $ getBinaryFunctionText Divide `shouldBe` "/"
    it "for Power" $ getBinaryFunctionText Power `shouldBe` "**"
    it "for LogBase" $ getBinaryFunctionText LogBase `shouldBe` "logBase"
