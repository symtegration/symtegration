-- |
-- Description: Tests for Symtegration.Symbolic.Haskell
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.HaskellSpec (spec) where

import Data.String (fromString)
import Data.Text (Text, toLower)
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Symtegration.Symbolic.Haskell
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import TextShow (showt)

spec :: Spec
spec = parallel $ do
  describe "toHaskell" $ do
    prop "converts for number" $ \n ->
      toHaskell (Number n) `shouldBe` showt n

    prop "converts for symbol" $ \(PrintableString s) ->
      toHaskell (Symbol $ fromString s) `shouldBe` fromString s

    describe "converts for unary function" $ do
      prop "with non-negative number" $ \func (NonNegative n) ->
        toHaskell (UnaryApply func $ Number n)
          `shouldBe` getUnaryFunctionText func <> " " <> showt n

      prop "with negative number" $ \func (Negative n) ->
        toHaskell (UnaryApply func $ Number n)
          `shouldBe` getUnaryFunctionText func <> " " <> par (showt n)

      prop "with symbol" $ \func s ->
        toHaskell (UnaryApply func $ Symbol $ fromString s)
          `shouldBe` getUnaryFunctionText func <> " " <> fromString s

      prop "with compound argument" $ \func (Compound e) ->
        toHaskell (UnaryApply func e)
          `shouldBe` getUnaryFunctionText func <> " " <> par (toHaskell e)

    describe "converts for binary function" $ do
      prop "logBase with non-negative numbers" $ \(NonNegative m) (NonNegative n) ->
        toHaskell (BinaryApply LogBase (Number m) (Number n))
          `shouldBe` "logBase " <> showt m <> " " <> showt n

      prop "logBase with negative numbers" $ \(Negative m) (Negative n) ->
        toHaskell (BinaryApply LogBase (Number m) (Number n))
          `shouldBe` "logBase " <> par (showt m) <> " " <> par (showt n)

      prop "logBase with symbols" $ \s r ->
        toHaskell (BinaryApply LogBase (Symbol $ fromString s) (Symbol $ fromString r))
          `shouldBe` "logBase " <> fromString s <> " " <> fromString r

      prop "logBase with compound arguments" $ \(Compound e1) (Compound e2) ->
        toHaskell (BinaryApply LogBase e1 e2)
          `shouldBe` "logBase " <> par (toHaskell e1) <> " " <> par (toHaskell e2)

      prop "operators with non-negative numbers" $ \op (NonNegative m) (NonNegative n) ->
        op /= LogBase ==>
          toHaskell (BinaryApply op (Number m) (Number n))
            `shouldBe` showt m <> " " <> getBinaryFunctionText op <> " " <> showt n

      prop "operators with negative numbers" $ \op (Negative m) (Negative n) ->
        op /= LogBase ==>
          toHaskell (BinaryApply op (Number m) (Number n))
            `shouldBe` par (showt m) <> " " <> getBinaryFunctionText op <> " " <> par (showt n)

      prop "operators with symbols" $ \op s r ->
        op /= LogBase ==>
          toHaskell (BinaryApply op (Symbol $ fromString s) (Symbol $ fromString r))
            `shouldBe` fromString s <> " " <> getBinaryFunctionText op <> " " <> fromString r

      prop "addition with compound arguments" $ \(Compound e1) (Compound e2) ->
        let text1 = toHaskell e1
            text2 = toHaskell e2
            t = toHaskell $ e1 :+: e2
         in t `shouldBe` case (e1, e2) of
              (BinaryApply _ _ _, BinaryApply _ _ _) -> text1 <> " + " <> text2
              (BinaryApply _ _ _, UnaryApply _ _) -> text1 <> " + " <> text2
              (BinaryApply _ _ _, _) -> text1 <> " + " <> par text2
              (UnaryApply _ _, BinaryApply _ _ _) -> text1 <> " + " <> text2
              (_, BinaryApply _ _ _) -> text1 <> " + " <> text2
              (UnaryApply _ _, UnaryApply _ _) -> text1 <> " + " <> text2
              (UnaryApply _ _, _) -> text1 <> " + " <> par text2
              (_, UnaryApply _ _) -> par text1 <> " + " <> text2
              _ -> par text1 <> " + " <> par text2

      prop "multiplication with compound arguments" $ \(Compound e1) (Compound e2) ->
        let text1 = toHaskell e1
            text2 = toHaskell e2
            t = toHaskell $ e1 :*: e2
         in t `shouldBe` case (e1, e2) of
              (_ :*: _, _ :*: _) -> text1 <> " * " <> text2
              (_ :*: _, _) -> text1 <> " * " <> par text2
              (_, _ :*: _) -> par text1 <> " * " <> text2
              _ -> par text1 <> " * " <> par text2

      prop "subtraction with compound arguments" $ \(Compound e1) (Compound e2) ->
        let text1 = toHaskell e1
            text2 = toHaskell e2
            t = toHaskell $ e1 :-: e2
         in t `shouldBe` case (e1, e2) of
              (_ :+: _, _) -> text1 <> " - " <> par text2
              _ -> par text1 <> " - " <> par text2

      prop "operators with compound arguments" $ \(Compound e1) (Compound e2) ->
        forAll (elements [Divide, Power]) $ \op ->
          let text1 = toHaskell e1
              text2 = toHaskell e2
              optext = getBinaryFunctionText op
              t = toHaskell (BinaryApply op e1 e2)
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

-- | Surrounds the given text with parentheses.
par :: Text -> Text
par s = "(" <> s <> ")"
