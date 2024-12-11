-- |
-- Description: Tests for Symtegration.SymbolicSpec
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.SymbolicSpec (spec) where

import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Ratio (denominator, numerator)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Symtegration.FiniteDouble
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | Same as 'evaluate', except specialized to 'FiniteDouble'.
evaluate' :: Expression -> (Text -> Maybe FiniteDouble) -> Maybe FiniteDouble
evaluate' = evaluate

spec :: Spec
spec = parallel $ do
  describe "Expression from" $ modifyMaxSuccess (`div` 10) $ do
    describe "IsString" $ do
      prop "fromString" $ \(SymbolText s) ->
        fromString (Text.unpack s) `shouldBe` Symbol s

    describe "Num" $ do
      prop "+" $ \(Simple x) (Simple y) ->
        x + y `shouldBe` BinaryApply Add x y

      prop "-" $ \(Simple x) (Simple y) ->
        x - y `shouldBe` BinaryApply Subtract x y

      prop "*" $ \(Simple x) (Simple y) ->
        x * y `shouldBe` BinaryApply Multiply x y

      prop "negate" $ \(Simple x) ->
        negate x `shouldBe` UnaryApply Negate x

      prop "abs" $ \(Simple x) ->
        abs x `shouldBe` UnaryApply Abs x

      prop "signum" $ \(Simple x) ->
        signum x `shouldBe` UnaryApply Signum x

      prop "fromInteger" $ \n ->
        fromInteger n `shouldBe` Number n

    describe "Fractional" $ do
      prop "/" $ \(Simple x) (Simple y) ->
        x / y `shouldBe` BinaryApply Divide x y

      prop "recip" $ \(Simple x) ->
        recip x `shouldBe` BinaryApply Divide 1 x

      prop "fromRational" $ \x ->
        let n = fromInteger $ numerator x
            d = fromInteger $ denominator x
         in fromRational x `shouldBe` BinaryApply Divide n d

    describe "Floating" $ do
      prop "pi" $ pi `shouldBe` Symbol "pi"

      prop "exp" $ \(Simple x) ->
        exp x `shouldBe` UnaryApply Exp x

      prop "log" $ \(Simple x) ->
        log x `shouldBe` UnaryApply Log x

      prop "sqrt" $ \(Simple x) ->
        sqrt x `shouldBe` UnaryApply Sqrt x

      prop "**" $ \(Simple x) (Simple y) ->
        x ** y `shouldBe` BinaryApply Power x y

      prop "logBase" $ \(Simple x) (Simple y) ->
        logBase x y `shouldBe` BinaryApply LogBase x y

      prop "sin" $ \(Simple x) ->
        sin x `shouldBe` UnaryApply Sin x

      prop "cos" $ \(Simple x) ->
        cos x `shouldBe` UnaryApply Cos x

      prop "tan" $ \(Simple x) ->
        tan x `shouldBe` UnaryApply Tan x

      prop "asin" $ \(Simple x) ->
        asin x `shouldBe` UnaryApply Asin x

      prop "acos" $ \(Simple x) ->
        acos x `shouldBe` UnaryApply Acos x

      prop "atan" $ \(Simple x) ->
        atan x `shouldBe` UnaryApply Atan x

      prop "sinh" $ \(Simple x) ->
        sinh x `shouldBe` UnaryApply Sinh x

      prop "cosh" $ \(Simple x) ->
        cosh x `shouldBe` UnaryApply Cosh x

      prop "tanh" $ \(Simple x) ->
        tanh x `shouldBe` UnaryApply Tanh x

      prop "asinh" $ \(Simple x) ->
        asinh x `shouldBe` UnaryApply Asinh x

      prop "acosh" $ \(Simple x) ->
        acosh x `shouldBe` UnaryApply Acosh x

      prop "atanh" $ \(Simple x) ->
        atanh x `shouldBe` UnaryApply Atanh x

  describe "substitute" $ do
    prop "for number" $ \n (SymbolMap m) ->
      substitute (Number n) (assign m) `shouldBe` Number n

    prop "for unmapped symbol" $ \(SymbolText s) ->
      substitute (Symbol s) (const Nothing) `shouldBe` Symbol s

    prop "for mapped symbol" $ \(SymbolText s) e (SymbolMap m) ->
      let m' = Map.insert s e m
       in substitute (Symbol s) (assign m') `shouldBe` e

    prop "for unary function" $ \func e (SymbolMap m) ->
      substitute (UnaryApply func e) (assign m)
        `shouldBe` UnaryApply func (substitute e (assign m))

    prop "for binary function" $ \func x y (SymbolMap m) ->
      substitute (BinaryApply func x y) (assign m)
        `shouldBe` BinaryApply func (substitute x $ assign m) (substitute y $ assign m)

  describe "Expression exactly evaluates as" $ do
    prop "number" $ \n (SymbolMap m) ->
      evaluate' (Number n) (assign m) `shouldBe` Just (fromInteger n)

    prop "symbol" $ \(SymbolText s) x ->
      evaluate' (Symbol s) (\s' -> if s' == s then Just x else Nothing) `shouldBe` Just x

    prop "unary function" $ \(Complete e m) func ->
      fmap Exact (evaluate' (UnaryApply func e) (assign m))
        `shouldBe` fmap (Exact . getUnaryFunction func) (evaluate' e (assign m))

    prop "binary function" $ \(Complete e1 m1) (Complete e2 m2) func ->
      let m = Map.union m1 m2
          f = getBinaryFunction func
       in fmap Exact (evaluate' (BinaryApply func e1 e2) (assign m))
            `shouldBe` fmap Exact (f <$> evaluate' e1 (assign m) <*> evaluate' e2 (assign m))

    prop "nothing" $ \(Complete e m) ->
      not (Map.null m) ==> evaluate' e (const Nothing) `shouldBe` Nothing

  describe "Expression fractionally evaluates as" $ do
    prop "number" $ \n ->
      fractionalEvaluate (Number n) (const Nothing) `shouldBe` Just (fromInteger n :: Rational)

    prop "symbol" $ \(SymbolText s) x ->
      fractionalEvaluate (Symbol s) (const $ Just x) `shouldBe` Just (x :: Rational)

    prop "similar to evaluate" $ \(Complete e m) ->
      let v = fractionalEvaluate e (fmap toRational . assign m)
          v' = evaluate e (assign m)
       in maybe False isFinite v' && isJust v ==>
            Near . FiniteDouble . fromRational <$> v `shouldBe` Near <$> v'

  describe "unary functions are correctly mapped for" $ do
    mapM_
      ( \(func, f) -> prop (show func) $ \x ->
          Exact (getUnaryFunction func x) `shouldBe` Exact (f x)
      )
      ( [ (Negate, negate),
          (Abs, abs),
          (Signum, signum),
          (Exp, exp),
          (Log, log),
          (Sqrt, sqrt),
          (Sin, sin),
          (Cos, cos),
          (Tan, tan),
          (Asin, asin),
          (Acos, acos),
          (Atan, atan),
          (Sinh, sinh),
          (Cosh, cosh),
          (Tanh, tanh),
          (Asinh, asinh),
          (Acosh, acosh),
          (Atanh, atanh)
        ] ::
          [(UnaryFunction, FiniteDouble -> FiniteDouble)]
      )

  describe "binary functions are correctly mapped for" $ do
    mapM_
      ( \(func, f) -> prop (show func) $
          \x y -> Exact (getBinaryFunction func x y) `shouldBe` Exact (f x y)
      )
      ( [ (Add, (+)),
          (Multiply, (*)),
          (Subtract, (-)),
          (Divide, (/)),
          (Power, (**)),
          (LogBase, logBase)
        ] ::
          [(BinaryFunction, FiniteDouble -> FiniteDouble -> FiniteDouble)]
      )

  describe "show" $ do
    prop "has inverse with read" $ \e ->
      read (show e) `shouldBe` (e :: Expression)
