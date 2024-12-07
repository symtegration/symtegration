-- |
-- Description: Tests for Symtegration.SymbolicSpec
-- Maintainer: dev@chungyc.org
module Symtegration.SymbolicSpec where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Ratio (denominator, numerator)
import Data.String (fromString)
import Data.Text (Text)
import Symtegration.Symbolic
import Symtegration.Symbolic.Arbitrary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

-- | Same as 'evaluate', except specialized to 'Double'.
evaluate' :: Expression -> Map Text Double -> Maybe Double
evaluate' = evaluate

spec :: Spec
spec = do
  describe "Expression from" $ modifyMaxSuccess (`div` 10) $ do
    describe "IsString" $ do
      prop "fromString" $
        \(PrintableString s) -> fromString s `shouldBe` Symbol (fromString s)

    describe "Num" $ do
      prop "+" $
        \(Simple x) (Simple y) -> x + y `shouldBe` BinaryApply Add x y

      prop "-" $
        \(Simple x) (Simple y) -> x - y `shouldBe` BinaryApply Subtract x y

      prop "*" $
        \(Simple x) (Simple y) -> x * y `shouldBe` BinaryApply Multiply x y

      prop "negate" $
        \(Simple x) -> negate x `shouldBe` UnaryApply Negate x

      prop "abs" $
        \(Simple x) -> abs x `shouldBe` UnaryApply Abs x

      prop "signum" $
        \(Simple x) -> signum x `shouldBe` UnaryApply Signum x

      prop "fromInteger" $
        \n -> fromInteger n `shouldBe` Number n

    describe "Fractional" $ do
      prop "/" $
        \(Simple x) (Simple y) -> x / y `shouldBe` BinaryApply Divide x y

      prop "recip" $
        \(Simple x) -> recip x `shouldBe` BinaryApply Divide 1 x

      prop "fromRational" $
        \x ->
          let n = fromInteger $ numerator x
              d = fromInteger $ denominator x
           in fromRational x `shouldBe` BinaryApply Divide n d

    describe "Floating" $ do
      prop "pi" $ pi `shouldBe` Symbol "pi"

      prop "exp" $
        \(Simple x) -> exp x `shouldBe` UnaryApply Exp x

      prop "log" $
        \(Simple x) -> log x `shouldBe` UnaryApply Log x

      prop "sqrt" $
        \(Simple x) -> sqrt x `shouldBe` UnaryApply Sqrt x

      prop "**" $
        \(Simple x) (Simple y) -> x ** y `shouldBe` BinaryApply Power x y

      prop "logBase" $
        \(Simple x) (Simple y) -> logBase x y `shouldBe` BinaryApply LogBase x y

      prop "sin" $
        \(Simple x) -> sin x `shouldBe` UnaryApply Sin x

      prop "cos" $
        \(Simple x) -> cos x `shouldBe` UnaryApply Cos x

      prop "tan" $
        \(Simple x) -> tan x `shouldBe` UnaryApply Tan x

      prop "asin" $
        \(Simple x) -> asin x `shouldBe` UnaryApply Asin x

      prop "acos" $
        \(Simple x) -> acos x `shouldBe` UnaryApply Acos x

      prop "atan" $
        \(Simple x) -> atan x `shouldBe` UnaryApply Atan x

      prop "sinh" $
        \(Simple x) -> sinh x `shouldBe` UnaryApply Sinh x

      prop "cosh" $
        \(Simple x) -> cosh x `shouldBe` UnaryApply Cosh x

      prop "tanh" $
        \(Simple x) -> tanh x `shouldBe` UnaryApply Tanh x

      prop "asinh" $
        \(Simple x) -> asinh x `shouldBe` UnaryApply Asinh x

      prop "acosh" $
        \(Simple x) -> acosh x `shouldBe` UnaryApply Acosh x

      prop "atanh" $
        \(Simple x) -> atanh x `shouldBe` UnaryApply Atanh x

  describe "Expression evaluates as" $ do
    prop "number" $
      \n m -> evaluate' (Number n) m `shouldBe` Just (fromInteger n)

    prop "symbol" $
      \s x m -> evaluate' (Symbol s) (M.insert s x m) `shouldBe` Just x

    prop "unary function" $
      \(Complete e m) func ->
        fmap EvaluateDouble (evaluate' (UnaryApply func e) m)
          `shouldBe` fmap (EvaluateDouble . getUnaryFunction func) (evaluate' e m)

    prop "binary function" $
      \(Complete e1 m1) (Complete e2 m2) func ->
        let m = M.union m1 m2
            f = getBinaryFunction func
         in fmap EvaluateDouble (evaluate' (BinaryApply func e1 e2) m)
              `shouldBe` fmap EvaluateDouble (f <$> evaluate' e1 m <*> evaluate' e2 m)

  describe "unary functions are correctly mapped for" $ do
    mapM_
      ( \(func, f) -> prop (show func) $
          \x ->
            EvaluateDouble (getUnaryFunction func x)
              `shouldBe` EvaluateDouble (f x)
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
          [(UnaryFunction, Double -> Double)]
      )

  describe "binary functions are correctly mapped for" $ do
    mapM_
      ( \(func, f) -> prop (show func) $
          \x y ->
            EvaluateDouble (getBinaryFunction func x y)
              `shouldBe` EvaluateDouble (f x y)
      )
      ( [ (Add, (+)),
          (Multiply, (*)),
          (Subtract, (-)),
          (Divide, (/)),
          (Power, (**)),
          (LogBase, logBase)
        ] ::
          [(BinaryFunction, Double -> Double -> Double)]
      )

-- | Wrapper type over 'Double' so that the same return values are compared as equal.
-- In other words, the same NaN compared to itself will be considered equal.
-- Used for confirming that 'evaluate' returns the expected results,
-- including when it returns NaN.
newtype EvaluateDouble = EvaluateDouble Double deriving (Show)

instance Eq EvaluateDouble where
  (EvaluateDouble x) == (EvaluateDouble y)
    | isNaN x && isNaN y = True
    | otherwise = x == y
