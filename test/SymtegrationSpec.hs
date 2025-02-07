module SymtegrationSpec (spec) where

import Data.Ratio ((%))
import Symtegration
import Test.Hspec

spec :: Spec
spec = parallel $ do
  -- If any of these tests fail, the examples in the README need to be updated.
  describe "README examples" $ do
    it "toHaskell <$> integrate x (4 * x ** 3 + 1)" $
      toHaskell <$> integrate "x" (4 * "x" ** 3 + 1) `shouldBe` Just "x + x ** 4"

    it "toHaskell <$> integrate z (x * z + y)" $
      toHaskell <$> integrate "z" ("x" * "z" + "y") `shouldBe` Just "y * z + 1 / 2 * x * z ** 2"

    it "fractionalEvaluate p (\\case x -> Just (3 / 7 :: Rational))" $
      let p | Just p' <- integrate "x" (4 * "x" ** 3 + 1) = p' | otherwise = 1
       in fractionalEvaluate p (\case "x" -> Just (3 / 7 :: Rational); _ -> Nothing)
            `shouldBe` Just (1110 % 2401)

    it "toHaskell <$> integrate x (a * x ** 4 + x + b)" $
      toHaskell <$> integrate "x" ("a" * "x" ** 4 + "x" + "b")
        `shouldBe` Just "b * x + 1 / 2 * x ** 2 + a * (x ** 5) / 5"

    it "evaluate p (\\case x -> Just 1)" $
      let p | Just p' <- integrate "x" ("x" ** 2) = p' | otherwise = 1
       in evaluate p (\case "x" -> Just 1; _ -> Nothing)
            `shouldBe` Just (0.3333333333333333 :: Double)

    it "fractionalEvaluate p (\\case x -> Just (1 :: Rational))" $
      let p | Just p' <- integrate "x" ("x" ** 2) = p' | otherwise = 1
       in fractionalEvaluate p (\case "x" -> Just (1 :: Rational); _ -> Nothing)
            `shouldBe` Just (1 % 3)

  -- If any of these tests fail, the examples on https://symtegration.dev/ need to be updated.
  describe "site examples" $ do
    describe "https://symtegration.dev/" $ do
      it "toHaskell <$> integrate x (4 * x **3 + 1)" $
        toHaskell <$> integrate "x" (4 * "x" ** 3 + 1) `shouldBe` Just "x + x ** 4"

      it "toHaskell <$> integrate x (1 / (1 + x ** 2))" $
        toHaskell <$> integrate "x" (1 / (1 + "x" ** 2)) `shouldBe` Just "atan x"

    describe "https://symtegration.dev/usage/" $ do
      it "toHaskell <$> integrate x (a * x ** 4 + x + b)" $
        toHaskell <$> integrate "x" ("a" * "x" ** 4 + "x" + "b")
          `shouldBe` Just "b * x + 1 / 2 * x ** 2 + a * (x ** 5) / 5"

      it "evaluate p (\\case x -> Just 1)" $
        let p | Just p' <- integrate "x" ("x" ** 2) = p' | otherwise = 1
         in evaluate p (\case "x" -> Just 1; _ -> Nothing)
              `shouldBe` Just (0.3333333333333333 :: Double)

      it "fractionalEvaluate p (\\case x -> Just (1 :: Rational))" $
        let p | Just p' <- integrate "x" ("x" ** 2) = p' | otherwise = 1
         in fractionalEvaluate p (\case "x" -> Just (1 :: Rational); _ -> Nothing)
              `shouldBe` Just (1 % 3)
