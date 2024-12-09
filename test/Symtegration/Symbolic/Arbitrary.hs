{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: QuickCheck Arbitrary instances for generating Symtegration.Symbolic values.
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Arbitrary
  ( Simple (..),
    Compound (..),
    Complete (..),
    arbitraryNumber,
    arbitrarySymbol,
    arbitraryUnaryFunction,
    arbitraryBinaryFunction,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Symtegration.Symbolic
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

instance Arbitrary Expression where
  arbitrary = sized $ \n -> case n of
    0 -> oneof [arbitraryNumber, arbitrarySymbol]
    _ ->
      frequency
        [ (1, arbitraryNumber),
          (1, arbitrarySymbol),
          (4, resize (max 0 (n - 1)) arbitraryUnaryFunction),
          (8, resize (n `div` 2) arbitraryBinaryFunction)
        ]

  shrink = genericShrink

instance Arbitrary UnaryFunction where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary BinaryFunction where
  arbitrary = chooseEnum (minBound, maxBound)

-- | Generates simple symbolic mathematical expressions.
-- Specically, those which represent a single symbol or a single number.
newtype Simple = Simple Expression deriving (Eq, Show)

instance Arbitrary Simple where
  arbitrary = Simple <$> oneof [arbitraryNumber, arbitrarySymbol]

-- | Generates a compound symbolic mathematical expression.
-- Specifically, either a unary function application or a binary function application.
newtype Compound = Compound Expression deriving (Eq, Show)

instance Arbitrary Compound where
  arbitrary = Compound <$> oneof [arbitraryUnaryFunction, arbitraryBinaryFunction]
  shrink (Compound e) = Compound <$> filter isCompound (shrink e)
    where
      isCompound (Number _) = False
      isCompound (Symbol _) = False
      isCompound _ = True

-- | Generates arbitrary expressions with a complete assignment of numbers to symbols.
data Complete = Complete Expression (Map Text Double) deriving (Eq, Show)

instance Arbitrary Complete where
  arbitrary = do
    expr <- arbitrary
    vals <- infiniteList
    let symbols = gather expr
    let assignment = Map.fromList $ zip (S.toList symbols) vals
    return $ Complete expr assignment
    where
      gather (Number _) = S.empty
      gather (Symbol s) = S.singleton s
      gather (UnaryApply _ x) = gather x
      gather (BinaryApply _ x y) = S.union (gather x) (gather y)

  shrink (Complete e m) = [Complete e' m | e' <- genericShrink e]

-- | Generate a random number.
arbitraryNumber :: Gen Expression
arbitraryNumber = Number <$> arbitrary

-- | Generate a random symbol with only letters.
arbitrarySymbol :: Gen Expression
arbitrarySymbol = Symbol . fromString <$> listOf1 (choose ('a', 'z'))

-- | Generate a random expression with an unary function application.
arbitraryUnaryFunction :: Gen Expression
arbitraryUnaryFunction = UnaryApply <$> arbitrary <*> arbitrary

-- | Generate a random expression with a binary function application.
arbitraryBinaryFunction :: Gen Expression
arbitraryBinaryFunction = BinaryApply <$> arbitrary <*> arbitrary <*> arbitrary
