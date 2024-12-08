{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: QuickCheck Arbitrary instances for generating Symtegration.Symbolic values.
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Arbitrary where

import Data.Map (Map)
import Data.Map qualified as M
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
          (4, resize (max 0 (n - 1)) $ UnaryApply <$> arbitrary <*> arbitrary),
          (8, resize (n `div` 2) $ BinaryApply <$> arbitrary <*> arbitrary <*> arbitrary)
        ]
    where
      arbitraryNumber = Number <$> arbitrary
      arbitrarySymbol = Symbol . fromString <$> listOf1 (choose ('a', 'z'))

  shrink = genericShrink

instance Arbitrary UnaryFunction where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary BinaryFunction where
  arbitrary = chooseEnum (minBound, maxBound)

-- | QuickCheck modifier for generating simple symbolic mathematical expressions.
-- Specically, those which represent a single symbol or a single number.
newtype Simple = Simple Expression deriving (Eq, Show)

instance Arbitrary Simple where
  arbitrary =
    oneof
      [ Simple . Number <$> arbitrary,
        Simple . Symbol . fromString <$> listOf1 (choose ('a', 'z'))
      ]

-- | Generates arbitrary expressions with a complete assignment of numbers to symbols.
data Complete = Complete Expression (Map Text Double) deriving (Eq, Show)

instance Arbitrary Complete where
  arbitrary = do
    expr <- arbitrary
    vals <- infiniteList
    let symbols = gather expr
    let assignment = M.fromList $ zip (S.toList symbols) vals
    return $ Complete expr assignment
    where
      gather (Number _) = S.empty
      gather (Symbol s) = S.singleton s
      gather (UnaryApply _ x) = gather x
      gather (BinaryApply _ x y) = S.union (gather x) (gather y)

  shrink (Complete e m) = [Complete e' m | e' <- genericShrink e]
