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
import Generic.Random
import Symtegration.Symbolic
import Test.QuickCheck
import Test.QuickCheck.Instances.Text ()

instance Arbitrary Expression where
  arbitrary =
    genericArbitraryRec (1 % 1 % 4 % 8 % ())
      `withBaseCase` oneof
        [ Number <$> arbitrary,
          Symbol . fromString <$> listOf1 arbitraryPrintableChar
        ]
  shrink = genericShrink

instance Arbitrary UnaryFunction where
  arbitrary = genericArbitraryU

instance Arbitrary BinaryFunction where
  arbitrary = genericArbitraryU

-- | QuickCheck modifier for generating simple symbolic mathematical expressions.
-- Specically, those which represent a single symbol or a single number.
newtype Simple = Simple Expression deriving (Eq, Show)

instance Arbitrary Simple where
  arbitrary =
    oneof
      [ Simple . Number <$> arbitrary,
        Simple . Symbol . fromString <$> listOf1 arbitraryPrintableChar
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
