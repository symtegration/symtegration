{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Description: QuickCheck Arbitrary instances for generating Symtegration.Symbolic values.
-- Copyright: Copyright 2024 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
module Symtegration.Symbolic.Arbitrary
  ( Simple (..),
    Compound (..),
    Complete (..),
    SymbolMap (..),
    SymbolText (..),
    arbitraryNumber,
    arbitrarySymbol,
    arbitraryUnaryFunction,
    arbitraryBinaryFunction,
    arbitrarySymbolText,
    shrinkSymbolText,
    arbitrarySymbolMap,
    shrinkSymbolMap,
    assign,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as S
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Symtegration.ErrorDouble
import Symtegration.FiniteDouble
import Symtegration.Symbolic
import Test.QuickCheck

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

  shrink (Number n) = Number <$> shrink n
  shrink (Symbol s) = Symbol <$> shrinkSymbolText s
  shrink (UnaryApply func x) = x : (UnaryApply func <$> shrink x)
  shrink (BinaryApply func x y) =
    x : y : [BinaryApply func x' y' | (x', y') <- shrink (x, y)]

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
-- The assignment of symbols to values will only contain symbols appearing in the expression.
-- Use the 'assign' function to turn the map into a function.
data Complete = Complete Expression (Map Text FiniteDouble) deriving (Eq, Show)

instance Arbitrary Complete where
  arbitrary = do
    expr <- arbitrary
    vals <- infiniteList
    let symbols = gatherSymbols expr
    let assignment = Map.fromList $ zip (S.toList symbols) vals
    let err = relativeError <$> evaluate expr (assign (Map.map includeError assignment))
    if err < Just 1e-5
      -- Only use expressions where slight divergences do not result in huge errors.
      then return $ Complete expr (Map.map FiniteDouble assignment)
      -- If we do not have such an expression, try again.
      else arbitrary

  shrink (Complete e m) = [Complete e' (restrict m e') | e' <- shrink e]
    where
      -- Keep symbol assignments still relevant to a shrinked expression.
      restrict xs x = Map.restrictKeys xs $ gatherSymbols x

-- | Gather the symbols appearing in an expression.
gatherSymbols :: Expression -> Set Text
gatherSymbols (Number _) = S.empty
gatherSymbols (Symbol s) = S.singleton s
gatherSymbols (UnaryApply _ x) = gatherSymbols x
gatherSymbols (BinaryApply _ x y) = S.union (gatherSymbols x) (gatherSymbols y)

-- | Generates a random assignment from symbols to values.
-- Use the 'assign' function to turn it into a function.
newtype SymbolMap a = SymbolMap (Map Text a) deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (SymbolMap a) where
  arbitrary = SymbolMap <$> arbitrarySymbolMap
  shrink (SymbolMap m) = SymbolMap <$> shrinkSymbolMap m

-- | Generates random readable symbol.
newtype SymbolText = SymbolText Text deriving (Eq, Show)

instance Arbitrary SymbolText where
  arbitrary = SymbolText <$> arbitrarySymbolText
  shrink (SymbolText s) = SymbolText <$> shrinkSymbolText s

-- | Generate a random number.
arbitraryNumber :: Gen Expression
arbitraryNumber = Number <$> arbitrary

-- | Generate a random symbol with only letters.
arbitrarySymbol :: Gen Expression
arbitrarySymbol = Symbol <$> arbitrarySymbolText

-- | Generate a random expression with an unary function application.
arbitraryUnaryFunction :: Gen Expression
arbitraryUnaryFunction = UnaryApply <$> arbitrary <*> arbitrary

-- | Generate a random expression with a binary function application.
arbitraryBinaryFunction :: Gen Expression
arbitraryBinaryFunction = BinaryApply <$> arbitrary <*> arbitrary <*> arbitrary

-- | Generate a random map from readable symbols to values.
arbitrarySymbolMap :: (Arbitrary a) => Gen (Map Text a)
arbitrarySymbolMap = Map.fromList <$> listOf assocs
  where
    assocs = do
      s <- arbitrarySymbolText
      x <- arbitrary
      return (s, x)

-- | Shrinks a map from readable symbols to values.
shrinkSymbolMap :: (Arbitrary a) => Map Text a -> [Map Text a]
shrinkSymbolMap = shrinkMapBy Map.fromList Map.toList (shrinkList shrinkAssoc)
  where
    shrinkAssoc (s, x) = do
      s' <- shrinkSymbolText s
      x' <- shrink x
      return (s', x')

-- | Generate random text that is appropriate as a readable symbol.
-- They will be short, since what exactly are in the symbols is usually not important.
-- Does not generate the special symbol "pi".
arbitrarySymbolText :: Gen Text
arbitrarySymbolText = resize 3 $ fromString <$> listOf1 (choose ('a', 'z')) `suchThat` (/= "pi")

-- | Shrinks readable symbols.
shrinkSymbolText :: Text -> [Text]
shrinkSymbolText s =
  -- Exclude empty text and s itself.
  drop 1 $ reverse $ drop 1 $ Text.tails s

-- | For creating a function which assigns symbols to values
-- based on the given map, which are easier to generate with
-- specific properties and easier to show than a function itself.
-- Shorthand for writing @assign m@ instead of @flip Map.lookup m@.
assign :: Map Text a -> Text -> Maybe a
assign = flip Map.lookup
