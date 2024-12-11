module Symtegration.Polynomial.Symbolic where

import Data.Monoid (Sum (..))
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import Symtegration.Polynomial
import Symtegration.Symbolic

fromExpression ::
  (Polynomial p e c, Num (p e c), Integral e, Fractional c) =>
  (Text -> Maybe (p e c), Expression -> Maybe c) ->
  Expression ->
  Maybe (p e c)
fromExpression _ (Number n) = Just $ fromInteger n
fromExpression (cf, _) (Symbol x) = cf x
fromExpression t (Negate' x) = negate <$> fromExpression t x
fromExpression t (x :+: y) = (+) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :*: y) = (*) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :-: y) = (-) <$> fromExpression t x <*> fromExpression t y
fromExpression t (x :**: (Number n))
  | n >= 0 = (^ n) <$> fromExpression t x
  | otherwise = Nothing
fromExpression _ (_ :**: _) = Nothing
fromExpression _ (Number n :/: Number m) = Just $ scale r 1
  where
    r = fromInteger n / fromInteger m
fromExpression (_, eval) e
  | Just e' <- eval e = Just $ scale e' 1
  | otherwise = Nothing

forVariable :: (Polynomial p e c) => Text -> Text -> Maybe (p e c)
forVariable x s
  | x == s = Just $ power 1
  | otherwise = Nothing

withSymbolicCoefficientsForVariable ::
  (Polynomial p e Expression) =>
  Text ->
  Text ->
  Maybe (p e Expression)
withSymbolicCoefficientsForVariable x s
  | x == s = Just $ power 1
  | otherwise = Just $ scale (Symbol s) $ power 0

toExpression :: (Polynomial p e c) => Text -> (c -> Expression) -> p e c -> Expression
toExpression x cf p = getSum $ foldTerms convert p
  where
    convert e c = Sum $ cf c * xp
      where
        xp = Symbol x ** Number (fromIntegral e)

toRationalCoefficient :: (Real c) => c -> Expression
toRationalCoefficient c
  | d == 1 = Number n
  | otherwise = Number n :/: Number d
  where
    r = toRational c
    n = fromInteger $ numerator r
    d = fromInteger $ denominator r

toSymbolicCoefficient :: Expression -> Expression
toSymbolicCoefficient = id
