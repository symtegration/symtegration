module Symtegration.Symbolic.Simplify.AlgebraicRingOrder where

import Data.Text (Text)
import Symtegration.Symbolic

-- No particular ordering should be expected.
toAddMultiplyList :: Expression -> [[Expression]]
toAddMultiplyList (x@(_ :+: _) :+: y@(_ :+: _)) = toAddMultiplyList x ++ toAddMultiplyList y
toAddMultiplyList (x@(_ :+: _) :+: y) = toMultiplyList y : toAddMultiplyList x
toAddMultiplyList (x :+: y@(_ :+: _)) = toMultiplyList x : toAddMultiplyList y
toAddMultiplyList (x :+: y) = map toMultiplyList [x, y]
toAddMultiplyList (x :-: y) = toAddMultiplyList (x :+: (Number (-1) :*: y))
toAddMultiplyList x = [toMultiplyList x]

-- No particular ordering should be expected.
toMultiplyList :: Expression -> [Expression]
toMultiplyList x@(Number _) = [x]
toMultiplyList x@(Symbol _) = [x]
toMultiplyList (Negate' x) = (-1) : toMultiplyList x
toMultiplyList (x@(_ :*: _) :*: y@(_ :*: _)) = toMultiplyList x ++ toMultiplyList y
toMultiplyList (x@(_ :*: _) :*: y) = y : toMultiplyList x
toMultiplyList (x :*: y@(_ :*: _)) = x : toMultiplyList y
toMultiplyList (x :*: y) = [x, y]
toMultiplyList x = [x]

fromMultiplyList :: [Expression] -> Expression
fromMultiplyList [] = Number 1
fromMultiplyList [x] = x
fromMultiplyList (x : xs) = x :*: fromMultiplyList xs

fromAddList :: [Expression] -> Expression
fromAddList [] = Number 0
fromAddList [x] = x
fromAddList (x : xs) = x :+: fromAddList xs

compareExpr :: Text -> Expression -> Expression -> Ordering
compareExpr v x y
  | (Just LT) <- compareDegree = LT
  | (Just GT) <- compareDegree = GT
  | LT <- comparePseudoDegree = LT
  | GT <- comparePseudoDegree = GT
  | otherwise = undefined
  where
    compareDegree = do
      xd <- degree v x
      yd <- degree v y
      return $ compare xd yd
    comparePseudoDegree = compare (pseudoDegree v x) (pseudoDegree v y)

degree :: Text -> Expression -> Maybe Integer
degree _ (Number _) = Just 0
degree v (Symbol s) | v == s = Just 1 | otherwise = Just 0
degree v (Negate' x) = degree v x
degree v (x :+: y) = max <$> degree v x <*> degree v y
degree v (x :-: y) = max <$> degree v x <*> degree v y
degree v (x :*: y) = (+) <$> degree v x <*> degree v y
degree v (x :/: y) = (-) <$> degree v x <*> degree v y
degree v (x :**: (Number n)) = (n *) <$> degree v x
degree v (x :**: Negate' y) = degree v $ x :**: y
degree _ _ = Nothing

pseudoDegree :: Text -> Expression -> Integer
pseudoDegree _ (Number _) = 0
pseudoDegree v (Symbol s) | v == s = 1 | otherwise = 0
pseudoDegree v (Negate' x) = pseudoDegree v x
pseudoDegree v (UnaryApply _ x) = pseudoDegree v x
pseudoDegree v (BinaryApply _ x y) = pseudoDegree v x + pseudoDegree v y

order :: Text -> Expression -> Int
order _ (Number _) = 0
-- constant symbol has order 1
order _ (UnaryApply Negate _) = 2
order _ (UnaryApply Signum _) = 3
order _ (UnaryApply Abs _) = 4
order _ (BinaryApply Add _ _) = 5
order _ (BinaryApply Subtract _ _) = 6
order _ (BinaryApply Multiply _ _) = 7
order _ (BinaryApply Divide _ _) = 8
order _ (BinaryApply Power _ _) = 9
order _ (UnaryApply Sqrt _) = 10
order _ (UnaryApply Exp _) = 11
order _ (UnaryApply Log _) = 12
order _ (BinaryApply LogBase _ _) = 13
order _ (UnaryApply Sin _) = 14
order _ (UnaryApply Cos _) = 15
order _ (UnaryApply Tan _) = 16
order _ (UnaryApply Asin _) = 17
order _ (UnaryApply Acos _) = 18
order _ (UnaryApply Atan _) = 19
order _ (UnaryApply Sinh _) = 20
order _ (UnaryApply Cosh _) = 21
order _ (UnaryApply Tanh _) = 22
order _ (UnaryApply Asinh _) = 23
order _ (UnaryApply Acosh _) = 24
order _ (UnaryApply Atanh _) = 25
order v (Symbol s)
  | v == s = 36
  | otherwise = 2
