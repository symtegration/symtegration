module Symtegration.Symbolic.Simplify.AlgebraicRingOrder where

import Data.List (sortBy)
import Data.Text (Text)
import Symtegration.Symbolic

order :: Text -> Expression -> Expression
order v e = fromAddList $ sortBy (compareExpressions v) orderedAddTerms
  where
    terms = toAddMultiplyList e
    orderedAddTerms = map (fromMultiplyList . sortBy (compareExpressions v)) terms

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

compareExpressions :: Text -> Expression -> Expression -> Ordering
compareExpressions v x y
  | (Just LT) <- compareDegree = LT
  | (Just GT) <- compareDegree = GT
  | LT <- comparePseudoDegree = LT
  | GT <- comparePseudoDegree = GT
  | LT <- compareOp = LT
  | GT <- compareOp = GT
  | Number n <- x, Number m <- y = compare n m
  | Symbol s <- x, Symbol r <- y = compare s r
  | UnaryApply _ x' <- x, UnaryApply _ y' <- y = compareExpressions v x' y'
  | BinaryApply _ x' x'' <- x,
    BinaryApply _ y' y'' <- y =
      case compareExpressions v x' y' of
        EQ -> compareExpressions v x'' y''
        c -> c
  | otherwise = EQ
  where
    compareDegree = do
      xd <- degree v x
      yd <- degree v y
      return $ compare xd yd
    comparePseudoDegree = compare (pseudoDegree v x) (pseudoDegree v y)
    compareOp = compare (expressionOrder v x) (expressionOrder v y)

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

expressionOrder :: Text -> Expression -> Int
expressionOrder _ (Number _) = 0
-- constant symbol has expressionOrder 1
expressionOrder _ (UnaryApply Negate _) = 2
expressionOrder _ (UnaryApply Signum _) = 3
expressionOrder _ (UnaryApply Abs _) = 4
expressionOrder _ (BinaryApply Add _ _) = 5
expressionOrder _ (BinaryApply Subtract _ _) = 6
expressionOrder _ (BinaryApply Multiply _ _) = 7
expressionOrder _ (BinaryApply Divide _ _) = 8
expressionOrder _ (BinaryApply Power _ _) = 9
expressionOrder _ (UnaryApply Sqrt _) = 10
expressionOrder _ (UnaryApply Exp _) = 11
expressionOrder _ (UnaryApply Log _) = 12
expressionOrder _ (BinaryApply LogBase _ _) = 13
expressionOrder _ (UnaryApply Sin _) = 14
expressionOrder _ (UnaryApply Cos _) = 15
expressionOrder _ (UnaryApply Tan _) = 16
expressionOrder _ (UnaryApply Asin _) = 17
expressionOrder _ (UnaryApply Acos _) = 18
expressionOrder _ (UnaryApply Atan _) = 19
expressionOrder _ (UnaryApply Sinh _) = 20
expressionOrder _ (UnaryApply Cosh _) = 21
expressionOrder _ (UnaryApply Tanh _) = 22
expressionOrder _ (UnaryApply Asinh _) = 23
expressionOrder _ (UnaryApply Acosh _) = 24
expressionOrder _ (UnaryApply Atanh _) = 25
expressionOrder v (Symbol s)
  | v == s = 26
  | otherwise = 1
