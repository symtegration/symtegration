module Symtegration.Symbolic.Simplify.AlgebraicRingOrder where

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
