module Symtegration.Symbolic.Simplify.NumericFolding where

import Symtegration.Symbolic

simplify :: Expression -> Expression
simplify e@(Number _) = e
simplify e@(Symbol _) = e
simplify (UnaryApply func x) =
  unary $ UnaryApply func $ simplify x
simplify (BinaryApply func x y) =
  binary $ BinaryApply func (simplify x) (simplify y)

unary :: Expression -> Expression
unary (Negate' (Number n)) = Number (-n)
unary (Abs' (Number n)) = Number $ abs n
unary (Signum' (Number n)) = Number $ signum n
unary (Exp' x) = simplifyExp x
unary (Log' x) = simplifyLog x
unary (Sqrt' x) = simplifySqrt x
unary (Sin' x) = simplifySin x
unary e = e

binary :: Expression -> Expression
binary (Number n :+: Number m) = Number (n + m)
binary ((Number n :/: Number m) :+: Number k) = reduceRatio (n + m * k) m
binary (Number n :+: (Number m :/: Number k)) = reduceRatio (n * k + m) k
binary ((Number n :/: Number m) :+: (Number k :/: Number l)) = reduceRatio (n * l + k * m) (m * l)
binary ((x :+: Number n) :+: Number m) = Number (n + m) :+: x
binary ((Number n :+: x) :+: Number m) = Number (n + m) :+: x
binary (Number n :+: (x :+: Number m)) = Number (n + m) :+: x
binary (Number n :+: (Number m :+: x)) = Number (n + m) :+: x
binary (Number n :*: Number m) = Number (n * m)
binary (Number n :*: (Number m :/: Number k)) = reduceRatio (n * m) k
binary ((Number n :/: Number m) :*: Number k) = reduceRatio (n * k) m
binary ((Number n :/: Number m) :*: (Number k :/: Number l)) = reduceRatio (n * k) (m * l)
binary ((x :*: Number n) :*: Number m) = Number (n * m) :*: x
binary ((Number n :*: x) :*: Number m) = Number (n * m) :*: x
binary (Number n :*: (x :*: Number m)) = Number (n * m) :*: x
binary (Number n :*: (Number m :*: x)) = Number (n * m) :*: x
binary (x :-: y) = simplify $ x :+: Negate' y
binary (x :/: (y :/: z)) = simplify $ (x :*: z) :/: y
binary ((x :/: y) :/: z) = simplify $ x :/: (y :*: z)
binary (Number n :/: Number m) = reduceRatio n m
binary e@(Number 0 :**: Number 0) = e
binary (Number 0 :**: _) = Number 0
binary (Number 1 :**: _) = Number 1
binary (Number n :**: Number m)
  | m >= 0 = Number (n ^ m)
  | otherwise = Number 1 :/: Number (n ^ m)
binary ((Number n :/: Number m) :**: Number k) = Number (n ^ k) :/: Number (m ^ k)
binary e = e

reduceRatio :: Integer -> Integer -> Expression
reduceRatio n 0 = Number n :/: Number 0
reduceRatio n 1 = Number n
reduceRatio n m
  | n == d = Number (n `div` m)
  | otherwise = Number (n `div` d) :/: Number (m `div` d)
  where
    d = gcd n m

root ::
  -- | Number whose root we want.
  Integer ->
  -- | The power of the root.
  Integer ->
  -- | The root.
  Maybe Integer
root 0 _ = Just 0
root 1 _ = Just 1
root n k
  | GT <- compare n 0 = search n 1 n
  | LT <- compare n 0, odd k = (* (-1)) <$> search (-n) 1 (-n)
  | otherwise = Nothing
  where
    search m low hi
      | low == hi, low ^ k == m = Just low
      | otherwise = undefined

simplifyExp :: Expression -> Expression
simplifyExp (Number 0) = Number 1
simplifyExp (Log' x) = x
simplifyExp e = e

simplifyLog :: Expression -> Expression
simplifyLog (Number 1) = Number 0
simplifyLog (Exp' x) = x
simplifyLog e = e

simplifySqrt :: Expression -> Expression
simplifySqrt (Number 0) = Number 0
simplifySqrt (x@(Number _) :**: 2) = x
simplifySqrt e = e

simplifySin :: Expression -> Expression
simplifySin (Number 0) = 0
simplifySin Pi' = 1
simplifySin e = e
