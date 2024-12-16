module Symtegration.Symbolic.Simplify.NumericFolding (simplify) where

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
unary (Sqrt' x) = simplify $ x :**: (Number 1 :/: Number 2)
unary (Sin' x) = simplifySin x
unary (Cos' x) = simplifyCos x
unary (Tan' x) = simplifyTan x
unary e = e

binary :: Expression -> Expression
-- Fold addition.
binary (Number n :+: Number m) = Number (n + m)
binary ((Number n :/: Number m) :+: Number k) = reduceRatio (n + m * k) m
binary (Number n :+: (Number m :/: Number k)) = reduceRatio (n * k + m) k
binary ((Number n :/: Number m) :+: (Number k :/: Number l)) = reduceRatio (n * l + k * m) (m * l)
binary ((x :+: Number n) :+: Number m) = Number (n + m) :+: x
binary ((Number n :+: x) :+: Number m) = Number (n + m) :+: x
binary (Number n :+: (x :+: Number m)) = Number (n + m) :+: x
binary (Number n :+: (Number m :+: x)) = Number (n + m) :+: x
-- Fold multiplication.
binary (Number n :*: Number m) = Number (n * m)
binary (Number n :*: (Number m :/: Number k)) = reduceRatio (n * m) k
binary ((Number n :/: Number m) :*: Number k) = reduceRatio (n * k) m
binary ((Number n :/: Number m) :*: (Number k :/: Number l)) = reduceRatio (n * k) (m * l)
binary ((x :*: Number n) :*: Number m) = Number (n * m) :*: x
binary ((Number n :*: x) :*: Number m) = Number (n * m) :*: x
binary (Number n :*: (x :*: Number m)) = Number (n * m) :*: x
binary (Number n :*: (Number m :*: x)) = Number (n * m) :*: x
-- Subtractions are turned into addition.
binary (x :-: y) = simplify $ x :+: Negate' y
-- Fold division.
binary (x :/: (y :/: z)) = simplify $ (x :*: z) :/: y
binary ((x :/: y) :/: z) = simplify $ x :/: (y :*: z)
binary (Number n :/: Number m) = reduceRatio n m
-- Fold exponentiation.
binary e@(Number 0 :**: Number 0) = e
binary (Number 0 :**: _) = Number 0
binary (Number 1 :**: _) = Number 1
binary (Number n :**: Number m)
  | m >= 0 = Number (n ^ m)
  | otherwise = Number 1 :/: Number (n ^ (-m))
binary ((Number n :/: Number m) :**: Number k) = Number (n ^ k) :/: Number (m ^ k)
binary e@(Number n :**: (Number m :/: Number k))
  | (Just l) <- root n k = Number (l ^ m)
  | otherwise = e
binary e@((Number n :/: Number m) :**: (Number k :/: Number l))
  | (Just n', Just m') <- (root n l, root m l) = (Number n' :/: Number m') :**: Number k
  | otherwise = e
-- Turn LogBase into Log.
binary (LogBase' b x) = simplify $ Log' x :/: Log' b
binary e = e

reduceRatio :: Integer -> Integer -> Expression
reduceRatio n 0 = Number n :/: Number 0
reduceRatio n 1 = Number n
reduceRatio n m
  | m == d = Number (n `div` m)
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
      | low >= hi, c /= EQ = Nothing
      | EQ <- c = Just mid
      | LT <- c = search m low (mid - 1)
      | GT <- c = search m (mid + 1) hi
      where
        mid = (low + hi) `div` 2
        c = compare (mid ^ k) m

simplifyExp :: Expression -> Expression
simplifyExp (Number 0) = Number 1
simplifyExp (Log' x) = x
simplifyExp e = Exp' e

simplifyLog :: Expression -> Expression
simplifyLog (Number 1) = Number 0
simplifyLog (Exp' x) = x
simplifyLog e = Log' e

simplifySin :: Expression -> Expression
simplifySin (Number 0) = 0
simplifySin (Number _ :*: Pi') = 0
simplifySin (Pi' :*: Number _) = 0
simplifySin ((Number n :/: 2) :*: Pi')
  | even n = 0
  | odd ((n - 1) `div` 2) = 1
  | otherwise = -1
simplifySin (Pi' :*: x@(Number _ :/: 2)) =
  simplifySin (x :*: Pi')
simplifySin e = Sin' e

simplifyCos :: Expression -> Expression
simplifyCos (Number 0) = 1
simplifyCos (Number n :*: Pi') | even n = 1 | odd n = -1
simplifyCos (Pi' :*: Number _) = 0
-- Any 2k/2 would have been simplified to k already.
simplifyCos ((Number _ :/: 2) :*: Pi') = 0
simplifyCos (Pi' :*: (Number _ :/: 2)) = 0
simplifyCos e = Cos' e

simplifyTan :: Expression -> Expression
simplifyTan (Number 0) = 0
simplifyTan e = Tan' e
