-- |
-- Module: Symtegration.Numeric
-- Description: Numerical algorithms that are useful for implementing symbolic integration.
-- Copyright: Copyright 2025 Yoo Chung
-- License: Apache-2.0
-- Maintainer: dev@chungyc.org
--
-- This module contains numerical algorithms that are useful to more than one module,
-- ultimately for the purpose of symbolic integration of mathematical expressions.
-- By numerical algorithms here, we means algorithm that work on pure numbers and not symbols.
-- The algorithms should still return exact results.
module Symtegration.Numeric (root) where

-- | Compute the integer root to the given power.
-- I.e., find \(m\) such that \(m^k = n\).
--
-- >>> root 27 3
-- Just 3
-- >>> root (-27) 3
-- Just (-3)
-- >>> root 2 2
-- Nothing
root ::
  -- | Number \(n\) whose root we want.
  Integer ->
  -- | The power \(k\) of the root.
  Integer ->
  -- | The root \(m\).
  Maybe Integer
root 0 _ = Just 0
root 1 _ = Just 1
root n k
  | k < 0 = Nothing
  | GT <- compare n 0 = search n 1 n
  | LT <- compare n 0, odd k = (* (-1)) <$> search (-n) 1 (-n)
  | otherwise = Nothing
  where
    search m low hi
      | low >= hi, c /= EQ = Nothing
      | EQ <- c = Just mid
      | GT <- c = search m low (mid - 1)
      | LT <- c = search m (mid + 1) hi
      where
        mid = (low + hi) `div` 2
        c = compare (mid ^ k) m
