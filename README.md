# Symtegration

This is a Haskell library intended to support symbolic integration of mathematical expressions.

It offers the following:

*   Symbolic integration of mathematical expressions.

    *   Currently, only simple integration of polynomials is supported.

*   Symbolic representation of mathematical expressions.

*   Utility functions to make it easier to read the mathematical expressions.
    For example, deriving equivalent Haskell code for a mathematical expression,
    and rudimentary support to simplify the symbolic representation.

## Integration

Mathematical expressions with either numeric coefficients or symbolic coefficients
can be integrated.  For example:

```haskell
>>> import Symtegration
>>> toHaskellText <$> integrate "x" (4 * "x" ** 3 + 1)
Just "(x ** 4) + x"
>>> toHaskellText <$> integrate "z" ("x" * "z" + "y")
Just "((x / 2) * (z ** 2)) + (y * z)"
```

Concrete numbers can also be computed from these integrals.  For example:

```haskell
>>> import Symtegration
>>> import Data.Map qualified as Map
>>> let assignments = Map.singleton "x" (3 / 7 :: Rational)
>>> let (Just p) = integrate "x" (4 * "x" ** 3 + 1)
>>> fractionalEvaluate p assignments
Just (1110 % 2401)
```

## Differentiation

This library does not offer symbolic differentiation on its own.
Symbolic derivatives can be obtained using [automatic differentiation]
on the symbolic represenation of a mathematical expression.

For example,

```haskell
>>> import Numeric.AD
>>> diff (\x -> x + 1) ("x" :: Expression)
Number 1
>>> toHaskellText $ simplify $ diff (\x -> x ** 3 + 1) ("x" :: Expression)
"3 * (x ** 2)"
```

[automatic differentiation]: https://hackage.haskell.org/package/ad
