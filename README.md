# Symtegration

This is a Haskell library intended to support symbolic integration of mathematical expressions.

It offers the following:

*   TODO: Symbolic integration of mathematical expressions.

*   Symbolic representation of mathematical expressions.

*   Utility functions to make it easier to read the mathematical expressions.
    For example, deriving equivalent Haskell code for a mathematical expression,
    and rudimentary support to simplify the symbolic representation.

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
