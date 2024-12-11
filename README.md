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
>>> toHaskell <$> integrate "x" (4 * "x" ** 3 + 1)
Just "(x ** 4) + x"
>>> toHaskell <$> integrate "z" ("x" * "z" + "y")
Just "(x / 2) * (z ** 2) + y * z"
```

Concrete numbers can also be computed from these integrals.  For example:

```haskell
>>> import Symtegration
>>> let (Just p) = integrate "x" (4 * "x" ** 3 + 1)
>>> fractionalEvaluate p (\case "x" -> Just (3 / 7 :: Rational))
Just (1110 % 2401)
```

### Symbolic integration in GHCi

With Symtegration, symbolic integration can be done within [GHCi].
When executing GHCi within the Symtegration project, it is best
to load only the `Symtegration` module to avoid name collisions,
so start GHCi without loading any modules.

```shell
$ stack ghci --no-load
```

Within GHCi, explicitly load the `Symtegration` module.
You can then proceed to symbolically integrate mathematical expressions
and compute approximate or exact values from these integrals.

```haskell
>>> :load Symtegration
>>> toHaskell <$> integrate "x" ("a" * "x" ** 4 + "x" + "b")
Just "(a / 5) * (x ** 5) + (1 / 2) * (x ** 2) + b * x"
>>>
>>> let (Just p) = integrate "x" ("x" ** 2)
>>> evaluate p (\case "x" -> Just 1)
Just 0.3333333333333333
>>>
>>> fractionalEvaluate p (\case "x" -> Just (1 :: Rational))
Just (1 % 3)
```

[GHCi]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html

## Differentiation

This library does not offer symbolic differentiation on its own.
Symbolic derivatives can be obtained using [automatic differentiation]
on the symbolic represenation of a mathematical expression.

For example,

```haskell
>>> import Numeric.AD
>>> diff (\x -> x + 1) ("x" :: Expression)
Number 1
>>> toHaskell $ simplify $ diff (\x -> x ** 3 + 1) ("x" :: Expression)
"3 * (x ** 2)"
```

[automatic differentiation]: https://hackage.haskell.org/package/ad

## Code of conduct

Be nice; see [`CODE_OF_CONDUCT.md`] for details.

[`CODE_OF_CONDUCT.md`]: docs/CODE_OF_CONDUCT.md

## Security policy

See [`SECURITY.md`] for details.

[`SECURITY.md`]: docs/SECURITY.md

## License

Apache 2.0; see [`LICENSE`] for details.

[`LICENSE`]: LICENSE
