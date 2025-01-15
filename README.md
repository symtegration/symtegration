# Symtegration

This is a Haskell library intended to support symbolic integration of mathematical expressions.

It offers the following:

*   Symbolic integration of mathematical expressions.

    *   Integration of polynomials.

    *   Integration of trigonometric functions.

    *   Integration of exponential and logarithmic functions.

    *   Partial support for integrating ratios of two polynomials.

    *   Integration by substitution.

*   Symbolic representation of mathematical expressions.

*   Utility functions to make it easier to read the mathematical expressions.
    For example, deriving equivalent Haskell code for a mathematical expression,
    and some support for simplifying symbolic representations.

[![Build](https://github.com/symtegration/symtegration/actions/workflows/build.yaml/badge.svg)](https://github.com/symtegration/symtegration/actions/workflows/build.yaml)
[![OpenSSF Best Practices](https://www.bestpractices.dev/projects/9864/badge)](https://www.bestpractices.dev/projects/9864)
[![OpenSSF Scorecard](https://api.scorecard.dev/projects/github.com/symtegration/symtegration/badge)](https://scorecard.dev/viewer/?uri=github.com/symtegration/symtegration)
[![codecov](https://codecov.io/gh/symtegration/symtegration/graph/badge.svg?token=CNBUMA1CKD)](https://codecov.io/gh/symtegration/symtegration)

## Integration

Mathematical expressions with either numeric coefficients or symbolic coefficients
can be integrated.  For example:

```haskell
>>> import Symtegration
>>> toHaskell <$> integrate "x" (4 * "x" ** 3 + 1)
Just "x + x ** 4"
>>> toHaskell <$> integrate "z" ("x" * "z" + "y")
Just "y * z + 1 / 2 * x * z ** 2"
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
Just "b * x + 1 / 2 * x ** 2 + a * (x ** 5) / 5"
>>>
>>> let (Just p) = integrate "x" ("x" ** 2)
>>> evaluate p (\case "x" -> Just 1)
Just 0.3333333333333333
>>>
>>> fractionalEvaluate p (\case "x" -> Just (1 :: Rational))
Just (1 % 3)
```

[GHCi]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html

### Symbolic integration in IHaskell

Symtegration can also be used in [IHaskell] to do symbolic integration.
Its use can be seen in an [example IHaskell notebook],
which you try out by [running on mybinder.org].

[IHaskell]: https://github.com/IHaskell/IHaskell
[example IHaskell notebook]: https://github.com/chungyc/haskell-notebooks/blob/main/Symtegration.ipynb
[running on mybinder.org]: https://mybinder.org/v2/gh/chungyc/ihaskell/custom?urlpath=git-pull%3Frepo%3Dhttps%253A%252F%252Fgithub.com%252Fchungyc%252Fhaskell-notebooks%26urlpath%3Dlab%252Ftree%252Fhaskell-notebooks%252FSymtegration.ipynb%26branch%3Dmain

## Changes

See [`CHANGELOG.md`] for what has changed.

[`CHANGELOG.md`]: docs/CHANGELOG.md

## Code of conduct

Be nice; see [`CODE_OF_CONDUCT.md`] for details.

[`CODE_OF_CONDUCT.md`]: docs/CODE_OF_CONDUCT.md

## Security policy

See [`SECURITY.md`] for details.

[`SECURITY.md`]: docs/SECURITY.md

## Contributing

See [`CONTRIBUTING.md`] for details.

[`CONTRIBUTING.md`]: docs/CONTRIBUTING.md

## License

Apache 2.0; see [`LICENSE`] for details.

[`LICENSE`]: LICENSE
