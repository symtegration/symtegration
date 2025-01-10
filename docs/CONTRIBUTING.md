# How to contribute

## Before you begin

### Review community guidelines

This project follows the [Contributor Covenant Code of Conduct].

[Contributor Covenant Code of Conduct]: CODE_OF_CONDUCT.md

### Review license

Any contributions are to be licensed under the [Apache-2.0 license].
Review the license to determine whether you are willing to license
any contributions under the same license.

[Apache-2.0 license]: ../LICENSE

### Background material

_[Symbolic Integration I: Transcendental Functions]_ by Manuel Bronstein
is a primary reference for the algorithms used by this project.

[Symbolic Integration I: Transcendental Functions]: https://doi.org/10.1007/b138171

## Contribution process

### Code reviews

All external contributions require review.
[GitHub pull requests] are used for this purpose.

[GitHub pull requests]: https://docs.github.com/en/pull-requests

### Coding standards

Code should be pure to the extent possible, and partial functions should be avoided.
User-visible entities should be documented with [Haddock], including examples if feasible.
[HLint] should report no issues, and formatting should be according to [Ormolu].

All changes should be accompanied by corresponding tests.
Code should be tested with property-based tests to the extent possible.
This project uses [Hspec] and [QuickCheck] for testing.
Examples in the Haddock documentation are tested using [`doctest-parallel`].

[Haddock]: https://haskell-haddock.readthedocs.io/
[HLint]: https://github.com/ndmitchell/hlint
[Ormolu]: https://github.com/tweag/ormolu
[Hspec]: https://hspec.github.io/
[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[`doctest-parallel`]: https://github.com/martijnbastiaan/doctest-parallel

### Dependencies

This project aims to avoid using too many heavy dependencies.
Care should be taken not to add dependencies casually.
If the same thing can be done with some additional code in the project,
then adding a dependency should be avoided.
This is more important the more heavy a dependency is or the less maintained it is.

### Releases

When releasing, these files should be updated:

*   [`CHANGELOG.md`] with user-visible changes.

*   [`package.yaml`] with the new version.  There should be at least one
    subsequent `stack build` to update [`symtegration.cabal`] as well.

Versioning is based on [semantic versioning] and the [Haskell package versioning policy].
When there are differences between the two policies, the latter takes precedence.

Lower version bounds for dependencies should be verified by setting the versions
to the lowest minor versions in the Cabal configuration and checking that
builds and tests are still successful.  These changes to the Cabal configuration
are only for confirming that the lower bounds are still valid, and should not
be submitted to the repository.

[`CHANGELOG.md`]: CHANGELOG.md
[`package.yaml`]: ../package.yaml
[`symtegration.cabal`]: ../symtegration.cabal
[semantic versioning]: https://semver.org/
[Haskell package versioning policy]: https://pvp.haskell.org/
