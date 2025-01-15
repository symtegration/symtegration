# Changelog for `symtegration`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

*   Attempt integration by substitution after factoring out constant factors.

## 0.4.0 - 2025-01-14

*   Integrate more rational functions.

    *   Find all real roots for integration involving solution of cubic equations.

    *   For integration involving solution of quartic equations,
        find real roots for more special cases.

*   Cancel out common integer factors in fractions.

*   Fewer parentheses in Haskell code output.

*   Fewer parentheses in LaTeX output.

*   Test with GHC 9.12.1.

## 0.3.0 - 2025-01-05

*   Implementation of Rioboo's algorithm.

    *   Supports integration of more rational functions.

    *   Integration of rational functions with rational number coefficients now
        only limited by finding solutions for polynomials.  As of yet, only
        rational functions which require solutions for polymials up to degree 2
        can be integrated.

*   Output `pi` as `\pi` in LaTeX.

## 0.2.0 - 2025-01-02

*   Integration of rational functions.

    *   Hermite reduction.

    *   Lazard-Rioboo-Trager integration.

*   Improvements to LaTeX output.

*   Remove simplification based on recursive heuristics,
    which were much more ad hoc.

*   Make `foldTerms` order consistent with simplification order,
    from lower to higher terms.

## 0.1.0 - 2024-12-24

*   Symbolic representation.

*   Simplification.

*   Basic integration support.

    *   Integration of polynomials.

    *   Integration of trigonometric functions.

    *   Integration of exponential and logarithmic functions.

    *   Integration by substitution.
