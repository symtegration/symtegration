# Changelog for `symtegration`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

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
