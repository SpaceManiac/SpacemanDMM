# `dreammaker` library

This library crate implements a parser for the DreamMaker language. It is a
core component of SpacemanDMM and powers the rest of the tooling.

## Diagnostics

* Preprocessor:
  * Redefining a macro which is already defined.
  * Undefining a macro which is not defined.
  * Unterminated conditional blocks (`#if`/`#ifdef`).
  * Using a define in a [buggy context][2072419].
* Language:
  * Incorrect or correct-but-strange syntax.
  * Non-constant initial values for object variables.
  * Integer constants which are outside of range.

[2072419]: https://secure.byond.com/forum/?post=2072419