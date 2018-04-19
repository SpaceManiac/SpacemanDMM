# DreamMaker Language Server

This package is a [language server] implementation for DreamMaker, the
scripting language of the [BYOND] game engine.

* [VS Code extension](https://marketplace.visualstudio.com/items?itemName=platymuus.dm-langclient)

[language server]: https://langserver.org/
[BYOND]: https://secure.byond.com/

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

## Workspace symbol search

* Searches macros, types, procs, and vars.
* Prefix query with `#` to search macros only, `var/` to search vars only, or
  `proc/` to search procs only.
* Include `/` in query to search types only.
