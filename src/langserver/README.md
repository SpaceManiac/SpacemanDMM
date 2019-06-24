# DreamMaker Language Server

This package is a [language server] implementation for DreamMaker, the
scripting language of the [BYOND] game engine.

* [VS Code extension](https://marketplace.visualstudio.com/items?itemName=platymuus.dm-langclient)

[language server]: https://langserver.org/
[BYOND]: https://secure.byond.com/

## Code completion

* Completes names of typepaths, procs, type vars, local vars, and macros.
* Popup includes symbol type as well as the value of constants.
* Completes var and proc overrides in type definitions.
  * Proc overrides include a stub which calls `..()`.

## Hover

* Shows inheritance information when hovering proc headers and type vars.

## Go to definition

* Finds the definition of typepaths, procs, type vars, local vars, and macros.
* In some clients, integrates with DM Reference browser.

## Workspace symbol search

* Searches macros, types, procs, and vars.
* Prefix query with `#` to search macros only, `var/` to search vars only, or
  `proc/` to search procs only.
* Include `/` in query to search types only.
* In some clients, integrates with DM Reference browser.

## Find all references

* Finds all uses of:
  * Typepaths as a literal.
  * Procs, called and overridden.
  * Type vars, read, written, and overridden.

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

## Signature help

* Gives proc argument help, including for builtin procs, when a `(` is typed or
  on command.

## Document symbols

* Provides an "outline" view of symbols in the current file.
