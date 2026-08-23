# DreamMaker Language Server

This package is a [language server] implementation for DreamMaker, the
scripting language of the [BYOND] game engine. All features are dependent on
client support.

* [Visual Studio Code extension](https://marketplace.visualstudio.com/items?itemName=platymuus.dm-langclient)
  ([code](https://github.com/SpaceManiac/vscode-dm-langclient)).
* [Sublime Text 3 package](https://packagecontrol.io/packages/DreamMaker%20Language%20Client)
  ([code](https://github.com/SpaceManiac/sublime-dm-langclient)).
* Other editors may have generic language client packages which will be
  compatible.

[language server]: https://langserver.org/
[BYOND]: https://www.byond.com/

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

* All [parsing suite] diagnostics.
* Optional [DreamChecker] diagnostics.

[parsing suite]: ../dreammaker/#diagnostics
[DreamChecker]: ../dreamchecker/#diagnostics

## Signature help

* Gives proc argument help, including for builtin procs, when a `(` is typed or
  on command.

## Document symbols

* Provides an "outline" view of symbols in the current file.
