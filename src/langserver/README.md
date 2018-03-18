# DreamMaker Language Server

This package is a [language server] implementation for DreamMaker, the
scripting language of the [BYOND] game engine.

* [VS Code extension](https://marketplace.visualstudio.com/items?itemName=platymuus.dm-langclient)

[language server]: https://langserver.org/
[BYOND]: https://secure.byond.com/

## Workspace symbol search

* Searches macros, types, procs, and vars.
* Prefix query with `#` to search macros only, `var/` to search vars only, or
  `proc/` to search procs only.
* Include `/` in query to search types only.
