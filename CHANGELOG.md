# [SpacemanDMM suite v1.12](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.12) (2026-09-20)

This update fixes parsing and analysis bugs, adds more missing 516 features,
and improves performance. Also, new sleep analysis modes collaborated on by
Cyberboss, CabinetOnFire, and ZeWaka are available.

## Core
Core updates apply to all components.

* Remove special handling of files named `tgstation.dme`. If needed, set `environment` in `SpacemanDMM.toml`.
* Parse for-key-value loops (by ZeWaka, #447, and LemonInTheDark, #469).
* Fix crash on out-of-range bitshifts in constant evaluation.
* Fix bugs with `##` token pasting and empty arguments:
  * Fix identifiers on the left-hand side of `##` being incorrectly dropped (by harry, #471).
  * Fix commas on the left-hand side of `##` being incorrectly preserved (#411, #192).
* Fix preprocessor `fexists` giving inaccurate results on `.dme` paths outside the working directory (by ZeWaka, #478).
* Mark `vector()` as a constant constructor (by LemonInTheDark, #468).
* Add builtins:
  * `/icon/proc/RscFile` and `/sound/proc/RscFile` (#416).
  * `/world/var/process` (by forgman6, #456).
* Gracefully resume parsing after previously fatal tree-scope parsing errors:
  * `/var/1 = 1`, such as from `/var/OPEN = 1` where `OPEN` is a macro.
  * Tree-scope `..()`, which has no effect.
  * Tree-scope empty blocks, such as from indented block comments, which have no effect.
* Improve accuracy of "check for extra indentation" note location on certain rare parse errors.
* Optimize parsing by interning keywords and other builtin strings.

## Language Server

* Add hover and go-to-definition for `/typepath::variable` syntax (by ZeWaka, #477).
* Add hover for `__FILE__` and `__LINE__` (by Cyberboss, #358).
* Fix go-to-definition failing on variables named `list`, `alist`, `input`, `locate`, and `pick`.
* Optimize the object tree browser and enable it by default.
* Distinguish constants, constructors, and methods in the object tree browser.

### Debugger

* Update Auxtools debug server from [v2.3.5 to v2.3.8](https://github.com/willox/auxtools/compare/v2.3.5...v2.3.8).
* Change the default debug engine to Auxtools. Extools is unmaintained since 2021 and support will be removed in a future release.

## DreamChecker

* Fix crash on `switch(rand(..))` containing `if(x to INFINITY)`.
* Fix undefined vars and procs not being caught if a var or proc with the same name exists globally.
* Fix the output type of `operator[]` calls being the same as the input type (#480).
* Improve "unreachable code" warnings to handle `continue` and `break` (by LemonInTheDark, #475).
* Add `SpacemanDMM_should_not_call_parent` opt-in warning (by Lucy, #455).
* Add new `SpacemanDMM_should_not_sleep` modes, configured by setting `dreamchecker.sleep_analysis_version`:
  * `sleep_analysis_version = 1` is the old default.
  * `sleep_analysis_version = 2` catches many more cases but has false positives (by CabinetOnFire and Cyberboss, #472).
  * `sleep_analysis_version = 3` catches a few more cases than `1` but has many fewer false positives than `2`, and is the new default (by ZeWaka, #479).
* Add flag definitions for `filter(type="displace")` (by Krashly, #445).
* Add warning for using the indexing operator on types without `proc/operator[]` (by LemonInTheDark, #397).
* Add warning for `list.Find()` with no arguments (by ZeWaka, #460).

## dmm-tools

* Fix loading `.dmi` icon states with backslashes or quotes in their name, such as in fonts.
* Add `icon-smoothing-2025` render pass to support tgstation's newest system (by Lucy, #454).
* Optimize blitting somewhat.

# [SpacemanDMM suite v1.11](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.11) (2025-08-30)

This maintenance update improves SpacemanDMM's support for BYOND 516, thanks to ShiftyRail, Zonespace, and other contributors.

## Core

Core updates apply to all components.

* Add support for `call_ext(LoadedFunc)` syntax (by Lucy, #433).
* Add support for `for (var/k, v in X) syntax` (by ShiftyRail, #431).
* Add missing `/pixloc/var/x` and `y` (by ZeWaka, #441).
* Remove warning for using a macro too close to undefining it, as it is now irrelevant (#423).
* Add a field list to the unnamed type used by `/callee/var/proc`, so its vars can be checked.

## Language Server

* Add `caller` and `callee` vars to the completions list.

## dreamchecker

* Lint for `set SpacemanDMM_` statements that have no effect (by Drathek, #435).
* Fix `/alist`s not being considered iterable (by Zonespace, #442).
* Fix `filter()` not accepting `name()` as a keyword argument (by Zonespace, #436).

## dmm-tools

* Simplify some internals and improve sprite clipping performance.

# [SpacemanDMM suite v1.10](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.10) (2025-05-11)

SpacemanDMM now supports most BYOND 516 features, with many thanks to harry and LemonInTheDark.

## Core

Core updates apply to all components.

* Add definitions for new types, vars, and procs for BYOND 516 (by harry, #420).
* Add `<=>` operator (by harry, #425).
* Add support for `as movable`, `as atom`, and `as list` (#399).
* Allow `operator` in type names again (#374).
* Add `pixel_w` and `pixel_z` to `image()` (by Waterpig, #424).
* Improve speed by changing the hasher used for HashMaps and HashSets (by ZeWaka, #414).
* Fix relative paths in preprocessor `fexists()` (by ZeWaka, #401).

## Language Server

* Fix document outline being inaccurate in some cases (#394).
* Fix a crash in the document outline (#395).

### Debugger

* Fix debugger not working if the configured DLL path override was set to the empty string (by Lucy, #428).
* Update auxtools debug server from v2.3.3 to v2.3.5.

## dreamchecker

* Handle `as` return types same as `set SpacemanDMM_return_type` types (#406).
* Fix configuration errors being silently ignored.

## dmdoc

* Add proc parameter `as` types, return `as` types, and `set SpacemanDMM_return_type` types to proc documentation.
* Add var `as` types to var documentation.
* Fix configuration errors being silently ignored.

# [SpacemanDMM suite v1.9](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.9) (2024-06-23)

A maintenance update with fixes to dmdoc, more BYOND 515 support, and improvements under the hood.

## Core

Core updates apply to all components.

* Implement `/proc/operator:=`
* Accept input types on bare variables like `var/foo as text|null`
* Support preprocessor `fexists()` (by ZeWaka, #388)
* Add `delay` argument to `animate()` (by Waterpig, #387)
* Overhaul doc comment parsing to fix some cases where comments would be dropped (#332)
* Fix Unicode being mangled in doc comments
* Fix parsing of union proc return types like `as num|text` (#385)

## Language Server

### Debugger

* Update auxtools debug server from v2.2.4 to v2.3.3
* Allow setting environment variables on DS/DD launch

## dmdoc

* Speed up rendering massively
* Fix Markdown headings rendering as `<hh2>` instead of `<h2>`

## DMM-Tools

* Fix the /tg/station gravity generator render pass (by mc-oofert, #393)

# [SpacemanDMM suite v1.8](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.8) (2023-10-29)

SpacemanDMM now supports most BYOND 515 features, with many thanks to LemonInTheDark and others.

## Core

Core updates apply to all components.

* Bump declared BYOND version to 515.1619
* Add `call_ext` (by willox, #353)
* Add v515 proc and constant builtins (by Spookerton, #354)
* Fix `operator/` and `operator/=`
* Add `%%`, `%%=`, and `operator""` (by LemonInTheDark, #371)
* Add `__TYPE__` and `__PROC__` (by LemonInTheDark, #366)
* Add `__IMPLIED_TYPE__` (by LemonInTheDark, #368)
* Add `#pragma multiple` (by LemonInTheDark, #372)
* Add `proc/final`, connected to existing "should not override" lint (by LemonInTheDark, #369)
* Add `::` scope operator (by LemonInTheDark, #367)
* Add constant-evaluation for `nameof()`

## Language Server

* Handle `::`, `__TYPE__`, `__PROC__`, and `__IMPLIED_TYPE__` in "Find All References"

### Debugger

* Update auxtools debug server from v2.2.3 to v2.2.4, adding support for BYOND 515.1606

## DreamChecker

* Handle `type::somevarname` so uses do not give type errors

# [SpacemanDMM suite v1.7.3](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.7.3) (2023-02-01)

Another maintenance release with minor additions and fixes.

Component version numbers have been synchronized at v1.7.3 instead of being versioned separately.

## Core

Core updates apply to all components.

* Fail gracefully instead of crashing when encountering bad .dmi files (by moxian, #319)
* Fix parsing heredocs with multiple quotes in a row
* Add some undocumented builtins for completeness:
  * `/dm_filter` type (by Spookerton, #312)
  * `/database/var/_binobj` and `/database/query/var/database` vars (by Cyberboss, #326)
  * `/generator/var/_binobj` var (by TiviPlus, #342)

## Language Server

* Add documentation on hover for macros
* Fix a possible crash if the environment loaded fast enough to cause rounding errors

### Debugger

* Update auxtools debug server from v2.2.2 to v2.2.3, adding support for BYOND 514.1584

## DreamChecker

* Add lints for the `switch(rand(L, H))` pattern (by pali, #302)
* Fix some proc overrides suppressing sleep/purity checks (by fira, #311)

## DMM-Tools

* Improve error locations in DMM parser

# [SpacemanDMM suite v1.7.2](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.7.2) (2021-12-18)

A small maintenance release, with performance and correctness improvements.

## DM Language
DM language updates apply to all components.
* Properly handle files with UTF-8 byte order mark (by PJB, #285)
* Add /particle/var/fadein builtin (by Azrun, #290)
* Fix `var_in_proc_parameter` diagnostic configuration not being properly applied (by igorsaux, #292)
* Use IndexMap and AHash to improve speed (by LatteKat, #293)
* Reorganize AST structures to reduce memory usage of the parser

## Language Server v1.5.1

* Fix `new variable.field()` syntax not being included in Find All References for `variable` or `field`
* Run DreamChecker and Find All References in the background in order to become responsive to queries sooner

# [SpacemanDMM suite v1.7.1](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.7.1) (2021-09-09)

This is a minor release primarily to resolve the dmdoc panic bug that exists in suite-1.7.

## DM Language
DM language updates apply to all components.
* Increased claimed BYOND version to 513.1556, by Watermelon914 (#273)

## dmdoc v1.4.1
* Fix crash if a type proc and global proc with the same name were both documented (#268)
* Set sans-serif fonts in dmdoc stylesheet, by LetterN (#278)

## DreamChecker v1.7.1
* Fix particles not actually being assumed to be particles by TiviPlus (#275)

# [SpacemanDMM suite v1.7](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.7) (2021-05-18)

This release's headline is BYOND 514 support, thanks largely to ZeWaka and spookydonut. Additionally, new configuration options have been added to dmdoc and new lints have been added to dreamchecker. Willox's work on the [auxtools] debug server has made debugging smoother and more interactive and will allow us to eventually deprecate extools.

## DM Language
DM language updates apply to all components.

- Improve memory usage of parser significantly, about 45% for /tg/station13 (#113).
- Add BYOND 514 entries to the builtins table (by ZeWaka, #240).
- Add constant-evaluation support for 514's expanded `rgb()` function (by ZeWaka, #254).
- Fix `new x[y]` being parsed incorrectly (by Willox, #257).
- Add support for infinite `for()` loops (by Willox, #259).
- Add support for `L?[x]` list access syntax (by Willox, #262).
- Fix `in` and ternary precedence (by Willox, #260).
- Support `CRASH()` with no arguments (#245).

## Language Server v1.5.0
- Track disk I/O separately from parsing in the load time report.

### Debugger
- Add [auxtools] debugging support (by Willox, #230).
  - Meant to eventually replace extools, which is no longer maintained.
  - Debug console allows evaluating expressions (by Willox, #263).
  - `#dis` and `#dis /type/proc/name` can be used to read disassembly (by Willox, #233).
- Show stdout/stderr of the launched process in the debug console.

[auxtools]: https://github.com/willox/auxtools

## dmdoc v1.4.0 <!-- was 1.3.0 -->
- Types, global procs, and global vars are now always included in the module tree as well.
- The module tree will start with the first level (usually "code") expanded.
- The `dmdoc.index_file` configuration option can be used to specify a file to be used as the documentation's index page.
- The `dmdoc.module_directories` option can be used to override the default module directory detection.
- Attempting to link to some builtins will now properly link to the DM reference (#215).

## DreamChecker v1.7.0 <!-- was 1.6.0 -->
- Fix "should not sleep" to catch `world.Export` and `world.Import` (by spookydonut, #216).
- Add warning for `return x` in spawn context (by pali6, #228).
- Fix flag values for filters (by ZeWaka, #243).
- Add detection of sleeping client procs (by spookydonut, #252).
- Fix missing detection of world `Import()`/`Export()` calls (by spookydonut, #252).
- Lint for ambiguous use of `!` on the left-hand side of a bitwise operation (by ZeWaka, #265).

## DMM-Tools v1.3.1 <!-- was 1.3.0 -->
- Speed up map rendering by approximately 9% (by PJB, #242).

# [SpacemanDMM suite v1.6](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.6) (2020-10-08)

This release stars many dmdoc improvements, especially around crosslinking. Updates to the /tg/ fancy map rendering, more robust debugger output, and DreamChecker bug fixes are also included.

## DM Language
DM language updates apply to all components.

- Add missing filter flags for the outline filter (by Neerti, #206)

## Language Server v1.4.0
- Add documentation to hovers, add hovers for var uses and proc calls (by Bobbahbrown, #210)
- Make type definitions count as an implementation rather than a reference
- Exclude `..()` parent calls from the references list
- Add document links for `#include` lines
- Add document links for `'resource'` expressions

### Debugger
- Rearrange variables pane so "Locals" is at the top
- Work around some variables being hidden if the same name is re-used within a proc
- Pull variable names from BYOND rather than using SpacemanDMM's parser
- Implement the "step out" feature
- Show the sleeping proc queue as threads in VSC

## dmdoc v1.3.0
- Search for module documentation in any `#include`d directory rather than just `code`
- Add `--index <filename.md>` flag to display a file on the docs index page
- Include `.txt` files as module documentation
- Strip `\proper` and `\improper` prefixes from names
- Promote `README` files to represent their parent directory
- Add detailed crosslink error information
- Allow crosslinking to modules by their source file name
- Add `--dry-run` flag to skip HTML output and exit with the number of crosslink errors

## DreamChecker v1.6.0
- Add lint for attempting to iterate over known non-list types
- Add lint for line comments ending in backslashes
- Remove error if a proc has both `should_not_sleep` and `set waitfor = 0` (by MrStonedOne, #211)
- Fix "relatively pathed type" warnings, which were broken by a previous update
- Populate errortype on more errors, to allow disabling them (by Leshana, #201)
- Fix sleep and purity lints not counting proc overrides (by spookydonut, #214)

## DMM-Tools v1.3.0
- Update renderer for /tg/ icon smooth refactor (twice)
- Update unary atmos icons for existence of layer 4
- Add relics to random render pass and fix kirbyplants

# [SpacemanDMM suite v1.5](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.5) (2020-07-11)

This release mainly improves performance, fixes bugs, and adds missing builtins.

## DM Language
DM language updates apply to all components.

- Improve I/O speed by buffering entire files during parsing.
- Speed up object tree traversal during parsing.
- Fix `;` in proc parameters causing parse failure (by Cyberboss, #176).
- Fix interpolated strings always being assumed to be truthy (#180).
- Fix precedence for bitwise operations (by mloc, #197).
- Improve builtins around `bounds` vars, `/image` vars (#184, #186).
- Add missing `opacity` var to `/mutable_appearance` (by spookydonut).
- Add the `PASS_MOUSE` builtin (by ZeWaka, #195).

## Language Server v1.3.0
- Add `environment` config option to choose which `.dme` file to load if several are present.
- Add expression and statement keywords to autocomplete list (#76).
- Fix "go to definition" not working for relative-pathed proc argument types (#190).
- Show full `#define` body in completion details.
- Format procpath variables in debugger.
- Add "Globals" alongside "Arguments" and "Locals" in debugger.
- Supply `stddef.dm` source when stepped into.

## dmdoc v1.2.2
- No specific changes.

## DreamChecker v1.5.0
- Emit error when proc is defined multiple times (by willox, #181).
- Add `--parse-only` flag which skips most lints.
- Fix sleep/purity checks failing to cross `parent_type` boundaries in some situations.
- Fix sleeping code under `spawn()` triggering "should not sleep" lints (by Cyberboss, #177).
- Fix false positive diagnostic on `PureProc().Foo()` calls (#188).

## DMM-Tools v1.2.0
- Remove `check` subcommand in favor of `dreamchecker --parse-only`.

# [SpacemanDMM suite v1.4](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.4) (2020-04-04)

This release stars more DreamChecker additions by spookydonut, with minor bugfixes elsewhere.

## DM Language
DM language updates apply to all components.
- Update language support to 513.1514 (by spookydonut).
- Fix location of `do while` condition (by spookydonut).
- Fix locations for if else arm and switch if arms (by spookydonut, #173).
- Add missing documented and undocumented `/icon` vars (by spookydonut).
- Fix case on `/client/proc/MeasureText()` arguments (by spookydonut).
- Add undocumented parameter `radius` for `animate()` (by spookydonut).
- Fix bad `WAVE_BOUNDED` check on ripple and wave filters (by Valtos, #171).

## Language Server v1.2.2
- Add "Find Implementations" for procs and vars and remove its results from "Find References".

## dmdoc v1.2.1
- No specific changes.

## DreamChecker v1.4.0
- Warn on weird values given to built-in set directives (by spookydonut, #168).
- Add lints for private/protected vars and procs (by spookydonut, #124).
- Add should-be-pure & should-not-sleep lints (by spookydonut, #132).
- Make local vars locally scoped (by spookydonut, #130).
- Add `filter()` flag checking (by spookydonut, #165).
- Add basic unreachable code detection (by spookydonut, #123).
- Add config options for constant eval condition errors (by spookydonut).

## DMM-Tools v1.1.2
- Fix `overlays` render pass causing "error loading icon" warnings if airlocks have no `overlays_file` var.

# [SpacemanDMM suite v1.3](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.3) (2020-02-18)

This release features parsing improvements and DreamChecker additions brought to you by spookydonut, as well as some improvements to dmdoc.

## DM Language
DM language updates apply to all components.
- Add various BYOND 513 beta builtin declarations.
- Fix incorrect locations of warnings in "else if" conditions (by spookydonut, #144).
- Readd override-precedes-definition handling (by spookydonut, #147).
- Fix regression on `defined()` preprocessing (by spookydonut, #152).
- Fix `new .()` not parsing correctly (by spookydonut, #153).
- Fix constant evaluation of expressions like `10 ** -1` (by spookydonut, #164).

## Language Server v1.2.1
- No specific changes.

## dmdoc v1.2.0
- Add option to disable using the `name` var in the type tree (by spookydonut, #149).
- Add links to parent vars and procs when documented (#93).
- Add automatic cross-link syntax (#92).
- Add deep-linking attributes to Markdown headings (#91).

## DreamChecker v1.3.0
- Add configuration option for `final_var` warnings (by spookydonut, #157).
- Warn about invalid `filter()` keyword arguments (by spookydonut, #154).
- Add off-by-default lint to forbid relative pathing (by spookydonut, #150).
- Add static return types for built-in global procs (by spookydonut, #128).

## DMM-Tools v1.1.1
- No specific changes.

# [SpacemanDMM suite v1.2](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.2) (2020-01-13)

Highlights of this release are BYOND 513 builtins, a debugger, a faster map rendering backend, and many dreamchecker additions by spookydonut.

## DM Language
DM language updates apply to all components.
- Fix `#include` resolution priority when there are multiple candidates (#111).
- Fix duplicate proc entries for New, Del, and Topic on some types.
- Add BYOND 513 builtins (by spookydonut, #116).
  - Add /sound/var/len (by yoyobatty, #140).
- Add constant evaluation for trig functions (#134).
- Add extra help for indentation-related `got '{'` errors (#117).

## Language Server v1.2.0
- Add helper routines for "build & run" in the Visual Studio Code extension.
- Add [debug adapter] support for codebases configured with [extools], in collaboration with Asd.
- Add document color support, showing previews and pickers for DM color codes.
- Include parameter default values in "find all references" (by Antur, #137).
- Decreased startup time by loading the object tree panel asynchronously.
- Add configuration option to run dreamchecker automatically.

## dmdoc v1.1.1
- No specific changes.

## DreamChecker v1.2.0
- Add proc setting to warn if it is overridden by subtypes (by spookydonut, #114).
- Add "final" vars, which warn if they are overridden (by spookydonut, #115).
- Warn on `++` or `--` on variables with a declared type (by spookydonut, #119).
- Warn on `!a in b`, `a && b in c`, etc. (by spookydonut, #121).
- Add ability to configure or disable some diagnostics (by spookydonut, #120).

## DMM-Tools v1.1.0
- Rewrite most of the internals of the map renderer, improving performance and flexibility.
- Add render passes for fancy layers and icon smoothing, allowing them to be disabled.
- Add fancy rendering for smart cables (#107).

[debug adapter]: https://microsoft.github.io/debug-adapter-protocol/overview
[extools]: https://github.com/MCHSL/extools

# [SpacemanDMM suite v1.1](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.1) (2019-11-08)

Bugfix and maintenance release.

## DM Language
DM language updates apply to all components.
- Add known values to built-in `/atom` variables (by SpaiR, #89).
- Properly handle `#include`ing a secondary `.dme` file.
- Attempt to parse strings as UTF-8 before falling back to Latin-1 (preparation for BYOND 513).
- Entirely skip constant evaluation on a fatal parser error, to make fatal parser errors easier to spot (#110).
- Fix `##` token-pasting operator not handling integer right-hand arguments (#109).
- Fix `/list`, `/savefile`, `/client`, and `/world` inheriting from `/datum` (#98).

## Language Server v1.1.0
- Include var and proc documentation in completion results.
- Remove some unnecessary debug logging.
- Fix a crash when adding `/*` comments in the global scope (#105).
- Fix proc header hover information repeating the proc name twice.
- Mark language server diagnostics as originating from "dm-langserver", to differentiate them from DM errors.

## dmdoc v1.1.0
- Add a basic dark theme (by PJB, #102).
- Add command-line options:
  - `-e <filename>` to specify environment to parse.
  - `--output <dirname>` to specify output directory (default is `dmdoc`).
  - `--modules <dirname>` to specify directory to scan for Markdown files (default is `code`).
- Fix `/*!` comments including the `!` in the output (#90).

## DreamChecker v1.1.0
- Add `-e <filename>` command-line option to specify environment to parse.
- Handle `set SpacemanDMM_should_call_parent = TRUE` and `FALSE` properly, rather than `1` and `0` only (#106).

## DMM-Tools v1.0.1
- No specific changes.

# [SpacemanDMM suite v1.0](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/suite-1.0) (2019-07-03)

This is the 1.0 release of the SpacemanDMM tooling suite for DreamMaker codebases, consisting of:

* dreamchecker 1.0.0, a static analysis tool suitable for CI use.
* dmdoc 1.0.0, an HTML documentation generator.
* dmm-tools 1.0.0, a map renderer.
* dm-langserver 1.0.0, a language smartness provider.

# [dmm-tools v0.1.1](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/cli-v0.1.1) (2018-05-26)

Additions
* When a `.dme` file is not specified, an attempt is made to autodetect one.
* The `__FILE__` and `__LINE__` macros are now expanded.
* `#if` and `#elif` directives are now actually evaluated.
* The `defined()` form is now recognized in preprocessor conditionals.
* The `parent_type` var is now handled properly.
* The `^` and `**` operators are now evaluated in constants.
* Built-in vars for `/image` are now recognized.
* [Buggy uses of macros in embedded expressions](https://secure.byond.com/forum/?post=2072419) are now linted against.

Tweaks
* The [lodepng](https://github.com/kornelski/lodepng-rust) library is now used to read `.dmi` files in one pass, improving speed.
* Map read errors now include semi-accurate line numbers rather than no line numbers.
* If parsing aborts, only the first "undefined var" error is shown.

Fixes
* Atoms which have been pixel-shifted entirely offscreen no longer crash.
* Numbers of the from `2e6-1` are no longer misinterpreted.
* The default values of `layer` for each atom type are now recognized.
* Invalid values for `layer` no longer crash.
* Indented `#include` directives no longer indent the first line of the file they include.
* Chaining multiple `#else` or `#elif` in a row no longer activates more than one.

# [dmm-tools v0.1.0](https://github.com/SpaceManiac/SpacemanDMM/releases/tag/cli-v0.1.0) (2018-04-15)

First binary release of the `dmm-tools` CLI, providing BYOND map rendering and analysis tools powered by SpacemanDMM.

Subcommands:
*    `diff-maps <left> <right>`:      List the differing coordinates between two maps.
*    `lint-maps [-n] [--reformat] <maps...>`:      Lint and automatically fix the specified maps.
*    `list-passes [-j]`:    Show information about the render-pass list.
*    `map-info [-j] <files...>`:       Show metadata information about the map.
* `minimap`:        Build minimaps of the specified maps.
  * `[--disable pass-1,pass-2,...] [--enable pass-1,pass-2,...]`: Disable or enable render-passes.
  * `[--max x,y[,z]] [--min x,y[,z]]`: Set the bounding cuboid to act upon.
  * `[--optipng] [--pngcrush]`: Run output through a PNG optimizer automatically.
  * `[-o output]`: Set the output directory.
  * `<files...>`

Default render passes:
* `hide-space`: Do not render space tiles, instead leaving transparency.
* `hide-areas`: Do not render area icons.
* `hide-invisible`: Do not render invisible or ephemeral objects such as mapping helpers.
* `random`: Replace random spawners with one of their possibilities.
* `pretty`: Add the minor cosmetic overlays for various objects.
* `spawners`: Replace object spawners with their spawned objects.
* `fake-glass`: Add underlays to fake glass turfs.
* `transit-tube`: Add overlays to connect transit tubes together.
* `gravity-gen`: Expand the gravity generator to the full structure.

Additional render passes:
* `only-powernet`: Render only power cables.
* `only-pipenet`: Render only atmospherics pipes.
