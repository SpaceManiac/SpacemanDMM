# SpacemanDMM

**SpacemanDMM** is a suite of tools for working with [DreamMaker] codebases
and map files. It features a full-size fancy map renderer, a [language server],
and a documentation generator for DM codebases.

Binary releases are posted to the [releases] page.

Support is currently provided in /tg/station13's coderbus (ping `SpaceManiac`)
and on GitHub's issue tracker. Pull requests are welcome.

[DreamMaker]: https://secure.byond.com/
[/tg/station13]: https://github.com/tgstation/tgstation/
[releases]: https://github.com/SpaceManiac/SpacemanDMM/releases

## [Map Renderer](src/cli/)

`dmm-tools` is SpacemanDMM's map renderer. It generates full-sized map images
and emulates many in-game graphical enhancements not usually visible in the map
editor, including overlays and smoothing. These features currently assume
[/tg/station13], but are highly likely to work on downstreams or closely
related codebases.

Basic usage: `dmm-tools minimap _maps/map_files/BoxStation/BoxStation.dmm`.
For best results, run from the directory containing your `.dme` file.

By default, output is saved to `data/minimaps`, which can be changed with the
`-o` flag. The `-e` flag, specified before the subcommand, can be used to load
a different `.dme` file. More detailed usage information is available in the
`--help` output.

The minimap output is a very large PNG (e.g. 9.3 MB for Box). You are strongly
advised to run the resulting file through image optimization software such as
`pngcrush`. The `--pngcrush` option to the `minimap` subcommand can do this
automatically in many cases, but is off by default for speed reasons.

## [Language Server](src/langserver/)

SpacemanDMM includes a [language server] providing autocomplete,
go-to-definition, and more for the DreamMaker language. The preferred
installation method is the [Visual Studio Code plugin][vsc], which will update
with newly-released language server binaries automatically. Details on
available features are listed in the language server's
[package readme][ls-readme].

The VS Code plugin is currently hosted in a [separate repository][vsc-src].

Use `cargo build -p dm-langserver` to build a local copy of the language server
for debugging or development purposes. The plugin can be configured to run a
locally built language server rather than the binary releases.

[language server]: https://langserver.org/
[vsc]: https://marketplace.visualstudio.com/items?itemName=platymuus.dm-langclient
[ls-readme]: ./src/langserver/README.md
[vsc-src]: https://github.com/SpaceManiac/vscode-dm-langclient

## [Documentation Generator](src/dmdoc/)

`dmdoc` is a simple Doxygen-esque documentation generator for DreamMaker code.
Files, macros, types, vars, and procs can be documented. Documentation comments
start with `/**` or `///` when preceding the documented item, or `/*!` or `//!`
when contained within it.

By default, only documented types are included in the output, and types with
only a one-line note do not get their own `.html` page. The index shows the
contents of `code/README.md`, and other Markdown files within the `code`
directory are shown alongside documented DM files.

If run in a Git repository, web links to source code are placed next to item
headings in the generated output; otherwise, file and line numbers are shown
but are not linked.

## Building

To build locally, begin by [installing Rust][rust] or updating your existing
installation. SpacemanDMM is tested against stable Rust.

For one-time installation, run
`cargo install --git https://github.com/SpaceManiac/SpacemanDMM cli`
to install the `dmm-tools` binary to `~/.cargo/bin`.

For development or easy installation of updates, clone this repository and run
`cargo build -p cli --release` to create `target/release/dmm-tools`. See the
[source readme] for a list of the available packages, or omit `--release` for
a debug build.

Executables are placed in `target/release` or `target/debug` depending on build
type. The CLI binary is known as `dmm-tools`.

[rust]: https://www.rust-lang.org/en-US/install.html
[source readme]: ./src/README.md

## License

SpacemanDMM is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SpacemanDMM is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with SpacemanDMM. If not, see http://www.gnu.org/licenses/.

