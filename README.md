# SpacemanDMM

**SpacemanDMM** is a suite of tools for working with [DreamMaker] codebases
and map files. Most prominently it features a map renderer which emulates many
in-game graphical enhancements not usually visible in the map editor, including
overlays and smoothing. These features currently assume [/tg/station13], but
are highly likely to work on downstreams or closely related codebases.

[DreamMaker]: https://secure.byond.com/
[/tg/station13]: https://github.com/tgstation/tgstation/

## Installation

Binary releases for Linux and Windows are available on the [releases] page, but
may not always be up to date.

To build locally, begin by [installing Rust][rust] or updating your existing
installation. SpacemanDMM is tested against stable Rust.

For one-time installation, run `cargo install --git https://github.com/SpaceManiac/SpacemanDMM cli`
to install the `dmm-tools` binary to `~/.cargo/bin`.

For development or easy installation of updates, clone this repository and run
`cargo build -p cli --release` to create `target/release/dmm-tools`. See the
[source readme] for a list of the available packages, or omit `--release` for a
debug build.

[releases]: https://github.com/SpaceManiac/SpacemanDMM/releases
[rust]: https://www.rust-lang.org/en-US/install.html
[source readme]: ./src/README.md

## Usage

Executables are placed in `target/release` or `target/debug` depending on build
type. The CLI binary is known as `dmm-tools`. For best results, run from the
directory containing `tgstation.dme`.

Basic usage: `dmm-tools minimap _maps/map_files/BoxStation/BoxStation.dmm`.

By default, output is saved to `data/minimaps`, which can be changed with the
`-o` flag. The `-e` flag, specified before the subcommand, can be used to load
a different `.dme` file. More detailed usage information is available in the
`--help` output.

Support is currently provided in /tg/station13's coderbus (ping `SpaceManiac`)
and on GitHub's issue tracker. Pull requests are welcome.

## Language Server

SpacemanDMM also provides a [language server] providing intelligence for the
DreamMaker language. A [Visual Studio Code plugin][vsc] is available, and will
install released language server binaries automatically. Details on available
features are listed in the language server's [package readme][ls-readme].

Use `cargo build -p dm-langserver` to build a local copy of the language server
for debugging or development purposes. The plugin can be configured to run a
locally built language server rather than the binary releases.

[language server]: https://langserver.org/
[vsc]: https://marketplace.visualstudio.com/items?itemName=platymuus.dm-langclient
[ls-readme]: ./src/langserver/README.md

## Recommendations

The minimap output is a very large PNG (e.g. 9.3 MB for Box). You are strongly
advised to run the resulting file through image optimization software such as
`pngcrush`. The `--pngcrush` option to the `minimap` subcommand can do this
automatically in many cases, but is off by default for speed reasons.
