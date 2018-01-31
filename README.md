# SpacemanDMM

**SpacemanDMM** is a suite of tools for working with [DreamMaker] codebases
and map files. Most prominently it features a map renderer which emulates many
in-game graphical enhancements not usually visible in the map editor, including
overlays and smoothing. These features currently assume [/tg/station13], but
are highly likely to work on downstreams or closely related codebases.

[DreamMaker]: https://secure.byond.com/
[/tg/station13]: https://github.com/tgstation/tgstation/

## Installation

1. Clone the repository to your machine.
1. [Install Rust] or update your existing installation.
1. In your `SpacemanDMM` directory, run `cargo build -p cli`.
   * `-p cli` is used to build only the command-line tools and not the editor,
     which requires Qt.
   * Add `--release` for a release build, with slightly more optimization flags
     set.

[Install Rust]: https://www.rust-lang.org/en-US/install.html

## Usage

Executables are placed in `target/debug` or `target/release` depending on build
type. The CLI binary is known as `dmm-tools`. For best results, run from the
directory containing `tgstation.dme`.

Basic usage: `dmm-tools minimap -o dir/to/save/minimap path/to/map.dmm`.

More detailed usage instructions are available in the `--help` output.

## Recommendations

The minimap output is a very large PNG (e.g. 9.3 MB for Box). You are strongly
advised to run the resulting file through image optimization software such as
`pngcrush`. The `--pngcrush` option to the `minimap` subcommand can do this
automatically in many cases, but is off by default for speed reasons.
