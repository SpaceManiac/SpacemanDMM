# SpacemanDMM

**SpacemanDMM** is a suite of tools for working with [DreamMaker] codebases
and map files. It features a full-size fancy map renderer, a [language server],
and a documentation generator for DM codebases.

Language server updates are released to the editor extensions on a regular
basis and binaries are occasionally posted to the [releases] page.
[Building from source](#building) is recommended to get the latest updates to
the map renderer.

The documentation and static analysis tools can be run as part of a continuous
integration build; see [/tg/station's `.travis.yml`][ci] for an example.

Support is currently provided in /tg/station13's coderbus (ping `SpaceManiac`)
and on the [issue tracker]. Pull requests are welcome.

[DreamMaker]: https://secure.byond.com/
[language server]: https://langserver.org/
[releases]: https://github.com/SpaceManiac/SpacemanDMM/releases
[ci]: https://github.com/tgstation/tgstation/blob/master/.travis.yml
[issue tracker]: https://github.com/SpaceManiac/SpacemanDMM/issues

## [Language Server](src/langserver/)

The language server provides autocomplete, go-to-definition, and
[other code intelligence][ls-readme] for the DreamMaker language. The preferred
installation method is the
[Visual Studio Code extension][vsc] ([source][vsc-src]).
There is also a [Sublime Text 3 package][st3] ([source][st3-src]), and the
implementation is compatible with most [language server clients][lsc].

[ls-readme]: ./src/langserver/README.md
[lsc]: https://langserver.org/#implementations-client
[vsc]: https://marketplace.visualstudio.com/items?itemName=platymuus.dm-langclient
[st3]: https://packagecontrol.io/packages/DreamMaker%20Language%20Client
[vsc-src]: https://github.com/SpaceManiac/vscode-dm-langclient
[st3-src]: https://github.com/SpaceManiac/sublime-dm-langclient

## [Map Renderer](src/cli/)

The map renderer produces full-scale PNGs of `.dmm` map files, including
configurable emulation of in-game graphical enhancements not usually visible in
the editor, specialized for [/tg/station13] but likely to work on similar
codebases.

```sh
$ cd path/to/tgstation/
$ dmm-tools minimap _maps/map_files/MetaStation/MetaStation.dmm
```

By default, output is saved to `data/minimaps/`, which can be changed with the
`-o` flag. More detailed usage information is available in the `--help` output.

For examples of the maps produced, visit the [SS13 WebMap][meta].

[/tg/station13]: https://github.com/tgstation/tgstation/
[meta]: https://affectedarc07.github.io/SS13WebMap/TG/MetaStation/

## [Documentation Generator](src/dmdoc/)

`dmdoc` is a simple Doxygen-esque documentation generator for DreamMaker code.
Files, macros, types, vars, and procs can be documented. Documentation comments
start with `/**` or `///` when preceding the documented item, or `/*!` or `//!`
when contained within it.

The contents of `code/README.md` are rendered as the index page and other
Markdown files within the `code/` directory are included in the output. The
generated documentation also includes GitHub links to item definitions.

For an example of the generated documentation, see
[/tg/station13's code docs][tgdocs].

[tgdocs]: https://codedocs.tgstation13.org/

## [Static Analysis](src/dreamchecker/)

DreamChecker is SpacemanDMM's static analysis tool. It can generate several
type-safety diagnostics which DreamMaker does not, and extends the langauge
with return-type annotations, described in its documentation. It is suitable
for running in continuous integration environments.

## Building

To build locally, begin by [installing Rust][rust] or updating your existing
installation. SpacemanDMM is tested against stable Rust.

Clone this repository and run `cargo build --release` to build the full suite
in `target/release/`. List and build individual binaries with
`cargo build --release --bin`. See the [source readme] for more information on
the individual packages.

[rust]: https://www.rust-lang.org/en-US/install.html
[source readme]: ./src/README.md

### Docker

A `dockerfile` is provided for the map generator binary. To build the docker
image, enter the SpacemanDMM directory and run:

```shell
docker build -t spacemandmm .
```

To use the image, switch to the codebase you want to generate maps for and invoke the container:

```shell
docker run -v "$PWD":/usr/src/codebase --rm -it spacemandmm -e /usr/src/codebase/tgstation.dme minimap /usr/src/codebase/_maps/map_files/BoxStation/BoxStation.dmm
```

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

