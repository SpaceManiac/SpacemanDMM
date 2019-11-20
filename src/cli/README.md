# Map Renderer

`dmm-tools` is SpacemanDMM's map renderer. It generates full-sized map images
and emulates many in-game graphical enhancements not usually visible in the map
editor, including overlays and smoothing. These features currently assume
[/tg/station13], but are highly likely to work on downstreams or closely
related codebases.

For best results, run from the directory containing your `.dme` file:

```sh
$ cd path/to/tgstation/
$ dmm-tools minimap _maps/map_files/BoxStation/BoxStation.dmm
```

By default, output is saved to `data/minimaps/`, which can be changed with the
`-o` flag. The `-e` flag, specified before the subcommand, can be used to load
a different `.dme` file. More detailed usage information is available in the
`--help` output.

The minimap output is a very large PNG (e.g. 9.3 MB for Box). You are strongly
advised to run the resulting file through image optimization software such as
`pngcrush`. The `--pngcrush` option to the `minimap` subcommand can do this
automatically in many cases, but is off by default for speed reasons.

## Render Passes

Render passes are used to provide enhanced rendering of certain object types,
making them appear more like in-game. The default set of render passes is
optimized for [/tg/station13]. Run `dmm-tools list-passes` to list the
available render passes. Select passes by providing the `--enable` or
`--disable` flags with a comma-separated list of pass names, or `--disable all`
to disable all default passes. For example, to show space and disable random
generation, use `--disable hide-space,random`, or to enable nothing but hiding
of areas, use `--disable all --enable hide-areas`.

[/tg/station13]: https://github.com/tgstation/tgstation/
