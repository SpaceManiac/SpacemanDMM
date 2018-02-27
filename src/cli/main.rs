//! CLI tools, including a map renderer, using the same backend as the editor.

extern crate structopt;
#[macro_use] extern crate structopt_derive;

extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;

extern crate dreammaker as dm;
#[macro_use] extern crate dmm_tools;

use std::fmt;
use std::path::PathBuf;
use std::collections::HashMap;

use structopt::StructOpt;

use dm::objtree::ObjectTree;
use dmm_tools::*;
use dmm_tools::dmi::IconFile;

// ----------------------------------------------------------------------------
// Main driver

fn main() {
    let opt = Opt::from_args();
    let mut context = Context::default();
    run(&opt, &opt.command, &mut context);

    #[cfg(feature="flame")] {
        println!("Saving flame graph");
        flame::dump_html(&mut std::io::BufWriter::new(std::fs::File::create(format!("{}/flame-graph.html", opt.output)).unwrap())).unwrap();
    }
}

#[derive(Default)]
struct Context {
    objtree: ObjectTree,
    icon_cache: HashMap<PathBuf, IconFile>,
}

impl Context {
    fn objtree(&mut self, opt: &Opt) {
        println!("parsing {}", opt.environment);
        flame!("parse");
        match dm::parse_environment(opt.environment.as_ref()) {
            Ok(tree) => self.objtree = tree,
            Err(_) => {
                // parse_environment has already pretty_printed the error message
                println!("fatal parse error");
                std::process::exit(1);
            }
        };
    }
}

#[derive(StructOpt, Debug)]
struct Opt {
    /// The environment file to operate under.
    #[structopt(short="e", long="env", default_value="tgstation.dme")]
    environment: String,

    #[structopt(short="v", long="verbose")]
    verbose: bool,

    #[structopt(subcommand)]
    command: Command,
}

// ----------------------------------------------------------------------------
// Subcommands

#[derive(StructOpt, Debug)]
enum Command {
    /// Show information about the render-pass list.
    #[structopt(name = "list-passes")]
    ListPasses,
    /// Build minimaps of the specified maps.
    #[structopt(name = "minimap")]
    Minimap {
        /// The output directory.
        #[structopt(short="o", default_value="data/minimaps")]
        output: String,

        /// Set the minimum x,y or x,y,z coordinate to act upon (1-indexed, inclusive).
        #[structopt(long="min")]
        min: Option<CoordArg>,

        /// Set the maximum x,y or x,y,z coordinate to act upon (1-indexed, inclusive).
        #[structopt(long="max")]
        max: Option<CoordArg>,

        /// Enable render-passes, or "all" to only exclude those passed to --disable.
        #[structopt(long="enable", default_value="")]
        enable: String,

        /// Disable render-passes, or "all" to only use those passed to --enable.
        #[structopt(long="disable", default_value="")]
        disable: String,

        /// Run output through pngcrush automatically. Requires pngcrush.
        #[structopt(long="pngcrush")]
        pngcrush: bool,

        /// Run output through optipng automatically. Requires optipng.
        #[structopt(long="optipng")]
        optipng: bool,

        /// The list of maps to process.
        files: Vec<String>,
    },
    /// Lint and automatically fix the specified maps.
    #[structopt(name = "lint-maps")]
    LintMaps {
        /// Only report and do not save out changes.
        #[structopt(short="n", long="dry-run")]
        dry_run: bool,

        /// Reformat the specified maps even if nothing was changed.
        #[structopt(long="reformat")]
        reformat: bool,

        /// The list of maps to process.
        files: Vec<String>,
    },
    /// List the differing coordinates between two maps.
    #[structopt(name="diff-maps")]
    DiffMaps {
        left: String,
        right: String,
    },
    /// Show metadata information about the map.
    #[structopt(name="map-info")]
    MapInfo {
        /// Output as JSON.
        #[structopt(short="j", long="json")]
        json: bool,

        /// The list of maps to show info on.
        files: Vec<String>,
    },
}

fn run(opt: &Opt, command: &Command, context: &mut Context) {
    match *command {
        // --------------------------------------------------------------------
        Command::ListPasses => {
            for pass in render_passes::RENDER_PASSES {
                println!("{}{}: {}", pass.name, if pass.default { " (default)" } else { "" }, pass.desc);
            }
        },
        // --------------------------------------------------------------------
        Command::Minimap {
            ref output, min, max, ref enable, ref disable, ref files,
            pngcrush, optipng,
        } => {
            context.objtree(opt);

            let render_passes = dmm_tools::render_passes::configure(enable, disable);
            for path in files.iter() {
                let path: &std::path::Path = path.as_ref();
                println!("{}", path.display());
                flame!(path.file_name().unwrap().to_string_lossy().into_owned());
                let mut map = dmm::Map::from_file(path).expect("DMM file missing");

                let (dim_x, dim_y, dim_z) = map.dim_xyz();
                let mut min = min.unwrap_or(CoordArg { x: 0, y: 0, z: 0 });
                let mut max = max.unwrap_or(CoordArg { x: dim_x + 1, y: dim_y + 1, z: dim_z + 1 });
                min.x = clamp(min.x, 1, dim_x);
                min.y = clamp(min.y, 1, dim_y);
                min.z = clamp(min.z, 1, dim_z);
                max.x = clamp(max.x, min.x, dim_x);
                max.y = clamp(max.y, min.y, dim_y);
                max.z = clamp(max.z, min.z, dim_z);
                println!("    rendering from {} to {}", min, max);

                for z in (min.z - 1)..(max.z) {
                    println!("    generating z={}", 1 + z);
                    let minimap_context = minimap::Context {
                        objtree: &context.objtree,
                        map: &map,
                        grid: map.z_level(z),
                        min: (min.x - 1, min.y - 1),
                        max: (max.x - 1, max.y - 1),
                        render_passes: &render_passes,
                    };
                    let image = minimap::generate(minimap_context, &mut context.icon_cache).unwrap();
                    std::fs::create_dir_all(output).expect("Failed to create output directory");
                    let outfile = format!("{}/{}-{}.png", output, path.file_stem().unwrap().to_string_lossy(), 1 + z);
                    println!("    saving {}", outfile);
                    image.to_file(outfile.as_ref()).unwrap();
                    if pngcrush {
                        println!("    pngcrush {}", outfile);
                        let temp = format!("{}.temp", outfile);
                        assert!(std::process::Command::new("pngcrush")
                            .arg("-ow")
                            .arg(&outfile)
                            .arg(&temp)
                            .stderr(std::process::Stdio::null())
                            .status()
                            .unwrap()
                            .success(), "pngcrush failed");
                    }
                    if optipng {
                        println!("    optipng {}", outfile);
                        assert!(std::process::Command::new("optipng")
                            .arg(&outfile)
                            .stderr(std::process::Stdio::null())
                            .status()
                            .unwrap()
                            .success(), "optipng failed");
                    }
                }
            }
        },
        // --------------------------------------------------------------------
        Command::LintMaps {
            dry_run, reformat, ref files,
        } => {
            context.objtree(opt);

            for path in files.iter() {
                let path: &std::path::Path = path.as_ref();
                println!("{}", path.display());
                flame!(path.file_name().unwrap().to_string_lossy().into_owned());
                let mut map = dmm::Map::from_file(path).unwrap();

                let linted = { flame!("lint"); lint::check(&context.objtree, &mut map) };
                print!("{}", linted);
                if !dry_run && (linted.any() || reformat) {
                    println!("    saving {}", path.display());
                    flame!("save");
                    map.to_file(path).unwrap();
                }
            }
        },
        // --------------------------------------------------------------------
        Command::DiffMaps {
            ref left, ref right,
        } => {
            use std::cmp::min;

            context.objtree(opt);

            let path: &std::path::Path = left.as_ref();
            println!("{}", path.display());
            let left_map = dmm::Map::from_file(path).unwrap();
            let path: &std::path::Path = right.as_ref();
            println!("{}", path.display());
            let right_map = dmm::Map::from_file(path).unwrap();

            let left_dims = left_map.dim_xyz();
            let right_dims = right_map.dim_xyz();
            if left_dims != right_dims {
                println!("    different size: {:?} {:?}", left_dims, right_dims);
            }

            for z in 0..min(left_dims.2, right_dims.2) {
                for y in 0..min(left_dims.1, right_dims.1) {
                    for x in 0..min(left_dims.0, right_dims.0) {
                        let left_tile = &left_map.dictionary[&left_map.grid[(z, left_dims.1 - y - 1, x)]];
                        let right_tile = &right_map.dictionary[&right_map.grid[(z, right_dims.1 - y - 1, x)]];
                        if left_tile != right_tile {
                            println!("    different tile: ({}, {}, {})", x + 1, y + 1, z + 1);
                        }
                    }
                }
            }
        },
        // --------------------------------------------------------------------
        Command::MapInfo {
            json, ref files,
        } => {
            if !json {
                println!("non-JSON output is not yet supported");
            }

            #[derive(Serialize)]
            struct Map {
                size: (usize, usize, usize),
                key_length: u8,
                num_keys: usize,
            }

            let mut report = HashMap::new();
            for path in files.iter() {
                let path: &std::path::Path = path.as_ref();
                let mut map = dmm::Map::from_file(path).unwrap();

                let dim = map.grid.dim();
                report.insert(path, Map {
                    size: (dim.2, dim.1, dim.0),  // zyx -> xyz
                    key_length: map.key_length,
                    num_keys: map.dictionary.len(),
                });
            }

            let stdout = std::io::stdout();
            serde_json::to_writer(stdout.lock(), &report).unwrap();
            println!();
        },
        // --------------------------------------------------------------------
    }
}

// ----------------------------------------------------------------------------
// Argument parsing helpers

#[derive(Debug, Copy, Clone)]
struct CoordArg {
    x: usize,
    y: usize,
    z: usize,
}

impl fmt::Display for CoordArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.z != 0 {
            write!(f, "{},{},{}", self.x, self.y, self.z)
        } else {
            write!(f, "{},{}", self.x, self.y)
        }
    }
}

impl std::str::FromStr for CoordArg {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        match s.split(",").map(|x| x.parse()).collect::<Result<Vec<_>, std::num::ParseIntError>>() {
            Ok(ref vec) if vec.len() == 2 => {
                Ok(CoordArg { x: vec[0], y: vec[1], z: 0 })
            }
            Ok(ref vec) if vec.len() == 3 => {
                Ok(CoordArg { x: vec[0], y: vec[1], z: vec[2] })
            }
            Ok(_) => Err("must specify 2 or 3 coordinates".into()),
            Err(e) => Err(e.to_string()),
        }
    }
}

fn clamp(val: usize, min: usize, max: usize) -> usize {
    if val < min {
        min
    } else if val > max {
        max
    } else {
        val
    }
}
