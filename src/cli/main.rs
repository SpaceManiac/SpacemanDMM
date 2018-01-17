//! CLI tools, including a map renderer, using the same backend as the editor.

extern crate structopt;
#[macro_use] extern crate structopt_derive;

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
        self.objtree = dm::parse_environment(opt.environment.as_ref()).unwrap();
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
        #[structopt(long="enable")]
        enable: Vec<String>,

        /// Disable render-passes, or "all" to only use those passed to --enable.
        #[structopt(long="disable")]
        disable: Vec<String>,

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
        } => {
            context.objtree(opt);

            let render_passes = dmm_tools::render_passes::configure(enable, disable);
            for path in files.iter() {
                let path: &std::path::Path = path.as_ref();
                println!("{}", path.display());
                flame!(path.file_name().unwrap().to_string_lossy().into_owned());
                let mut map = dmm::Map::from_file(path).unwrap();

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
                    };
                    let image = minimap::generate(minimap_context, &mut context.icon_cache, &render_passes).unwrap();
                    let output = format!("{}/{}-{}.png", output, path.file_stem().unwrap().to_string_lossy(), 1 + z);
                    println!("    saving {}", output);
                    image.to_file(output.as_ref()).unwrap();
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
