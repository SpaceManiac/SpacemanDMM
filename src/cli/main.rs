//! CLI tools, including a map renderer, using the same backend as the editor.
#![forbid(unsafe_code)]
#![doc(hidden)]  // Don't interfere with lib docs.

extern crate rayon;
extern crate structopt;

extern crate serde;
extern crate serde_json;
#[macro_use] extern crate serde_derive;

extern crate dreammaker as dm;
extern crate dmm_tools;

use std::collections::HashMap;
use std::fmt;
use std::path::Path;
use std::sync::atomic::{AtomicIsize, Ordering};
use std::sync::RwLock;
use std::collections::HashSet;

use structopt::StructOpt;

use dm::objtree::ObjectTree;
use dmm_tools::*;

// ----------------------------------------------------------------------------
// Main driver

fn main() {
    let opt = Opt::from_clap(&Opt::clap()
        .long_version(concat!(
            env!("CARGO_PKG_VERSION"), "\n",
            include_str!(concat!(env!("OUT_DIR"), "/build-info.txt")),
        ).trim_end())
        .get_matches());

    let mut context = Context::default();
    context.dm_context.set_print_severity(Some(dm::Severity::Error));
    rayon::ThreadPoolBuilder::new()
        .num_threads(opt.jobs)
        .build_global()
        .expect("failed to initialize thread pool");
    context.parallel = opt.jobs != 1;

    run(&opt, &opt.command, &mut context);

    std::process::exit(context.exit_status.into_inner() as i32);
}

#[derive(Default)]
struct Context {
    dm_context: dm::Context,
    objtree: ObjectTree,
    icon_cache: IconCache,
    exit_status: AtomicIsize,
    parallel: bool,
    procs: bool,
}

impl Context {
    fn objtree(&mut self, opt: &Opt) {
        let environment = match opt.environment {
            Some(ref env) => env.into(),
            None => match dm::detect_environment_default() {
                Ok(Some(found)) => found,
                _ => dm::DEFAULT_ENV.into(),
            },
        };
        println!("parsing {}", environment.display());

        if let Some(parent) = environment.parent() {
            self.icon_cache.set_icons_root(&parent);
        }

        let pp = match dm::preprocessor::Preprocessor::new(&self.dm_context, environment) {
            Ok(pp) => pp,
            Err(e) => {
                eprintln!("i/o error opening environment:\n{}", e);
                std::process::exit(1);
            }
        };
        let indents = dm::indents::IndentProcessor::new(&self.dm_context, pp);
        let mut parser = dm::parser::Parser::new(&self.dm_context, indents);
        if self.procs {
            parser.enable_procs();
        }
        self.objtree = parser.parse_object_tree();
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name="dmm-tools",
author="Copyright (C) 2017-2019  Tad Hardesty",
about="This program comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under the conditions of the GNU
General Public License version 3.")]
struct Opt {
    /// The environment file to operate under.
    #[structopt(short="e", long="env")]
    environment: Option<String>,

    #[structopt(short="v", long="verbose")]
    verbose: bool,

    /// Set the number of threads to be used for parallel execution when
    /// possible. A value of 0 will select automatically, and 1 will be serial.
    #[structopt(long="jobs", default_value="1")]
    jobs: usize,

    #[structopt(subcommand)]
    command: Command,
}

// ----------------------------------------------------------------------------
// Subcommands

#[derive(StructOpt, Debug)]
enum Command {
    /// Show information about the render-pass list.
    #[structopt(name = "list-passes")]
    ListPasses {
        /// Output as JSON.
        #[structopt(short="j", long="json")]
        json: bool,
    },
    /// Check the environment for errors and warnings.
    #[structopt(name = "check")]
    Check {
        /// The minimum severity to print, of "error", "warning", "info", "hint".
        #[structopt(long="severity", default_value="info")]
        severity: String,
        /// Check proc bodies as well as the object tree.
        #[structopt(long="procs")]
        procs: bool,
    },
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
        Command::ListPasses { json } => {
            if json {
                #[derive(Serialize)]
                struct Pass<'a> {
                    name: &'a str,
                    desc: &'a str,
                    default: bool,
                }

                let mut report = Vec::new();
                for &render_passes::RenderPassInfo {
                    name, desc, default, new: _,
                } in render_passes::RENDER_PASSES {
                    report.push(Pass { name, desc, default });
                }
                output_json(&report);
            } else {
                println!("default passes:");
                let mut non_default = Vec::new();
                for pass in render_passes::RENDER_PASSES {
                    if pass.default {
                        println!("{}: {}", pass.name, pass.desc);
                    } else {
                        non_default.push(pass);
                    }
                }
                if !non_default.is_empty() {
                    println!("\nadditional passes:");
                    for pass in non_default {
                        println!("{}: {}", pass.name, pass.desc);
                    }
                }
            }
        },
        // --------------------------------------------------------------------
        Command::Check { ref severity, procs } => {
            let severity = match severity.as_str() {
                "error" => dm::Severity::Error,
                "warning" => dm::Severity::Warning,
                "info" => dm::Severity::Info,
                _ => dm::Severity::Hint,
            };
            context.dm_context.set_print_severity(Some(severity));
            context.procs = procs;
            context.objtree(opt);
            *context.exit_status.get_mut() = context
                .dm_context
                .errors()
                .iter()
                .filter(|e| e.severity() <= severity)
                .count() as isize;
        },
        // --------------------------------------------------------------------
        Command::Minimap {
            ref output, min, max, ref enable, ref disable, ref files,
            pngcrush, optipng,
        } => {
            context.objtree(opt);
            if context
                .dm_context
                .errors()
                .iter()
                .filter(|e| e.severity() <= dm::Severity::Error)
                .next()
                .is_some()
            {
                println!("there were some parsing errors; render may be inaccurate")
            }
            let Context {
                ref objtree,
                ref icon_cache,
                ref exit_status,
                parallel,
                ..
            } = *context;

            let render_passes = &dmm_tools::render_passes::configure(enable, disable);
            let paths: Vec<&Path> = files.iter().map(|p| p.as_ref()).collect();
            let errors: RwLock<HashSet<String>> = Default::default();

            let perform_job = move |path: &Path| {
                let mut filename;
                let prefix = if parallel {
                    filename = path.file_name().unwrap().to_string_lossy().into_owned();
                    filename.push_str(": ");
                    println!("{}{}", filename, path.display());
                    &filename
                } else {
                    println!("{}", path.display());
                    "    "
                };

                let map = match dmm::Map::from_file(path) {
                    Ok(map) => map,
                    Err(e) => {
                        eprintln!("Failed to load {}:\n{}", path.display(), e);
                        exit_status.fetch_add(1, Ordering::Relaxed);
                        return;
                    }
                };

                let (dim_x, dim_y, dim_z) = map.dim_xyz();
                let mut min = min.unwrap_or(CoordArg { x: 0, y: 0, z: 0 });
                let mut max = max.unwrap_or(CoordArg {
                    x: dim_x + 1,
                    y: dim_y + 1,
                    z: dim_z + 1,
                });
                min.x = clamp(min.x, 1, dim_x);
                min.y = clamp(min.y, 1, dim_y);
                min.z = clamp(min.z, 1, dim_z);
                max.x = clamp(max.x, min.x, dim_x);
                max.y = clamp(max.y, min.y, dim_y);
                max.z = clamp(max.z, min.z, dim_z);
                println!("{}rendering from {} to {}", prefix, min, max);

                let do_z_level = |z| {
                    println!("{}generating z={}", prefix, 1 + z);
                    let bump = Default::default();
                    let minimap_context = minimap::Context {
                        objtree: &objtree,
                        map: &map,
                        level: map.z_level(z),
                        min: (min.x - 1, min.y - 1),
                        max: (max.x - 1, max.y - 1),
                        render_passes: &render_passes,
                        errors: &errors,
                        bump: &bump,
                    };
                    let image = minimap::generate(minimap_context, icon_cache).unwrap();
                    if let Err(e) = std::fs::create_dir_all(output) {
                        eprintln!("Failed to create output directory {}:\n{}", output, e);
                        exit_status.fetch_add(1, Ordering::Relaxed);
                        return;
                    }
                    let outfile = format!(
                        "{}/{}-{}.png",
                        output,
                        path.file_stem().unwrap().to_string_lossy(),
                        1 + z
                    );
                    println!("{}saving {}", prefix, outfile);
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
                        println!("{}optipng {}", prefix, outfile);
                        assert!(std::process::Command::new("optipng")
                            .arg(&outfile)
                            .stderr(std::process::Stdio::null())
                            .status()
                            .unwrap()
                            .success(), "optipng failed");
                    }
                };

                if parallel {
                    use rayon::iter::{IntoParallelIterator, ParallelIterator};
                    ((min.z - 1)..(max.z)).into_par_iter().for_each(do_z_level);
                } else {
                    ((min.z - 1)..(max.z)).into_iter().for_each(do_z_level);
                }
            };

            if parallel {
                use rayon::iter::{IntoParallelIterator, ParallelIterator};
                // Suboptimal due to mixing I/O in with what's meant for CPU
                // tasks, but it should get the job done for now.
                paths.into_par_iter().for_each(perform_job);
            } else {
                paths.into_iter().for_each(perform_job);
            }
        },
        // --------------------------------------------------------------------
        Command::DiffMaps {
            ref left, ref right,
        } => {
            use std::cmp::min;

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
                eprintln!("non-JSON output is not yet supported");
            }

            #[derive(Serialize)]
            struct Map {
                size: (usize, usize, usize),
                key_length: u8,
                num_keys: usize,
            }

            let mut report = HashMap::new();
            for path in files.iter() {
                let path = std::path::Path::new(path);
                let map = dmm::Map::from_file(path).unwrap();
                report.insert(path, Map {
                    size: map.dim_xyz(),
                    key_length: map.key_length(),
                    num_keys: map.dictionary.len(),
                });
            }
            output_json(&report);
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
        match s
            .split(",")
            .map(|x| x.parse())
            .collect::<Result<Vec<_>, std::num::ParseIntError>>()
        {
            Ok(ref vec) if vec.len() == 2 => Ok(CoordArg {
                x: vec[0],
                y: vec[1],
                z: 0,
            }),
            Ok(ref vec) if vec.len() == 3 => Ok(CoordArg {
                x: vec[0],
                y: vec[1],
                z: vec[2],
            }),
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

fn output_json<T: serde::Serialize>(t: &T) {
    let stdout = std::io::stdout();
    serde_json::to_writer(stdout.lock(), t).unwrap();
    println!();
}
