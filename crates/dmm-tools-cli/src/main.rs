//! CLI tools, including a map renderer, using the same backend as the editor.
#![forbid(unsafe_code)]
#![doc(hidden)] // Don't interfere with lib docs.

extern crate rayon;
extern crate structopt;

extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

extern crate dmm_tools;
extern crate dreammaker as dm;

use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicIsize, Ordering};
use std::sync::RwLock;

use rayon::iter::{IndexedParallelIterator, IntoParallelIterator, ParallelIterator};
use structopt::StructOpt;

use dm::objtree::ObjectTree;
use dmm_tools::*;

use ahash::RandomState;

// ----------------------------------------------------------------------------
// Main driver

fn main() {
    let opt = Opt::from_clap(
        &Opt::clap()
            .long_version(
                concat!(
                    env!("CARGO_PKG_VERSION"),
                    "\n",
                    include_str!(concat!(env!("OUT_DIR"), "/build-info.txt")),
                )
                .trim_end(),
            )
            .get_matches(),
    );

    let mut context = Context::default();
    context
        .dm_context
        .set_print_severity(Some(dm::Severity::Error));
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
        eprintln!("parsing {}", environment.display());

        if let Some(parent) = environment.parent() {
            self.icon_cache.set_icons_root(parent);
        }

        self.dm_context.autodetect_config(&environment);
        let pp = match dm::preprocessor::Preprocessor::new(&self.dm_context, environment) {
            Ok(pp) => pp,
            Err(e) => {
                eprintln!("i/o error opening environment:\n{}", e);
                std::process::exit(1);
            }
        };
        let indents = dm::indents::IndentProcessor::new(&self.dm_context, pp);
        let parser = dm::parser::Parser::new(&self.dm_context, indents);
        self.objtree = parser.parse_object_tree();
    }
}

#[derive(StructOpt, Debug)]
#[structopt(
    name = "dmm-tools",
    author = "Copyright (C) 2017-2021  Tad Hardesty",
    about = "This program comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under the conditions of the GNU
General Public License version 3."
)]
struct Opt {
    /// The environment file to operate under.
    #[structopt(short = "e", long = "env")]
    environment: Option<String>,

    #[structopt(short = "v", long = "verbose")]
    #[allow(dead_code)]
    verbose: bool,

    /// Set the number of threads to be used for parallel execution when
    /// possible. A value of 0 will select automatically, and 1 will be serial.
    #[structopt(long = "jobs", default_value = "1")]
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
        #[structopt(short = "j", long = "json")]
        json: bool,
    },
    /// Build minimaps of the specified maps.
    #[structopt(name = "minimap")]
    Minimap {
        /// The output directory.
        #[structopt(short = "o", default_value = "data/minimaps")]
        output: String,

        /// Set the minimum x,y or x,y,z coordinate to act upon (1-indexed, inclusive).
        #[structopt(long = "min")]
        min: Option<CoordArg>,

        /// Set the maximum x,y or x,y,z coordinate to act upon (1-indexed, inclusive).
        #[structopt(long = "max")]
        max: Option<CoordArg>,

        /// Enable render-passes, or "all" to only exclude those passed to --disable.
        #[structopt(long = "enable", default_value = "")]
        enable: String,

        /// Disable render-passes, or "all" to only use those passed to --enable.
        #[structopt(long = "disable", default_value = "")]
        disable: String,

        /// Run output through pngcrush automatically. Requires pngcrush.
        #[structopt(long = "pngcrush")]
        pngcrush: bool,

        /// Run output through optipng automatically. Requires optipng.
        #[structopt(long = "optipng")]
        optipng: bool,

        /// The list of maps to process.
        files: Vec<String>,
    },
    /// List the differing coordinates between two maps.
    #[structopt(name = "diff-maps")]
    DiffMaps { left: String, right: String },
    /// Show metadata information about the map.
    #[structopt(name = "map-info")]
    MapInfo {
        /// Output as JSON.
        #[structopt(short = "j", long = "json")]
        json: bool,

        /// The list of maps to show info on.
        files: Vec<String>,
    },
    /// Read a JSON RenderManyCommand from stdin, execute it, and print a RenderManyCommandResult.
    RenderMany,
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
                    name,
                    desc,
                    default,
                    new: _,
                } in render_passes::RENDER_PASSES
                {
                    report.push(Pass {
                        name,
                        desc,
                        default,
                    });
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
        }
        // --------------------------------------------------------------------
        Command::Minimap {
            ref output,
            min,
            max,
            ref enable,
            ref disable,
            ref files,
            pngcrush,
            optipng,
        } => {
            context.objtree(opt);
            if context
                .dm_context
                .errors()
                .iter()
                .any(|e| e.severity() <= dm::Severity::Error)
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

            let render_passes = &dmm_tools::render_passes::configure(
                &context.dm_context.config().map_renderer,
                enable,
                disable,
            );
            let paths: Vec<&Path> = files.iter().map(|p| p.as_ref()).collect();
            let errors: RwLock<HashSet<String, RandomState>> = Default::default();

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
                        objtree,
                        map: &map,
                        level: map.z_level(z),
                        min: (min.x - 1, min.y - 1),
                        max: (max.x - 1, max.y - 1),
                        render_passes,
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
                        assert!(
                            std::process::Command::new("pngcrush")
                                .arg("-ow")
                                .arg(&outfile)
                                .arg(&temp)
                                .stderr(std::process::Stdio::null())
                                .status()
                                .unwrap()
                                .success(),
                            "pngcrush failed"
                        );
                    }
                    if optipng {
                        println!("{}optipng {}", prefix, outfile);
                        assert!(
                            std::process::Command::new("optipng")
                                .arg(&outfile)
                                .stderr(std::process::Stdio::null())
                                .status()
                                .unwrap()
                                .success(),
                            "optipng failed"
                        );
                    }
                };

                if parallel {
                    ((min.z - 1)..(max.z)).into_par_iter().for_each(do_z_level);
                } else {
                    ((min.z - 1)..(max.z)).into_iter().for_each(do_z_level);
                }
            };

            if parallel {
                // Suboptimal due to mixing I/O in with what's meant for CPU
                // tasks, but it should get the job done for now.
                paths.into_par_iter().for_each(perform_job);
            } else {
                paths.into_iter().for_each(perform_job);
            }
        }
        // --------------------------------------------------------------------
        Command::DiffMaps {
            ref left,
            ref right,
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
                        let left_tile =
                            &left_map.dictionary[&left_map.grid[(z, left_dims.1 - y - 1, x)]];
                        let right_tile =
                            &right_map.dictionary[&right_map.grid[(z, right_dims.1 - y - 1, x)]];
                        if left_tile != right_tile {
                            println!("    different tile: ({}, {}, {})", x + 1, y + 1, z + 1);
                        }
                    }
                }
            }
        }
        // --------------------------------------------------------------------
        Command::MapInfo { json, ref files } => {
            if !json {
                eprintln!("non-JSON output is not yet supported");
            }

            #[derive(Serialize)]
            struct Map {
                size: (usize, usize, usize),
                key_length: u8,
                num_keys: usize,
            }

            let mut report = HashMap::with_hasher(RandomState::default());
            for path in files.iter() {
                let path = std::path::Path::new(path);
                let map = dmm::Map::from_file(path).unwrap();
                report.insert(
                    path,
                    Map {
                        size: map.dim_xyz(),
                        key_length: map.key_length(),
                        num_keys: map.dictionary.len(),
                    },
                );
            }
            output_json(&report);
        }
        // --------------------------------------------------------------------
        Command::RenderMany => {
            let stdin = std::io::stdin();
            let command: RenderManyCommand = serde_json::from_reader(stdin.lock()).unwrap();
            context.objtree(opt);
            let result = render_many(context, command);
            let stdout = std::io::stdout();
            serde_json::to_writer(stdout.lock(), &result).unwrap();
        } // --------------------------------------------------------------------
    }
}

// ----------------------------------------------------------------------------
// Argument parsing helpers

#[derive(Debug, Copy, Clone, Deserialize)]
struct CoordArg {
    x: usize,
    y: usize,
    #[serde(default)]
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
            .split(',')
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
    let mut stdout = stdout.lock();
    serde_json::to_writer(&mut stdout, t).unwrap();
    stdout.write_all(b"\n").unwrap();
}

// ----------------------------------------------------------------------------
// Render Many implementation

#[derive(Deserialize)]
struct RenderManyCommand {
    /// Directory to write results to.
    output_directory: PathBuf,

    /// `.dmm` files to render.
    ///
    /// Including a file multiple times in this list is less efficient than
    /// combining its chunks into one entry in this list.
    files: Vec<RenderManyFile>,

    /// Render passes to enable, or `["all"]` for all not explicitly disabled.
    #[serde(default)]
    enable: Vec<String>,

    /// Render passes to disable, or `["all"]` for all not explicitly enabled.
    #[serde(default)]
    disable: Vec<String>,
}

#[derive(Deserialize)]
struct RenderManyFile {
    /// The path to the `.dmm` file to render.
    path: PathBuf,
    /// Defaults to one chunk per z-level if omitted.
    chunks: Option<Vec<RenderManyChunk>>,
}

#[derive(Deserialize, Debug)]
struct RenderManyChunk {
    /// The z-level to render.
    z: usize,
    /// Defaults to 1.
    min_x: Option<usize>,
    /// Defaults to 1.
    min_y: Option<usize>,
    /// Defaults to the X size of the map.
    max_x: Option<usize>,
    /// Defaults to the Y size of the map.
    max_y: Option<usize>,
}

#[derive(Serialize)]
struct RenderManyCommandResult {
    /// Results for each file in the same order as the input.
    files: Vec<RenderManyFileResult>,
}

#[derive(Serialize)]
struct RenderManyFileResult {
    /// Results for each chunk in the same order as the input.
    ///
    /// If `chunks` was omitted in the input, this has one chunk per z-level.
    chunks: Vec<RenderManyChunkResult>,
}

#[derive(Serialize)]
struct RenderManyChunkResult {
    /// The filename, not including the output directory, that the chunk was written to.
    filename: PathBuf,
}

fn render_many(context: &Context, command: RenderManyCommand) -> RenderManyCommandResult {
    // Parse the object tree and otherwise prepare the context.
    if context
        .dm_context
        .errors()
        .iter()
        .any(|e| e.severity() <= dm::Severity::Error)
    {
        eprintln!("there were some parsing errors; render may be inaccurate")
    }
    let Context {
        ref objtree,
        ref icon_cache,
        ref exit_status,
        ..
    } = *context;
    let render_passes = &dmm_tools::render_passes::configure_list(
        &context.dm_context.config().map_renderer,
        &command.enable,
        &command.disable,
    );
    let errors: RwLock<HashSet<String, RandomState>> = Default::default();

    // Prepare output directory.
    let output_directory = command.output_directory;
    if let Err(e) = std::fs::create_dir_all(&output_directory) {
        eprintln!(
            "failed to create output directory {}:\n{}",
            output_directory.display(),
            e
        );
        exit_status.fetch_add(1, Ordering::Relaxed);
        panic!();
    }

    // Iterate over the maps
    let result_files: Vec<_> = command
        .files
        .into_par_iter()
        .enumerate()
        .map(|(file_idx, file)| {
            eprintln!("{}: load {}", file_idx, file.path.display());
            let stem = file.path.file_stem().unwrap().to_string_lossy();
            let map = dmm::Map::from_file(&file.path).unwrap(); // TODO: error handling
            let (dim_x, dim_y, dim_z) = map.dim_xyz();

            // If `chunks` was not specified, render one chunk per z-level.
            let chunks = file.chunks.unwrap_or_else(|| {
                (1..=dim_z)
                    .map(|z| RenderManyChunk {
                        z,
                        min_x: None,
                        min_y: None,
                        max_x: None,
                        max_y: None,
                    })
                    .collect()
            });

            let result_chunks: Vec<_> = chunks
                .into_par_iter()
                .enumerate()
                .map(|(chunk_idx, chunk)| {
                    eprintln!("{}/{}: render {:?}", file_idx, chunk_idx, chunk);

                    // Render the image.
                    let bump = Default::default();
                    let minimap_context = minimap::Context {
                        objtree,
                        map: &map,
                        level: map.z_level(chunk.z - 1),
                        // Default and clamp to [1, max].
                        min: (
                            chunk.min_x.unwrap_or(1).max(1) - 1,
                            chunk.min_y.unwrap_or(1).max(1) - 1,
                        ),
                        max: (
                            chunk.max_x.unwrap_or(dim_x).min(dim_x) - 1,
                            chunk.max_y.unwrap_or(dim_y).min(dim_y) - 1,
                        ),
                        render_passes,
                        errors: &errors,
                        bump: &bump,
                    };
                    let image = minimap::generate(minimap_context, icon_cache).unwrap(); // TODO: error handling

                    // Write it to file.
                    let filename =
                        PathBuf::from(format!("{}_z{}_chunk{}.png", stem, chunk.z, chunk_idx,));
                    eprintln!("{}/{}: save {}", file_idx, chunk_idx, filename.display());
                    let outfile = output_directory.join(&filename);
                    image.to_file(&outfile).unwrap(); // TODO: error handling

                    RenderManyChunkResult { filename }
                })
                .collect();

            RenderManyFileResult {
                chunks: result_chunks,
            }
        })
        .collect();

    RenderManyCommandResult {
        files: result_files,
    }
}
