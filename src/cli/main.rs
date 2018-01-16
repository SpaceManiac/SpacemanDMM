//! CLI tools, including a map renderer, using the same backend as the editor.

extern crate structopt;
#[macro_use] extern crate structopt_derive;

extern crate dreammaker as dm;
#[macro_use] extern crate dmm_tools;

use std::fmt;

use structopt::StructOpt;
use dmm_tools::*;

#[derive(StructOpt, Debug)]
struct Opt {
    /// The environment file to use for the rendering.
    #[structopt(short="e", long="env", default_value="tgstation.dme")]
    environment: String,

    /// The output directory.
    #[structopt(short="o", default_value="data/minimaps")]
    output: String,

    /// Build minimaps of the specified maps.
    #[structopt(long="minimap")]
    minimap: bool,

    /// Lint and automatically fix the specified maps.
    #[structopt(long="lint")]
    lint: bool,

    /// Dry-run rather than actually saving out changes.
    #[structopt(short="n", long="dry-run")]
    dry_run: bool,

    /// Reformat saved maps.
    #[structopt(long="reformat")]
    reformat: bool,

    /// Set the minimum x,y or x,y,z coordinate to act upon (1-indexed, inclusive).
    #[structopt(long="min")]
    min: Option<CoordArg>,

    /// Set the maximum x,y or x,y,z coordinate to act upon (1-indexed, inclusive).
    #[structopt(long="max")]
    max: Option<CoordArg>,

    /// The list of files to process.
    files: Vec<String>,
}

fn main() {
    let opt = Opt::from_args();

    let objtree = {
        println!("parsing {}", opt.environment);
        flame!("parse");
        dm::parse_environment(opt.environment.as_ref()).unwrap()
    };
    let mut icon_cache = std::collections::HashMap::new();

    for path in opt.files.iter() {
        let path: &std::path::Path = path.as_ref();
        println!("{}", path.display());
        flame!(path.file_name().unwrap().to_string_lossy().into_owned());
        let mut map = dmm::Map::from_file(path).unwrap();

        let linted_any = opt.lint && {
            let linted = { flame!("lint"); lint::check(&objtree, &mut map) };
            print!("{}", linted);
            linted.any()
        };
        if !opt.dry_run && (linted_any || opt.reformat) {
            println!("    saving {}", path.display());
            flame!("save");
            map.to_file(path).unwrap();
        }

        if opt.minimap {
            let (dim_x, dim_y, dim_z) = map.dim_xyz();
            let mut min = opt.min.unwrap_or(CoordArg { x: 0, y: 0, z: 0 });
            let mut max = opt.max.unwrap_or(CoordArg { x: dim_x + 1, y: dim_y + 1, z: dim_z + 1 });
            min.x = clamp(min.x, 1, dim_x);
            min.y = clamp(min.y, 1, dim_y);
            min.z = clamp(min.z, 1, dim_z);
            max.x = clamp(max.x, min.x, dim_x);
            max.y = clamp(max.y, min.y, dim_y);
            max.z = clamp(max.z, min.z, dim_z);
            println!("    rendering from {} to {}", min, max);

            for z in (min.z - 1)..(max.z) {
                println!("    generating z={}", 1 + z);
                let context = minimap::Context {
                    objtree: &objtree,
                    map: &map,
                    grid: map.z_level(z),
                    min: (min.x - 1, min.y - 1),
                    max: (max.x - 1, max.y - 1),
                };
                let image = minimap::generate(context, &mut icon_cache).unwrap();
                let output = format!("{}/{}-{}.png", opt.output, path.file_stem().unwrap().to_string_lossy(), 1 + z);
                if !opt.dry_run {
                    println!("    saving {}", output);
                    image.to_file(output.as_ref()).unwrap();
                }
            }
        }
    }

    #[cfg(feature="flame")] {
        println!("Saving flame graph");
        flame::dump_html(&mut std::io::BufWriter::new(std::fs::File::create(format!("{}/flame-graph.html", opt.output)).unwrap())).unwrap();
    }
}

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
