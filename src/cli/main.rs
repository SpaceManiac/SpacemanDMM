//! CLI tools, including a map renderer, using the same backend as the editor.

extern crate structopt;
#[macro_use] extern crate structopt_derive;

extern crate dreammaker as dm;
#[macro_use] extern crate dmm_tools;

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
            for z in 0..map.dim_z() {
                println!("    generating z={}", 1 + z);
                let image = minimap::generate(&objtree, &map, z, &mut icon_cache).unwrap();
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
