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

    /* /// Build minimaps of the specified maps.
    #[structopt(long="minimap")]
    minimap: bool, */

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
        let map = dmm::Map::from_file(path).unwrap();

        for z in 0..map.dim_z() {
            println!("    generating z={}", 1 + z);
            let image = minimap::generate(&objtree, &map, z, &mut icon_cache).unwrap();
            let output = format!("{}/{}-{}.png", opt.output, path.file_stem().unwrap().to_string_lossy(), 1 + z);
            println!("    saving {}", output);
            image.to_file(output.as_ref()).unwrap();
        }
    }

    #[cfg(feature="flame")] {
        println!("Saving flame graph");
        flame::dump_html(&mut std::io::BufWriter::new(std::fs::File::create(format!("{}/flame-graph.html", opt.output)).unwrap())).unwrap();
    }
}
