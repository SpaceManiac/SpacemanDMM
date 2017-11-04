#[macro_use] extern crate dmm_tools;
extern crate dreammaker as dm;

use dmm_tools::*;

fn main() {
    let objtree = {
        flame!("parse");
        dm::parse_environment("tgstation.dme".as_ref()).unwrap()
    };
    let mut icon_cache = std::collections::HashMap::new();

    for path in std::env::args().skip(1) {
        let path: &std::path::Path = path.as_ref();
        println!("{}", path.display());
        flame!(path.file_name().unwrap().to_string_lossy().into_owned());
        let map = dmm::Map::from_file(path).unwrap();

        for z in 0..map.dim_z() {
            println!("    generating z={}", 1 + z);
            let image = minimap::generate(&objtree, &map, z, &mut icon_cache).unwrap();
            let output = format!("data/minimaps/{}-{}.png", path.file_stem().unwrap().to_string_lossy(), 1 + z);
            println!("    saving {}", output);
            image.to_file(output.as_ref()).unwrap();
        }
    }

    #[cfg(feature="flame")] {
        println!("Saving flame graph");
        flame::dump_html(&mut std::io::BufWriter::new(std::fs::File::create("data/minimaps/flame-graph.html").unwrap())).unwrap();
    }
}
