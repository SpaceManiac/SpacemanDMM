extern crate dmm_tools;
extern crate walkdir;

use dmm_tools::*;
use std::path::Path;
use walkdir::{DirEntry, WalkDir};

fn is_visible(entry: &DirEntry) -> bool {
    entry
        .path()
        .file_name()
        .unwrap_or("".as_ref())
        .to_str()
        .map(|s| !s.starts_with("."))
        .unwrap_or(true)
}

fn files_with_extension<F: FnMut(&Path)>(ext: &str, mut f: F) {
    let dir = match std::env::var_os("TEST_DME") {
        Some(dme) => Path::new(&dme).parent().unwrap().to_owned(),
        None => {
            println!("Set TEST_DME to check .{} files", ext);
            return;
        }
    };
    for entry in WalkDir::new(dir).into_iter().filter_entry(is_visible) {
        let entry = entry.unwrap();
        if entry.file_type().is_file() && entry.path().extension() == Some(ext.as_ref()) {
            let path = entry.path();
            println!("{}", path.display());
            f(path);
        }
    }
}

#[test]
fn parse_all_dmm() {
    files_with_extension("dmm", |path| {
        dmm::Map::from_file(path).unwrap();
    });
}

#[test]
fn parse_all_dmi() {
    files_with_extension("dmi", |path| {
        dmi::Metadata::from_file(path).unwrap();
    });
}
