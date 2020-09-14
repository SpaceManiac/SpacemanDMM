extern crate dreammaker as dm;
extern crate dmm_tools;
extern crate walkdir;
extern crate ndarray;

use std::path::Path;
use std::collections::HashMap;
use walkdir::{DirEntry, WalkDir};
use dmm_tools::dmi::*;
use ndarray::s;

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
            f(path);
        }
    }
}

fn all_same(icon_file: &IconFile, states: &[&State]) -> bool {
    let (first, rest) = states.split_first().unwrap();
    let first_start_index = first.offset as u32;
    for state in rest {
        if state.dirs != first.dirs || state.frames != first.frames {
            return false;
        }
        let start_index = state.offset as u32;
        for i in 0..state.num_sprites() as u32 {
            let rect1 = icon_file.rect_of_index(first_start_index + i);
            let rect2 = icon_file.rect_of_index(start_index + i);
            let slice1 = icon_file.image.data.slice(s![
                rect1.1 as isize..(rect1.1 + rect1.3) as isize,
                rect1.0 as isize..(rect1.0 + rect1.2) as isize,
                ..
            ]);
            let slice2 = icon_file.image.data.slice(s![
                rect2.1 as isize..(rect2.1 + rect2.3) as isize,
                rect2.0 as isize..(rect2.0 + rect2.2) as isize,
                ..
            ]);
            if slice1 != slice2 {
                return false;
            }
        }
    }
    true
}

pub fn main() {
    files_with_extension("dmi", |path| {
        let icon_file = IconFile::from_file(path).unwrap();
        let mut counts = HashMap::<_, Vec<_>>::new();
        for state in icon_file.metadata.states.iter() {
            let mut name = format!("{:?}", state.name);
            if state.movement {
                name.push_str(" (movement)");
            }
            counts.entry(name).or_default().push(state);
        }
        let mut name = false;
        for (k, v) in counts {
            if v.len() > 1 {
                if !name {
                    println!("{}", path.display());
                    name = true;
                }
                let star = if all_same(&icon_file, &v) { "*" } else { " " };
                println!("  {} {}x {}", star, v.len(), k);
            }
        }
    });
}
