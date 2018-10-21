use std::collections::{hash_map, HashMap};
use std::path::{Path, PathBuf};
use std::sync::{Arc, RwLock};

use super::dmi::IconFile;

#[derive(Default)]
pub struct IconCache {
    lock: RwLock<HashMap<PathBuf, Option<Arc<IconFile>>>>,
}

impl IconCache {
    pub fn retrieve_uniq(&mut self, path: &Path) -> Option<&IconFile> {
        let map = self.lock.get_mut().unwrap();
        (match map.entry(path.to_owned()) {
            hash_map::Entry::Occupied(entry) => entry.into_mut().as_mut(),
            hash_map::Entry::Vacant(entry) => entry.insert(load(path).map(Arc::new)).as_mut(),
        }.map(|x| &**x))
    }

    pub fn retrieve_shared(&self, path: &Path) -> Option<Arc<IconFile>> {
        let existing = self.lock.read().unwrap().get(path).cloned();
        // shouldn't be inlined or the lifetime of the lock will be extended
        match existing {
            Some(existing) => existing,
            None => {
                let arc = load(path).map(Arc::new);
                self.lock.write().unwrap().insert(path.to_owned(), arc.clone());
                arc
            }
        }
    }
}

fn load(path: &Path) -> Option<IconFile> {
    match IconFile::from_file(path) {
        Ok(loaded) => Some(loaded),
        Err(err) => {
            eprintln!("error loading icon: {}\n  {}", path.display(), err);
            None
        }
    }
}
