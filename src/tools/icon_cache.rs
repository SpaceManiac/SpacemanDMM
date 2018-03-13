use std::sync::{Arc, RwLock};
use std::path::{Path, PathBuf};
use std::collections::{HashMap, hash_map};

use super::dmi::IconFile;

#[derive(Default)]
pub struct IconCache {
    lock: RwLock<HashMap<PathBuf, Arc<IconFile>>>,
}

impl IconCache {
    pub fn retrieve_uniq(&mut self, path: &Path) -> Option<&IconFile> {
        let map = self.lock.get_mut().unwrap();
        match map.entry(path.to_owned()) {
            hash_map::Entry::Occupied(entry) => Some(entry.into_mut()),
            hash_map::Entry::Vacant(entry) => load(path).map(|icon| &**entry.insert(Arc::new(icon))),
        }
    }

    pub fn retrieve_shared(&self, path: &Path) -> Option<Arc<IconFile>> {
        let existing = self.lock.read().unwrap().get(path).cloned();
        // shouldn't be inlined or the lifetime of the lock will be extended
        match existing {
            Some(existing) => Some(existing),
            None => load(path).map(|icon| {
                let arc = Arc::new(icon);
                self.lock.write().unwrap().insert(path.to_owned(), arc.clone());
                arc
            }),
        }
    }
}

fn load(path: &Path) -> Option<IconFile> {
    match IconFile::from_file(path) {
        Ok(loaded) => Some(loaded),
        Err(err) => {
            eprintln!("error loading icon: {}\n{}", path.display(), err);
            None
        }
    }
}
