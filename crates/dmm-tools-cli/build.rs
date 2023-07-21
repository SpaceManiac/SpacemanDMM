extern crate chrono;
extern crate git2;

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut f = File::create(out_dir.join("build-info.txt")).unwrap();

    if let Ok(commit) = read_commit() {
        writeln!(f, "commit: {}", commit).unwrap();
    }
    writeln!(f, "build date: {}", chrono::Utc::now().date_naive()).unwrap();
}

fn read_commit() -> Result<String, git2::Error> {
    let repo = git2::Repository::discover(".")?;
    let hash = repo.head()?.peel_to_commit()?.id().to_string();
    Ok(hash)
}
