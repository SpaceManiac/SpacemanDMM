extern crate chrono;
extern crate git2;

use std::io::Write;
use std::fs::File;
use std::env;
use std::path::PathBuf;

fn main() {
    // build info
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut f = File::create(&out_dir.join("build-info.txt")).unwrap();

    if let Ok(commit) = read_commit() {
        writeln!(f, "commit: {}", commit).unwrap();
    }
    writeln!(f, "build date: {}", chrono::Utc::today()).unwrap();

    // extools bundling
    println!("cargo:rerun-if-env-changed=EXTOOLS_BUNDLE_DLL");
    if env::var_os("EXTOOLS_BUNDLE_DLL").is_some() {
        println!("cargo:rustc-cfg=extools_bundle");
    }

    // auxtools bundling
    println!("cargo:rerun-if-env-changed=AUXTOOLS_BUNDLE_DLL");
    if env::var_os("AUXTOOLS_BUNDLE_DLL").is_some() {
        println!("cargo:rustc-cfg=auxtools_bundle");
    }
}

fn read_commit() -> Result<String, git2::Error> {
    let repo = git2::Repository::discover(".")?;
    let hash = repo.head()?.peel_to_commit()?.id().to_string();
    Ok(hash)
}
