extern crate chrono;
extern crate git2;

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    // build info
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut f = File::create(&out_dir.join("build-info.txt")).unwrap();

    match read_commit() {
        Ok(commit) => writeln!(f, "commit: {}", commit).unwrap(),
        Err(err) => println!("cargo:warning=Failed to fetch commit info: {}", err),
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
    let head = repo.head()?.peel_to_commit()?.id();

    let mut all_tags = Vec::new();
    repo.tag_foreach(|oid, _| {
        all_tags.push(oid);
        true
    })?;

    let mut best = None;
    for tag_id in all_tags {
        let tag_commit = repo.find_tag(tag_id)?.as_object().peel_to_commit()?.id();
        let (ahead, behind) = repo.graph_ahead_behind(head, tag_commit)?;
        if behind == 0 {
            match best {
                None => best = Some(ahead),
                Some(prev) if ahead < prev => best = Some(ahead),
                _ => {}
            }
        }
        if ahead == 0 {
            break;
        }
    }

    match best {
        None | Some(0) => {}
        Some(ahead) => println!(
            "cargo:rustc-env=CARGO_PKG_VERSION={}+{}",
            std::env::var("CARGO_PKG_VERSION").unwrap(),
            ahead
        ),
    }

    Ok(head.to_string())
}
