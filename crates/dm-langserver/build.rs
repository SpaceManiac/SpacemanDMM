extern crate chrono;
extern crate git2;

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

fn main() {
    // build info
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut f = File::create(out_dir.join("build-info.txt")).unwrap();

    match read_commit() {
        Ok(commit) => writeln!(f, "commit: {}", commit).unwrap(),
        Err(err) => println!("cargo:warning=Failed to fetch commit info: {}", err)
    }
    writeln!(f, "build date: {}", chrono::Utc::now().date_naive()).unwrap();

    // extools bundling
    println!("cargo:rustc-cfg=extools_bundle");
    download_dll(
        &out_dir,
        "extools.dll",
        "v0.0.7", // EXTOOLS_TAG
        "https://github.com/tgstation/tgstation/raw/34f0cc6394a064b87cbd1d6cb225f1d3df444ba7/byond-extools.dll", // EXTOOLS_DLL_URL
        "073dd08790a13580bae71758e9217917700dd85ce8d35cb030cef0cf5920fca8", // EXTOOLS_DLL_SHA256
    );

    // auxtools bundling
    println!("cargo:rustc-cfg=auxtools_bundle");
    download_dll(
        &out_dir,
        "debug_server.dll",
        "v2.3.3", // DEBUG_SERVER_TAG
        "https://github.com/willox/auxtools/releases/download/v2.3.3/debug_server.dll", // DEBUG_SERVER_DLL_URL
        "8c3633d8237738be39a8c8f34bbd3d819cdaa270d1f30774c5007481fc32418c", // DEBUG_SERVER_DLL_SHA256
    );
}

fn read_commit() -> Result<String, git2::Error> {
    let repo = git2::Repository::discover(".")?;
    let head = repo.head()?.peel_to_commit()?.id();

    let mut all_tags = Vec::new();
    repo.tag_foreach(|oid, _| { all_tags.push(oid); true })?;

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
        Some(ahead) => println!("cargo:rustc-env=CARGO_PKG_VERSION={}+{}", std::env::var("CARGO_PKG_VERSION").unwrap(), ahead),
    }

    Ok(head.to_string())
}

fn download_dll(out_dir: &Path, fname: &str, tag: &str, url: &str, sha256: &str) {
    let full_path = out_dir.join(fname);
    println!("cargo:rustc-env=BUNDLE_PATH_{}={}", fname, full_path.display());
    println!("cargo:rustc-env=BUNDLE_VERSION_{}={}", fname, tag);

    if let Ok(digest) = sha256::try_digest(&full_path) {
        if digest == sha256 {
            return;
        }
    }

    std::io::copy(
        &mut ureq::get(url).call().expect("Error downloading DLL to bundle").into_reader(),
        &mut std::fs::File::create(full_path).unwrap(),
    ).unwrap();
}
