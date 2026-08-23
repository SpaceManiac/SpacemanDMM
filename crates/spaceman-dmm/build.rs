use std::env;
use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    // build info
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut f = File::create(out_dir.join("build-info.txt")).unwrap();
    match read_commit() {
        Ok((commit, date)) => writeln!(f, "commit: {commit}\ndate: {date}").unwrap(),
        Err(err) => println!("cargo:warning=Failed to fetch commit info: {err}"),
    }

    // windres icon
    if cfg!(windows) {
        let out_dir = env::var("OUT_DIR").expect("can't find out_dir");

        if cfg!(target_env = "msvc") {
            if let Err(e) = Command::new("windres")
                .args(["res/editor.rc", "-o"])
                .arg(format!("{}/editor_rc.lib", out_dir))
                .status()
            {
                println!("cargo:warning=`windres` unavailable: {}", e);
                return;
            }
        } else {
            if let Err(e) = Command::new("windres")
                .args(["res/editor.rc", "-o"])
                .arg(format!("{}/editor.rc.o", out_dir))
                .status()
            {
                println!("cargo:warning=`windres` unavailable: {}", e);
                return;
            }
            Command::new("ar")
                .args(["crus", "libeditor_rc.a", "editor.rc.o"])
                .current_dir(Path::new(&out_dir))
                .status()
                .unwrap();
        }

        println!("cargo:rerun-if-changed=editor.rc");
        println!("cargo:rerun-if-changed=gasmask.ico");
        println!("cargo:rustc-link-search=native={}", out_dir);
        println!("cargo:rustc-link-lib=static=editor_rc");
    }
}

fn read_commit() -> Result<(String, String), git2::Error> {
    let repo = git2::Repository::discover(".")?;
    let commit = repo.head()?.peel_to_commit()?;
    let hash = commit.id().to_string();
    let time = chrono::DateTime::from_timestamp_secs(commit.time().seconds()).unwrap();
    Ok((hash, time.to_string()))
}
