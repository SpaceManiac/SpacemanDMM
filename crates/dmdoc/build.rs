use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut f = File::create(out_dir.join("build-info.txt")).unwrap();
    match read_commit() {
        Ok((commit, date)) => writeln!(f, "commit: {commit}\ndate: {date}").unwrap(),
        Err(err) => println!("cargo:warning=Failed to fetch commit info: {err}"),
    }
}

fn read_commit() -> Result<(String, String), git2::Error> {
    let repo = git2::Repository::discover(".")?;
    let commit = repo.head()?.peel_to_commit()?;
    let hash = commit.id().to_string();
    let time = chrono::DateTime::from_timestamp_secs(commit.time().seconds()).unwrap();
    Ok((hash, time.to_string()))
}
