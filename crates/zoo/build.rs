use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

fn main() {
    let out_dir = PathBuf::from(env::var_os("OUT_DIR").unwrap());
    let mut f = File::create(out_dir.join("build-info.txt")).unwrap();
    match read_base_commit() {
        Ok(commit) => write!(f, "{commit}").unwrap(),
        Err(err) => println!("cargo:warning=Failed to fetch commit info: {err}"),
    }
}

fn read_base_commit() -> Result<String, git2::Error> {
    let repo = git2::Repository::discover(".")?;
    let commit = repo.head()?.peel_to_commit()?;
    let master = repo
        .find_branch("master", git2::BranchType::Local)?
        .into_reference()
        .peel_to_commit()?;
    let base = repo.merge_base(commit.id(), master.id())?;
    let hash = base.to_string();
    Ok(hash)
}
