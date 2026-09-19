use std::{
    collections::{BTreeMap, HashSet, VecDeque},
    error::Error,
    fs,
    panic::{AssertUnwindSafe, catch_unwind},
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use chrono::{Datelike, Timelike};
use clap::{ArgAction::SetTrue, Parser, Subcommand};
use dreammaker::Severity;
use git2::{BranchType, Commit, FetchOptions, RemoteCallbacks, Repository, Tree};
use serde::{Deserialize, Serialize};

mod db;

const ABBREV: usize = 12;

// ----------------------------------------------------------------------------
// CLI

pub fn main() -> Result<(), Box<dyn Error>> {
    let zoo = Zoo::parse();
    zoo.run()
}

#[derive(Parser, Debug)]
struct Zoo {
    #[arg(long = "root")]
    root: PathBuf,

    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand, Debug)]
enum Command {
    Analyze(Analyze),
}

#[derive(Parser, Debug)]
struct Analyze {
    #[arg(long = "no-pull", action = SetTrue)]
    no_pull: bool,

    #[arg(long = "max-date")]
    max_date: Option<String>,
}

// ----------------------------------------------------------------------------
// Root CLI

impl Zoo {
    pub fn run(&self) -> Result<(), Box<dyn Error>> {
        match &self.command {
            Command::Analyze(analyze) => analyze.run(self),
        }
    }
}

// ----------------------------------------------------------------------------
// Analyze

impl Analyze {
    pub fn run(&self, zoo: &Zoo) -> Result<(), Box<dyn Error>> {
        let zoo_toml: ZooToml = toml::from_slice(&std::fs::read(zoo.root.join("zoo.toml"))?)?;

        // Spaceman revisions
        eprintln!("==== SpacemanDMM revisions ====");
        let db = db::Db::open(&zoo.root);
        let spaceman_rev = db.set_spaceman_tags()?;

        // Analyze
        eprintln!("==== Analyze ====");
        let repo = Repository::open(zoo.root.join("repo"))?;
        let mut checkouts = HashSet::new();

        let max_date = match &self.max_date {
            None => None,
            Some(max_date) => Some({
                if let Ok(rev) = Repository::open_from_env()?.revparse_single(max_date) {
                    &format_git_time(
                        rev.peel(git2::ObjectType::Commit)?
                            .into_commit()
                            .unwrap()
                            .time(),
                    )
                } else {
                    max_date
                }
            }),
        };

        for (i, (codebase, info)) in zoo_toml.codebase.iter().enumerate() {
            eprintln!("-- ({}/{}) {}", i + 1, zoo_toml.codebase.len(), codebase);

            // Metadata
            let mut commit;
            if self.no_pull {
                let branch = match repo.find_branch(codebase, BranchType::Local) {
                    Ok(branch) => branch,
                    Err(e) => {
                        eprintln!("ERROR: {codebase}: {e}");
                        continue;
                    },
                };
                commit = branch.get().peel_to_commit()?;
            } else {
                match &info.source {
                    Source::Url { url } => {
                        eprintln!("pulling {:?}", url);

                        let mut cbs = RemoteCallbacks::new();
                        cbs.sideband_progress(|text| {
                            eprint!("remote: {}", String::from_utf8_lossy(text));
                            true
                        });

                        let mut remote = repo.remote_anonymous(url)?;
                        remote.fetch(
                            &[format!("HEAD:refs/heads/{codebase}")],
                            Some(FetchOptions::new().remote_callbacks(cbs)),
                            None,
                        )?;

                        commit = repo
                            .find_branch(codebase, BranchType::Local)?
                            .get()
                            .peel_to_commit()?;
                    },
                    Source::Commit { commit: sha } => {
                        commit = repo.find_commit_by_prefix(sha)?;
                    },
                }
                repo.branch(codebase, &commit, true)?;
            }

            if let Some(max_date) = &max_date {
                commit = with_max_date(&commit, max_date);
            }

            let commit_sha = commit.id().to_string();
            let tree = commit.tree()?;
            let tree_sha = tree.id().to_string();
            db.insert_tree_with(&tree_sha, || {
                let path = checkout(&mut checkouts, &zoo.root, &repo, &tree);
                let suite = discover_suite(&path);
                let config = discover_config(&path);
                eprintln!(
                    "commit = {}, suite = {:?}, config = {:?}",
                    &commit_sha[..ABBREV],
                    suite,
                    config.as_ref().map(|x| x.len()),
                );
                (suite, config)
            })?;
            db.insert_commit(&commit)?;
            db.insert_codebase(codebase, info.url())?;
            db.insert_codebase_commit(codebase, &commit_sha)?;

            // Analysis
            db.insert_tree_analysis_with(&tree_sha, &spaceman_rev, || {
                let path = checkout(&mut checkouts, &zoo.root, &repo, &tree);
                match catch_unwind(AssertUnwindSafe(|| {
                    let analysis = analyze(&path, info.environment.as_deref()).unwrap();
                    eprintln!(
                        "{:?}",
                        Analysis {
                            errors: Default::default(),
                            ..analysis
                        },
                    );
                    serde_json::to_string(&analysis).unwrap()
                })) {
                    Ok(v) => v,
                    Err(e) => serde_json::to_string(&Panic::new(e.as_ref())).unwrap(),
                }
            })?;
        }

        Ok(())
    }
}

fn discover_suite(path: &Path) -> Option<String> {
    for fname in [
        "dependencies.sh",
        "_build_dependencies.sh",
        ".gitlab-ci.yml",
        "buildByond.conf",
        "tools/travis/install_spaceman_dmm.sh",
    ] {
        if let Ok(contents) = fs::read_to_string(path.join(fname))
            && let Some(start) = contents.find("suite-")
            && let Some(len) =
                contents[start..].find(|ch: char| !(ch.is_alphanumeric() || ch == '-' || ch == '.'))
        {
            return Some(contents[start..start + len].to_owned());
        }
    }
    None
}

fn discover_config(path: &Path) -> Option<String> {
    fs::read_to_string(path.join("SpacemanDMM.toml"))
        .ok()
        .filter(|text| !text.starts_with("#FAKE"))
}

fn analyze(path: &Path, override_dme: Option<&str>) -> Result<Analysis, Box<dyn Error>> {
    let mut context = dreammaker::Context::default();
    let dme = match (context.configure(path), override_dme) {
        (_, Some(forced)) => path.join(forced),
        (Ok(detected), None) => detected,
        (Err(err), None) => return Err(err.into()),
    };

    // Preprocess once to get files in cache.
    let start = Instant::now();
    let pp = dreammaker::Preprocessor::new(&context, dme.clone())?;
    pp.count();
    let precache_time = start.elapsed();

    // Parse while it's cached for an accurate measurement.
    context.reset_io_time();
    let start = Instant::now();
    let pp = dreammaker::Preprocessor::new(&context, dme)?;
    let mut parser = dreammaker::Parser::new(&context, pp);
    parser.enable_procs();
    let (fatal_errored, objtree) = parser.parse_object_tree_2();
    let io_time = context.get_io_time();
    let parse_time = start.elapsed().saturating_sub(io_time);
    let parse_errors = ErrorCounts::new(&context);

    // Run dreamchecker.
    let dreamchecker_voluntary = context.config().langserver.dreamchecker;
    let start = Instant::now();
    if !fatal_errored {
        dreamchecker::run(&context, &objtree);
    }
    let dreamchecker_time = start.elapsed();
    let dreamchecker_errors = ErrorCounts::new(&context) - parse_errors;

    let mut errors = Vec::new();
    for error in context.errors().iter() {
        if error.severity() <= Severity::Info {
            context
                .pretty_print_error(&mut termcolor::Ansi::new(&mut errors), error)
                .unwrap();
        }
    }
    let errors = String::from_utf8_lossy(&errors).into_owned();

    Ok(Analysis {
        precache_time,
        io_time,
        parse_time,
        parse_errors,
        fatal_errored,
        dreamchecker_time,
        dreamchecker_errors,
        dreamchecker_voluntary,
        errors,
    })
}

#[derive(Debug, Serialize, Deserialize)]
struct Panic {
    panic: String,
}

impl Panic {
    fn new(e: &(dyn std::any::Any + Send + 'static)) -> Self {
        if let Some(panic) = e.downcast_ref::<&str>() {
            Panic { panic: panic.to_string() }
        } else if let Some(panic) = e.downcast_ref::<String>() {
            Panic { panic: panic.to_string() }
        } else {
            panic!("couldn't get panic message")
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
struct Analysis {
    precache_time: Duration,
    io_time: Duration,
    parse_time: Duration,
    parse_errors: ErrorCounts,
    fatal_errored: bool,
    dreamchecker_time: Duration,
    dreamchecker_errors: ErrorCounts,
    dreamchecker_voluntary: bool,
    errors: String,
}

#[derive(Debug, Default, Clone, Copy, Serialize, Deserialize)]
struct ErrorCounts {
    hint: usize,
    info: usize,
    warning: usize,
    error: usize,
}

impl ErrorCounts {
    fn new(context: &dreammaker::Context) -> ErrorCounts {
        let mut this = ErrorCounts::default();
        for each in context.errors().iter() {
            match each.severity() {
                dreammaker::Severity::Error => this.error += 1,
                dreammaker::Severity::Warning => this.warning += 1,
                dreammaker::Severity::Info => this.info += 1,
                dreammaker::Severity::Hint => this.hint += 1,
            }
        }
        this
    }
}

impl std::ops::Sub<ErrorCounts> for ErrorCounts {
    type Output = ErrorCounts;

    fn sub(self, rhs: ErrorCounts) -> Self::Output {
        ErrorCounts {
            hint: self.hint - rhs.hint,
            info: self.info - rhs.info,
            warning: self.warning - rhs.warning,
            error: self.error - rhs.error,
        }
    }
}

fn format_git_time(time: git2::Time) -> String {
    let time = chrono::DateTime::from_timestamp_secs(time.seconds()).unwrap();
    format!(
        "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
        time.year(),
        time.month(),
        time.day(),
        time.hour(),
        time.minute(),
        time.second()
    )
}

fn with_max_date<'repo>(commit: &Commit<'repo>, max_date: &str) -> Commit<'repo> {
    let mut best: Option<Commit<'repo>> = None;
    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(commit.clone());
    while let Some(current) = queue.pop_front() {
        for each in current.parents() {
            if seen.insert(each.id()) {
                queue.push_back(each);
            }
        }

        if format_git_time(current.time()).as_str() < max_date {
            if best
                .as_ref()
                .is_none_or(|best| current.time() > best.time())
            {
                best = Some(current);
            }
        }
    }
    best.unwrap_or(commit.clone())
}

// ----------------------------------------------------------------------------
// Checkouts

fn checkout(set: &mut HashSet<String>, root: &Path, repo: &Repository, tree: &Tree) -> PathBuf {
    let tree_sha = tree.id().to_string();
    let path = root.join("tree").join(&tree_sha);
    if set.insert(tree_sha.clone()) {
        eprintln!("checking out {path:?}");
        _ = std::fs::remove_dir_all(&path);
        std::fs::create_dir_all(&path).unwrap();
        // let worktree = repo.worktree(&tree_sha, &path, None).unwrap();
        repo.set_workdir(&path, false).unwrap();
        repo.checkout_tree(tree.as_object(), None).unwrap();
    }
    path
}

// ----------------------------------------------------------------------------
// Config

#[derive(Debug, Deserialize)]
struct ZooToml {
    codebase: BTreeMap<String, Codebase>,
}

#[derive(Debug, Deserialize)]
struct Codebase {
    #[serde(flatten)]
    source: Source,

    environment: Option<String>,
}

impl Codebase {
    fn url(&self) -> Option<&str> {
        match &self.source {
            Source::Url { url } => Some(url),
            _ => None,
        }
    }
}

#[derive(Debug, Deserialize)]
#[serde(untagged)]
enum Source {
    Url { url: String },
    Commit { commit: String },
}
