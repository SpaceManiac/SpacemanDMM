use std::{
    collections::HashMap,
    error::Error,
    fs,
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

use chrono::{Datelike, Timelike};
use clap::{ArgAction::SetTrue, Parser, Subcommand};
use dreammaker::Severity;
use git2::Repository;
use rusqlite::{OptionalExtension, named_params};
use serde::{Deserialize, Serialize};

mod db;

const COMMIT: &str = include_str!(concat!(env!("OUT_DIR"), "/build-info.txt"));

// ----------------------------------------------------------------------------
// CLI

pub fn main() -> Result<(), Box<dyn Error>> {
    let zoo = Zoo::parse();
    zoo.run()
}

#[derive(Parser, Debug)]
struct Zoo {
    #[arg(long = "repos")]
    repos: PathBuf,

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
        let db = db::Db::open(&zoo.repos);
        eprintln!("==== SpacemanDMM revisions ====");
        db.set_spaceman_tags()?;

        let mut paths = Vec::new();
        for entry in fs::read_dir(&zoo.repos)? {
            let entry = entry?;
            let name = entry.file_name();
            let name = name.to_string_lossy();
            if name.starts_with(".") {
                continue;
            }
            let path = entry.path();
            if !fs::metadata(&path)?.is_dir() {
                continue;
            }
            paths.push(path);
        }
        paths.sort();

        // Pull everything
        eprintln!("==== Pull ====");
        let mut remote_and_commit = HashMap::new();
        let mut dates = HashMap::new();
        for (i, path) in paths.iter().enumerate() {
            let repo = Repository::open(&path)?;
            let short_path = path.strip_prefix(&zoo.repos).unwrap_or(path);
            let codebase = short_path.to_string_lossy();

            // Discover upstream and pull
            assert!(!repo.head_detached()?, "head detached: {:?}", path);
            let mut head = repo.head()?;
            assert!(head.is_branch());
            let mut git_rev = String::new();
            match repo.branch_upstream_remote(head.name()?) {
                Ok(remote) => {
                    let remote_name = remote.as_str().unwrap();
                    let remote_branch = repo.branch_upstream_name(head.name()?)?;
                    let remote_branch = remote_branch.as_str().unwrap();

                    let mut remote = repo.find_remote(remote_name)?;
                    if !self.no_pull {
                        eprintln!("{:?}: pulling", short_path);
                        let refspecs = remote.fetch_refspecs()?;
                        let refspecs = refspecs
                            .into_iter()
                            .filter_map(|s| s.ok())
                            .filter_map(|s| s)
                            .collect::<Vec<_>>();
                        match remote.fetch(&refspecs, None, None) {
                            Ok(()) => {
                                head = repo.find_reference(remote_branch)?;
                                repo.reset(
                                    head.peel_to_commit()?.as_object(),
                                    git2::ResetType::Hard,
                                    None,
                                )?;
                            },
                            Err(e) => {
                                eprintln!("WARNING: {:?}: not fetchable: {}", short_path, e);
                            },
                        }
                    }

                    git_rev = head.peel_to_commit()?.id().to_string();
                    remote_and_commit
                        .insert(path.as_path(), (remote.url()?.to_string(), git_rev.clone()));
                },
                Err(_) => {
                    eprintln!(
                        "WARNING: {:?}: no upstream for branch {:?}",
                        short_path,
                        head.name()?
                    );
                },
            }

            // Extract commit timestamp
            let date = format_git_time(head.peel_to_commit()?.time());
            dates.insert(path.as_path(), date);

            // Extract configuration
            let existing_row = db
                .query_one(
                    "SELECT * FROM config WHERE codebase = :codebase AND git_rev = :git_rev",
                    named_params! {
                        ":codebase": codebase,
                        ":git_rev": git_rev,
                    },
                    |_| Ok(()),
                )
                .optional()?;
            if existing_row.is_none() {
                let suite = discover_suite(path);
                let config = discover_config(path);
                db.execute(
                    "
                        INSERT INTO config (
                            codebase,
                            git_rev,
                            suite,
                            config
                        ) VALUES (
                            :codebase,
                            :git_rev,
                            :suite,
                            :config
                        );
                    ",
                    named_params! {
                        ":codebase": codebase,
                        ":git_rev": git_rev,
                        ":suite": suite,
                        ":config": config,
                    },
                )?;
                eprintln!(
                    "({}/{}) {:?}: updated to {}, suite = {:?}, config = {:?}",
                    i + 1,
                    paths.len(),
                    short_path,
                    &git_rev[..12],
                    suite,
                    config.as_ref().map(|x| x.len()),
                );
            } else if !self.no_pull {
                eprintln!("({}/{}) {:?}: up to date", i + 1, paths.len(), short_path);
            } else {
                eprintln!("({}/{}) {:?}: not updated", i + 1, paths.len(), short_path);
            }
        }

        // Analyze
        eprintln!("==== Analyze ====");

        eprintln!("SpacemanDMM commit: {}", COMMIT);

        let mut paths_by_date = paths.iter().map(|p| p.as_path()).collect::<Vec<_>>();
        paths_by_date.sort_by_key(|p| dates.get(p));
        paths_by_date.reverse();

        for (i, &path) in paths_by_date.iter().enumerate() {
            let short_path = path.strip_prefix(&zoo.repos).unwrap_or(path);
            let codebase = short_path.to_string_lossy();
            let (git_remote, git_rev) = remote_and_commit
                .get(path)
                .map_or(("", ""), |(a, b)| (&a, &b));
            let date = dates.get(path).unwrap();

            let existing_row = db
                .query_one(
                    "
                SELECT *
                FROM analysis
                WHERE codebase = :codebase
                    AND spaceman_rev = :spaceman_rev
                    AND git_rev = :git_rev;
            ",
                    named_params! {
                        ":spaceman_rev": COMMIT,
                        ":codebase": codebase,
                        ":git_rev": git_rev,
                    },
                    |_| Ok(()),
                )
                .optional()?;
            if existing_row.is_some() {
                eprintln!(
                    "({}/{}) {:?}: cached",
                    (i + 1),
                    paths_by_date.len(),
                    short_path,
                );
            } else {
                match std::panic::catch_unwind(|| analyze(path)) {
                    Ok(Ok(analysis)) => {
                        let json = serde_json::to_string(&analysis)?;

                        db.execute(
                            "
                                INSERT INTO analysis (
                                    codebase,
                                    spaceman_rev,
                                    git_remote,
                                    git_rev,
                                    git_date,
                                    analysis
                                ) VALUES (
                                    :codebase,
                                    :spaceman_rev,
                                    :git_remote,
                                    :git_rev,
                                    :git_date,
                                    :analysis
                                );
                            ",
                            named_params! {
                                ":codebase": codebase,
                                ":spaceman_rev": COMMIT,
                                ":git_remote": git_remote,
                                ":git_rev": git_rev,
                                ":git_date": date,
                                ":analysis": &json,
                            },
                        )?;

                        eprintln!(
                            "({}/{}) {:?}: json size = {}",
                            (i + 1),
                            paths_by_date.len(),
                            short_path,
                            json.len(),
                        );
                    },
                    Ok(Err(err)) => {
                        eprintln!("ERROR: {:?}: {}", short_path, err);
                    },
                    Err(_) => {
                        eprintln!("ERROR: {:?}: panicked", short_path);
                    },
                }
            }
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

fn analyze(path: &Path) -> Result<Analysis, Box<dyn Error>> {
    let mut context = dreammaker::Context::default();
    let dme = context.configure(path)?;

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
