#![forbid(unsafe_code)]
extern crate dreammaker as dm;

use std::{path::PathBuf, process::ExitCode};

use clap::{ArgAction::SetTrue, Parser};

fn main() -> std::process::ExitCode {
    DreamCheckerCli::parse().run()
}

#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

// ----------------------------------------------------------------------------
// CLI driver

#[derive(clap::Parser, Debug)]
#[command(
    name="dreamchecker",
    version=env!("CARGO_PKG_VERSION"),
    long_version=concat!(
        env!("CARGO_PKG_VERSION"), "  Copyright (C) 2017-2026  Tad Hardesty", "\n",
        "This program comes with ABSOLUTELY NO WARRANTY. This is free software,
and you are welcome to redistribute it under the conditions of the GNU
General Public License version 3.", "\n",
        "\n",
        include_str!(concat!(env!("OUT_DIR"), "/build-info.txt")),
    ),
)]
pub struct DreamCheckerCli {
    /// The environment to load, usually a `.dme` file.
    ///
    /// May also point to a `SpacemanDMM.toml` or to a directory containing
    /// a `.dme` file or a `SpacemanDMM.toml`.
    #[arg(short = 'e', long = "env", default_value = ".")]
    environment: PathBuf,

    /// Also print a JSON object summarizing the results to stdout.
    #[arg(long = "json", action = SetTrue)]
    json: bool,

    /// Only run the parser, skipping full DreamChecker analysis.
    #[arg(long = "parse-only", action = SetTrue)]
    parse_only: bool,
}

impl DreamCheckerCli {
    pub fn run(&self) -> ExitCode {
        let mut context = dm::Context::default();
        context.set_print_severity(Some(dm::Severity::Info));
        let dme = context.configure_cli(&self.environment);

        println!("============================================================");
        println!("Parsing {}...\n", dme.display());
        let pp = context.unwrap(dm::Preprocessor::new(&context, dme));
        let mut parser = dm::Parser::new(&context, pp);
        parser.enable_procs();
        let (fatal_errored, tree) = parser.parse_object_tree_2();

        if !self.parse_only && !fatal_errored {
            dreamchecker::run_cli(&context, &tree);
        }

        println!("============================================================");
        let errors = context
            .errors()
            .iter()
            .filter(|each| each.severity() <= dm::Severity::Info)
            .count();
        println!("Found {errors} diagnostics");

        if self.json {
            serde_json::to_writer(std::io::stdout().lock(), &serde_json::json! {{
                "hint": context.errors().iter().filter(|each| each.severity() == dm::Severity::Hint).count(),
                "info": context.errors().iter().filter(|each| each.severity() == dm::Severity::Info).count(),
                "warning": context.errors().iter().filter(|each| each.severity() == dm::Severity::Warning).count(),
                "error": context.errors().iter().filter(|each| each.severity() == dm::Severity::Error).count(),
            }}).unwrap();
        }

        if errors > 0 {
            ExitCode::FAILURE
        } else {
            ExitCode::SUCCESS
        }
    }
}
