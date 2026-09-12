//! Configuration file for diagnostics.

use foldhash::HashMap;
use std::path::{Path, PathBuf};

use serde::Deserialize;

use crate::error::Severity;
use crate::lexer::{LocationTracker, buffer_file};
use crate::{DMError, Location};

/// Struct for deserializing from a config TOML
#[derive(Deserialize, Default, Debug, Clone)]
#[serde(default)]
pub struct Config {
    pub(crate) environment: Option<PathBuf>,

    // diagnostic configuration
    display: WarningDisplay,
    diagnostics: HashMap<String, WarningLevel>,
    pub code_standards: CodeStandards,

    // tool-specific configuration
    pub dreamchecker: DreamChecker,
    pub langserver: Langserver,
    pub dmdoc: DMDoc,
    pub debugger: Debugger,
    pub map_renderer: MapRenderer,
}

/// General error display options
#[derive(Deserialize, Default, Debug, Clone)]
pub struct WarningDisplay {
    #[serde(default)]
    error_level: WarningLevel,
}

/// DreamChecker config options.
#[derive(Deserialize, Debug, Clone)]
#[serde(default)]
pub struct DreamChecker {
    /// Version of the `SpacemanDMM_should_not_sleep` analysis.
    pub sleep_analysis_version: u8,
}

impl Default for DreamChecker {
    fn default() -> Self {
        Self {
            sleep_analysis_version: 2,
        }
    }
}

/// Langserver config options
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Langserver {
    pub dreamchecker: bool,
}

/// Extremely opinionated linter config options
#[derive(Deserialize, Default, Debug, Clone)]
#[serde(default)]
pub struct CodeStandards {
    pub disallow_relative_proc_definitions: bool,
    pub disallow_relative_type_definitions: bool,
}

/// DMDoc config options
#[derive(Deserialize, Default, Debug, Clone)]
#[serde(default)]
pub struct DMDoc {
    pub use_typepath_names: bool,
    pub index_file: Option<String>,
    pub module_directories: Vec<String>,
}

// Debugger config options
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Debugger {
    #[serde(default)]
    pub engine: DebugEngine,
}

/// Severity overrides from configuration
#[derive(Debug, Deserialize, Clone, Copy, PartialEq)]
#[serde(rename_all(deserialize = "lowercase"))]
#[derive(Default)]
pub enum WarningLevel {
    #[serde(alias = "errors")]
    Error = 1,
    #[serde(alias = "warnings")]
    Warning = 2,
    #[serde(alias = "infos")]
    Info = 3,
    #[serde(alias = "hints")]
    Hint = 4,
    #[serde(alias = "false", alias = "off")]
    Disabled = 5,
    #[default]
    Unset = 6,
}

/// Available debug engines.
#[derive(Debug, Default, Deserialize, Clone, Copy, PartialEq)]
pub enum DebugEngine {
    #[serde(alias = "extools")]
    Extools,
    #[serde(alias = "auxtools")]
    #[default]
    Auxtools,
}

/// Config for the map renderer.
#[derive(Debug, Default, Deserialize, Clone)]
#[serde(default)]
pub struct MapRenderer {
    /// Map from render pass name to whether it should be enabled/disabled.
    ///
    /// Priority is: CLI arguments > config > defaults.
    pub render_passes: HashMap<String, bool>,

    /// Map from typepath to layer number.
    pub fancy_layers: HashMap<String, f32>,

    /// List of typepath to just hide
    pub hide_invisible: Vec<String>,
}

impl Config {
    /// Read a config TOML and generate a [`Config`] struct
    ///
    /// [`Config`]: struct.Config.html
    pub fn read_toml(file: crate::FileId, path: &Path) -> Result<Config, DMError> {
        let config_toml = buffer_file(file, path)?;
        toml::from_slice(&config_toml).map_err(|e| {
            DMError::new(
                match dbg!(&e).span() {
                    Some(span) => LocationTracker::count_location(file, &config_toml[..span.start])
                        .add_columns(1),
                    None => Location {
                        file,
                        line: 1,
                        column: 1,
                    },
                },
                e.message(),
            )
            // No `with_cause` since its Display is mostly redundant with ours
        })
    }

    fn config_warninglevel(&self, error: &DMError) -> Option<&WarningLevel> {
        if let Some(errortype) = error.errortype() {
            return self.diagnostics.get(errortype);
        }
        None
    }

    /// Return a new [`DMError`] with the configured [`Severity`] or [`None`] if disabled
    ///
    /// [`DMError`]: ../struct.DMError.html
    /// [`Severity`]: ../enum.Severity.html
    /// [`None`]: ../../std/option/enum.Option.html#variant.None
    pub fn set_configured_severity(&self, error: DMError) -> Option<DMError> {
        Some(match self.config_warninglevel(&error) {
            Some(WarningLevel::Error) => error.with_severity(Severity::Error),
            Some(WarningLevel::Warning) => error.with_severity(Severity::Warning),
            Some(WarningLevel::Info) => error.with_severity(Severity::Info),
            Some(WarningLevel::Hint) => error.with_severity(Severity::Hint),
            Some(WarningLevel::Disabled) => return None,
            Some(WarningLevel::Unset) | None => error,
        })
    }

    /// Test the error against the configured error level threshold
    pub fn registerable_error(&self, error: &DMError) -> bool {
        self.display.error_level.applies_to(error.severity())
    }
}

impl WarningLevel {
    fn applies_to(self, severity: Severity) -> bool {
        match self {
            WarningLevel::Disabled => false,
            WarningLevel::Error => severity <= Severity::Error,
            WarningLevel::Warning => severity <= Severity::Warning,
            WarningLevel::Info => severity <= Severity::Info,
            WarningLevel::Hint => severity <= Severity::Hint,
            WarningLevel::Unset => true,
        }
    }
}

impl From<Severity> for WarningLevel {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Error => WarningLevel::Error,
            Severity::Warning => WarningLevel::Warning,
            Severity::Info => WarningLevel::Info,
            Severity::Hint => WarningLevel::Hint,
        }
    }
}

impl PartialEq<Severity> for WarningLevel {
    fn eq(&self, other: &Severity) -> bool {
        matches!(
            (self, other),
            (WarningLevel::Error, Severity::Error)
                | (WarningLevel::Warning, Severity::Warning)
                | (WarningLevel::Info, Severity::Info)
                | (WarningLevel::Hint, Severity::Hint)
        )
    }
}
