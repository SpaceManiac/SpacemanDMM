//! Configuration file for diagnostics.

use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::collections::HashMap;

use serde::Deserialize;

use crate::error::Severity;
use crate::DMError;

/// Struct for deserializing from a config TOML
#[derive(Deserialize, Default, Debug, Clone)]
#[serde(default)]
pub struct Config {
    pub environment: Option<PathBuf>,

    // diagnostic configuration
    display: WarningDisplay,
    diagnostics: HashMap<String, WarningLevel>,
    pub code_standards: CodeStandards,

    // tool-specific configuration
    pub langserver: Langserver,
    pub dmdoc: DMDoc,
}

/// General error display options
#[derive(Deserialize, Default, Debug, Clone)]
pub struct WarningDisplay {
    #[serde(default)]
    error_level: WarningLevel,
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
pub struct DMDoc {
    pub use_typepath_names: bool,
}

/// Severity overrides from configuration
#[derive(Debug, Deserialize, Clone, Copy, PartialEq)]
#[serde(rename_all(deserialize = "lowercase"))]
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
    Unset = 6,
}

impl Config {
    /// Read a config TOML and generate a [`Config`] struct
    ///
    /// [`Config`]: struct.Config.html
    pub fn read_toml(path: &Path) -> Result<Config, Error> {
        let mut file = File::open(path)?;
        let mut config_toml = String::new();
        file.read_to_string(&mut config_toml)?;
        Ok(toml::from_str(&config_toml)?)
    }

    fn config_warninglevel(&self, error: &DMError) -> Option<&WarningLevel> {
        if let Some(errortype) = error.errortype() {
            return self.diagnostics.get(errortype)
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
            Some(WarningLevel::Error) => error.set_severity(Severity::Error),
            Some(WarningLevel::Warning) => error.set_severity(Severity::Warning),
            Some(WarningLevel::Info) => error.set_severity(Severity::Info),
            Some(WarningLevel::Hint) => error.set_severity(Severity::Hint),
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

impl Default for WarningLevel {
    fn default() -> WarningLevel {
        WarningLevel::Unset
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
        match (self, other) {
            (WarningLevel::Error, Severity::Error) => true,
            (WarningLevel::Warning, Severity::Warning) => true,
            (WarningLevel::Info, Severity::Info) => true,
            (WarningLevel::Hint, Severity::Hint) => true,
            _ => false,
        }
    }
}

/// Config parse error
#[derive(Debug)]
pub enum Error {
    Io(std::io::Error),
    Toml(toml::de::Error),
}

impl Error {
    pub fn line_col(&self) -> Option<(u32, u16)> {
        match self {
            Error::Io(_) => None,
            Error::Toml(toml) => toml.line_col().map(|(l, c)| (l as u32 + 1, c as u16 + 1)),
        }
    }

    pub fn into_boxed_error(self) -> Box<dyn std::error::Error + Send + Sync> {
        match self {
            Error::Io(err) => Box::new(err),
            Error::Toml(err) => Box::new(err),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<toml::de::Error> for Error {
    fn from(err: toml::de::Error) -> Error {
        Error::Toml(err)
    }
}
