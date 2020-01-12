// various config options

use serde::Deserialize;
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;
use std::cmp::Ordering;
use crate::error::Severity;
use crate::DMError;

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
    display: WarningDisplay,

    #[serde(default)]
    warnings: HashMap<String, WarningLevel>,
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct WarningDisplay {
    #[serde(default="WarningLevel::default_all")]
    error_level: WarningLevel,

    #[serde(default)]
    print_level: WarningLevel,
}

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
    pub fn new() -> Config {
        Config::default()
    }

    pub fn read_config_toml(path: String) -> Result<Config, Box<dyn std::error::Error>> {
        let mut file = match File::open(&path) {
            Ok(file) => file,
            Err(_)  => return Ok(Config::default()),
        };

        let mut config_toml = String::new();
        file.read_to_string(&mut config_toml)?;
        Ok(toml::from_str(&config_toml)?)
    }

    fn config_warninglevel(&self, error: &DMError) -> Option<&WarningLevel> {
        if let Some(errortype) = error.errortype() {
            return self.warnings.get(errortype)
        }
        None
    }

    pub fn set_configured_severity(&self, error: DMError) -> Option<DMError> {
        let newerror = match self.config_warninglevel(&error) {
            Some(WarningLevel::Error) => error.set_severity(Severity::Error),
            Some(WarningLevel::Warning) => error.set_severity(Severity::Warning),
            Some(WarningLevel::Info) => error.set_severity(Severity::Info),
            Some(WarningLevel::Hint) => error.set_severity(Severity::Hint),
            Some(WarningLevel::Disabled) => return None,
            Some(_) | None => error,
        };
        Some(newerror)
    }

    pub fn printable_error(&self, error: &DMError) -> bool {
        self.display.print_level >= error.severity()
    }

    pub fn registerable_error(&self, error: &DMError) -> bool {
        self.display.error_level >= error.severity()
    }

    pub fn set_print_severity(&mut self, print_severity: Option<Severity>) {
        if self.display.print_level != WarningLevel::Unset {
            return
        }
        match print_severity {
            Some(severity) => self.display.print_level = WarningLevel::from(severity),
            None => self.display.print_level = WarningLevel::Disabled,
        }
    }
}

impl WarningLevel {
    fn default_all() -> WarningLevel {
        WarningLevel::Hint
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

impl PartialOrd<Severity> for WarningLevel {
    fn partial_cmp(&self, other: &Severity) -> Option<Ordering> {
        match self {
            WarningLevel::Disabled | WarningLevel::Unset => None,
            _ => Some((*self as u8).cmp(&(*other as u8))),
        }
    }
}
