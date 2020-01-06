// various config options

use serde::Deserialize;
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;
use std::cmp::Ordering;
use crate::error::Severity;
use crate::DMError;

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

impl Default for WarningLevel {
    fn default() -> WarningLevel {
        WarningLevel::Unset
    }
}

impl WarningLevel {
    fn default_all() -> WarningLevel {
        WarningLevel::Hint
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

/*
config.toml
[warnings]
duplicate_include = false
should_call_parent = false
should_not_override = false
field_access_static_type = false
proc_call_static_type = false
integer_precision_loss = false
colon_path_warning = false
var_in_proc_paramater = false
static_in_proc_paramater = false
macro_redefined = false
undefine_undefined_macro = false

[display]
error_level = "hint"

print_level = "error"

*/

#[derive(Deserialize, Default, Debug, Clone)]
pub struct Config {
    #[serde(default)]
    warnings: HashMap<String, WarningLevel>,
    display: WarningDisplay,
}

impl Config {
    pub fn new() -> Config {
        Config {
            warnings: HashMap::<String, WarningLevel>::new(),
            display: WarningDisplay::default(),
        }
    }

    pub fn read_config_toml(path: String) -> Config {
        let mut config_toml = String::new();
    
        let mut file = match File::open(&path) {
            Ok(file) => file,
            Err(_)  => {
                return Config::new();
            }
        };
    
        file.read_to_string(&mut config_toml)
                .unwrap_or_else(|err| panic!("Error while reading config: [{}]", err));
    
        return toml::from_str(&config_toml)
            .unwrap_or_else(|err| panic!("TOML Error while reading config file: {}", err));
    }

    fn config_warninglevel(&self, error: &DMError) -> Option<&WarningLevel> {
        if let Some(errortype) = error.errortype() {
            return self.warnings.get(errortype)
        }
        None
    }

    pub fn printable_error(&self, error: &DMError) -> bool {
        let mut error_sev = error.severity();
        if let Some(level) = self.config_warninglevel(error) {
            match level {
                WarningLevel::Error => error_sev = Severity::Error,
                WarningLevel::Warning => error_sev = Severity::Warning,
                WarningLevel::Info => error_sev = Severity::Info,
                WarningLevel::Hint => error_sev = Severity::Hint,
                WarningLevel::Disabled => return false,
                WarningLevel::Unset => {},
            }
        }
        self.display.print_level >= error_sev
    }

    pub fn registerable_error(&self, error: &DMError) -> bool {
        let mut error_sev = error.severity();
        if let Some(level) = self.config_warninglevel(error) {
            match level {
                WarningLevel::Error => error_sev = Severity::Error,
                WarningLevel::Warning => error_sev = Severity::Warning,
                WarningLevel::Info => error_sev = Severity::Info,
                WarningLevel::Hint => error_sev = Severity::Hint,
                WarningLevel::Disabled => return false,
                WarningLevel::Unset => {},
            }
        }
        self.display.error_level >= error_sev
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

#[derive(Deserialize, Default, Debug, Clone)]
pub struct WarningDisplay {
    #[serde(default="WarningLevel::default_all")]
    error_level: WarningLevel,

    #[serde(default)]
    print_level: WarningLevel,
}
