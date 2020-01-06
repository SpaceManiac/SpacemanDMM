// various config options

use serde::Deserialize;
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;
use crate::error::Severity;
use crate::DMError;

#[derive(Debug, Deserialize, Clone, Copy)]
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
/*
impl<'de> Deserialize<'de> for WarningLevel {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?.to_lowercase();
        match s.as_str() {
            "error" | "errors" => Ok(WarningLevel::Error),
            "warning" | "warnings" => Ok(WarningLevel::Warning),
            "info" | "infos" => Ok(WarningLevel::Info),
            "hint" | "hints" => Ok(WarningLevel::Hint),
            "false" | "off" => Ok(WarningLevel::Disabled),
            other => Err(serde::de::Error::custom(format!("invalid warning level value: {}", other))),
        }
    }
}*/

/*
config.toml
[warnings]
duplicate_includes: false

[display]
error_level: "hint"

print_level: "error"

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
        guard!(let Some(print_level) = self.display.print_level else {
            return false
        });
        error_sev <= print_level
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
        error_sev <= self.display.error_level
    }

    pub fn set_print_severity(&mut self, print_severity: Option<Severity>) {
        self.display.print_level = print_severity
    }
}

#[derive(Deserialize, Default, Debug, Clone)]
pub struct WarningDisplay {
    #[serde(default="Severity::default_all")]
    error_level: Severity,

    #[serde(default="Severity::default_disabled")]
    print_level: Option<Severity>,
}

/*
#[derive(Deserialize, Default, Debug, Clone)]
pub struct Warnings {
    #[serde(default)]
    duplicate_includes: WarningLevel,

    #[serde(default)]
    should_call_parent: WarningLevel,

    #[serde(default)]
    should_not_override: WarningLevel,

    #[serde(default)]
    field_access_static_type: WarningLevel,

    #[serde(default)]
    proc_call_static_type: WarningLevel,

    #[serde(default)]
    integer_precision_loss: WarningLevel,

    #[serde(default)]
    colon_path_warning: WarningLevel,

    #[serde(default)]
    var_in_proc_paramater: WarningLevel,

    #[serde(default)]
    static_in_proc_paramater: WarningLevel,

    #[serde(default)]
    macro_redefined: WarningLevel,

    #[serde(default)]
    undefine_undefined_macro: WarningLevel,
}

impl Warnings {
    pub fn warning_level_for(&self, errortype: &'static str) -> WarningLevel {
        match errortype {
            "duplicate_include" => { return self.duplicate_includes },
            "must_call_parent" => { return self.should_call_parent },
            "must_not_override" => { return self.should_not_override },
            "field_access_static_type" => { return self.field_access_static_type },
            "proc_call_static_type" => { return self.proc_call_static_type },
            "integer_precision_loss" => { return self.integer_precision_loss },
            "colon_path_warning" => { return self.colon_path_warning },
            "var_in_proc_paramater" => { return self.var_in_proc_paramater },
            "static_in_proc_parameter" => { return self.var_in_proc_paramater },
            "macro_redefined" => { return self.macro_redefined },
            "undefine_undefined_macro" => { return self.undefine_undefined_macro }
            _ => { return WarningLevel::Unset }
        };
    }
}*/
