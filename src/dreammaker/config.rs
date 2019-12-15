// various config options

use serde::{Deserialize, Deserializer};
use std::fs::File;
use std::io::Read;
use crate::error::Severity;

#[derive(Debug, Clone, Copy)]
pub enum WarningLevel {
    Error = 1,
    Warning = 2,
    Info = 3,
    Hint = 4,
    Disabled = 5,
    Unset = 6,
}

impl Default for WarningLevel {
    fn default() -> WarningLevel {
        WarningLevel::Unset
    }
}

impl<'de> Deserialize<'de> for WarningLevel {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?.to_lowercase();
        let warninglevel = match s.as_str() {
            "error" | "errors" => Ok(WarningLevel::Error),
            "warning" | "warnings" => Ok(WarningLevel::Warning),
            "info" | "infos" => Ok(WarningLevel::Info),
            "hint" | "hints" => Ok(WarningLevel::Hint),
            "false" | "off" => Ok(WarningLevel::Disabled),
            _other => Ok(WarningLevel::Unset),
        };
        warninglevel
    }
}

impl From<WarningLevel> for Severity {
    fn from(warninglevel: WarningLevel) -> Self {
        match warninglevel {
            WarningLevel::Error => Severity::Error,
            WarningLevel::Warning => Severity::Warning,
            WarningLevel::Info => Severity::Info,
            WarningLevel::Hint => Severity::Hint,
            _ => Severity::Error,
        }
    }
}

#[derive(Deserialize, Default)]
pub struct Config {
    #[serde(default)]
    pub warnings: Warnings,
}

impl Config {
    pub fn new() -> Config {
        Config {
            warnings: Warnings::default()
        }
    }
}

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

    #[serde(default="Severity::default_all")]
    error_level: Severity,
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

    pub fn severe_enough(&self, severity: Severity) -> bool {
        return severity <= self.error_level
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

    let config: Config = toml::from_str(&config_toml).unwrap();
    return config

}
