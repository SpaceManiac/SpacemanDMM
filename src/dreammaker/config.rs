// various config options

use serde::Deserialize;
use std::fs::File;
use std::io::Read;
use error::Severity;

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
    #[serde(default="default_to_true")]
    duplicate_includes: bool,

    #[serde(default="default_to_true")]
    should_call_parent: bool,

    #[serde(default="default_to_true")]
    should_not_override: bool,

    #[serde(default="default_to_true")]
    field_access_static_type: bool,

    #[serde(default="default_to_true")]
    proc_call_static_type: bool,

    #[serde(default="default_to_true")]
    integer_precision_loss: bool,

    #[serde(default="default_to_true")]
    colon_path_warning: bool,

    #[serde(default="default_to_true")]
    var_in_proc_paramater: bool,

    #[serde(default="default_to_true")]
    static_in_proc_paramater: bool,

    #[serde(default="default_to_true")]
    macro_redefined: bool,

    #[serde(default="default_to_true")]
    undefine_undefined_macro: bool,

    #[serde(default="Severity::default_all")]
    error_level: Severity,
}

pub fn default_to_true() -> bool {
    true
}

impl Warnings {
    pub fn is_disabled(&self, errortype: &'static str) -> bool {
        match errortype {
            "duplicate_include" => { return !self.duplicate_includes },
            "must_call_parent" => { return !self.should_call_parent },
            "must_not_override" => { return !self.should_not_override },
            "field_access_static_type" => { return !self.field_access_static_type },
            "proc_call_static_type" => { return !self.proc_call_static_type },
            "integer_precision_loss" => { return !self.integer_precision_loss },
            "colon_path_warning" => { return !self.colon_path_warning },
            "var_in_proc_paramater" => { return !self.var_in_proc_paramater },
            "static_in_proc_parameter" => { return !self.var_in_proc_paramater },
            "macro_redefined" => { return !self.macro_redefined },
            "undefine_undefined_macro" => { return !self.undefine_undefined_macro }
            _ => { return false }
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
