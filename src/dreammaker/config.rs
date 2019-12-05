// various config options

use serde::Deserialize;
use std::fs::File;
use std::io::Read;

#[derive(Deserialize, Default)]
pub struct Config {
    #[serde(default)]
    warnings: Warnings,
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
    duplicate_includes: bool,

    #[serde(default)]
    should_call_parent: bool,
}

impl Warnings {
    pub fn is_disabled(&self, errortype: &'static str) -> bool {
        match errortype {
            "duplicateinclude" => { return self.duplicate_includes }
            "shouldcallparent" => { return self.should_call_parent }
            _ => { return false }
        };
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
