use std::{io, env, fs};
use std::path::{Path, PathBuf};

#[derive(Serialize, Deserialize, Debug, Default)]
pub struct Config {
    pub recent: Vec<PathBuf>,
}

impl Config {
    pub fn load() -> Config {
        load(&get_config_path()).unwrap_or_default()
    }

    pub fn save(&self) {
        if let Err(e) = save(self, &get_config_path()) {
            println!("Config write error:\n{}", e);
        }
    }

    pub fn make_recent(&mut self, recent: &Path) {
        if let Some(first) = self.recent.first() {
            if first == recent {
                return  // no work to do
            }
        }

        self.recent.retain(|path| path != recent);
        self.recent.insert(0, recent.to_owned());
    }
}

fn get_config_path() -> PathBuf {
    // Determine the configuration directory
    let mut config_dir;
    if let Some(manifest_dir) = env::var_os("CARGO_MANIFEST_DIR") {
        // If we're being run through Cargo, put runtime files in target/
        config_dir = PathBuf::from(manifest_dir);
        config_dir.push("target");
    } else if let Ok(current_exe) = env::current_exe() {
        // Otherwise, put runtime files adjacent to the executable
        config_dir = current_exe;
        config_dir.pop();
    } else {
        // As a fallback, use the working directory
        config_dir = PathBuf::from(".");
    }
    config_dir.push("config.toml");
    config_dir
}

fn load(path: &Path) -> io::Result<Config> {
    use std::io::Read;

    let mut contents = String::new();
    io::BufReader::new(fs::File::open(path)?).read_to_string(&mut contents)?;
    match ::toml::from_str::<Config>(&contents) {
        Ok(cfg) => Ok(cfg),
        Err(e) => {
            println!("Config read error:\n{}", e);
            Ok(Config::default())
        }
    }
}

fn save(cfg: &Config, path: &Path) -> io::Result<()> {
    use std::io::Write;
    use serde::Serialize;

    let mut buffer = String::new();
    cfg.serialize(::toml::ser::Serializer::new(&mut buffer)
        .pretty_string(true)
        .pretty_string_literal(false)
        .pretty_array(true)).unwrap();

    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }

    fs::File::create(path)?.write_all(buffer.as_bytes())
}
