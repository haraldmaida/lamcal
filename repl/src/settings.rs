#![allow(missing_copy_implementations)]

#[cfg(feature = "config")]
use std::path::Path;

#[cfg(feature = "config")]
use config::{Config, ConfigError, File};

#[derive(Debug, PartialEq)]
pub struct Settings {
    pub debug: bool,
    pub history: History,
}

impl Default for Settings {
    fn default() -> Self {
        Settings {
            debug: false,
            history: History::default(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct History {
    pub max_len: usize,
    pub ignore_dups: bool,
    pub ignore_space: bool,
}

impl Default for History {
    fn default() -> Self {
        History {
            max_len: 100,
            ignore_dups: true,
            ignore_space: false,
        }
    }
}

#[cfg(feature = "config")]
impl Settings {
    pub fn merge(mut self, path: &Path) -> Result<Self, ConfigError> {
        let mut config = Config::new();
        if let Some(file_path) = path.to_str() {
            config.merge(File::with_name(file_path).required(false))?;
        }
        if let Ok(debug) = config.get_bool("debug") {
            self.debug = debug;
        }
        if let Ok(ignore_dups) = config.get_bool("history.ignore_dups") {
            self.history.ignore_dups = ignore_dups;
        }
        if let Ok(ignore_space) = config.get_bool("history.ignore_space") {
            self.history.ignore_space = ignore_space;
        }
        Ok(self)
    }
}
