use std::process::Command;

use cu::pre::*;
use dashmap::DashMap;

pub struct Demangler {
    cache: DashMap<String, String>,
}

impl Demangler {
    pub fn new() -> Self {
        Self {
            cache: DashMap::new()
        }
    }
    pub fn demangle(&self, symbol: &str) -> cu::Result<String> {
        if let Some(x) = self.cache.get(symbol) {
            return Ok(x.to_owned());
        }

        let output = self.demangle_with_cxxfilt(symbol)?;
        self.cache.insert(symbol.to_string(), output.clone());
        Ok(output)
    }

    fn demangle_with_cxxfilt(&self, symbol: &str) -> cu::Result<String> {
        let cxxfilt = cu::bin::find("llvm-cxxfilt", [
            cu::bin::from_env("CXXFILT"),
            cu::bin::in_PATH(),
        ]);
        let cxxfilt = cu::check!(cxxfilt, "could not find llvm-cxxfilt (please install llvm or set CXXFILT env var to path of llvm-cxxfilt)")?;

        let output = cu::check!(Command::new(cxxfilt)
            .arg(symbol)
            .output(), "failed to spawn cxxfilt command")?;
        if !output.status.success() {
            return Ok(symbol.to_string());
        }
        let result = cu::check!(String::from_utf8(output.stdout), "cxxfilt output is not valid UTF-8")?;
        Ok(result)
    }
}


