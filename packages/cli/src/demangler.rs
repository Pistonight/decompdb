use std::collections::BTreeMap;
use std::path::PathBuf;
use std::process::Command;
use std::sync::atomic::AtomicUsize;

use cu::pre::*;
use dashmap::DashMap;

pub struct Demangler {
    cache: DashMap<String, String>,
    cache_path: PathBuf,
    modification_count: AtomicUsize,
}

impl Demangler {
    pub fn try_new(cache_path: PathBuf) -> cu::Result<Self> {
        let cache = match cu::fs::reader(&cache_path) {
            Ok(x) => {
                match json::read::<DashMap<String, String>>(x) {
                    Ok(x) => x,
                    Err(e) => {
                        cu::warn!("failed to load demangler cache: {e}");
                        Default::default()
                    }
                }
            }
            Err(_) => Default::default()
        };
        Ok(Self {
            cache,
            cache_path,
            modification_count: AtomicUsize::new(0)
        })
    }
    pub fn demangle(&self, symbol: &str) -> cu::Result<String> {
        if !symbol.starts_with('?') && !symbol.starts_with("_Z") {
            return Ok(symbol.to_owned());
        }
        if let Some(x) = self.cache.get(symbol) {
            return Ok(x.to_owned());
        }

        let output = cu::check!(self.demangle_with_cxxfilt(symbol), "failed to demangle '{symbol}'")?;
        self.cache.insert(symbol.to_string(), output.clone());

        // note this will lose a few entries in the end, which is fine
        let c = self.modification_count.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        if c >= 5000 {
            let mut ordered = BTreeMap::new();
            ordered.extend(self.cache.clone());
            let cache_string = json::stringify_pretty(&ordered)?;
            cu::fs::write(&self.cache_path, cache_string)?;
            self.modification_count.store(0, std::sync::atomic::Ordering::Release);
        }
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
        Ok(result.trim().to_string())
    }
}


