use std::path::{Path, PathBuf};

use cu::pre::*;

mod extract_resolution;
pub use extract_resolution::*;

// mod util_contrib;
// use util_contrib::*;

/// Load config from a file
pub fn load(path: impl AsRef<Path>) -> cu::Result<Config> {
    let path = path.as_ref();
    let file_content = cu::fs::read_string(path)?;
    let mut config = toml::parse::<Config>(&file_content)?;

    let base = path.parent_abs()?;
    let base_rel = base.try_to_rel();
    resolve_path(&base_rel, &mut config.paths.elf);
    resolve_path(&base_rel, &mut config.paths.extract);
    resolve_path(&base_rel, &mut config.paths.functions_csv.path);
    resolve_path(&base_rel, &mut config.paths.data_csv.path);

    config.extract.name_resolution.test_rules()?;
    match config.extract.pointer_width {
        8 | 16 | 32 | 64 => {}
        _ => cu::bailfyi!("invalid config.extract.pointer-width. must be 8, 16, 32 or 64"),
    }

    Ok(config)
}

#[derive(Debug, Deserialize)]
pub struct Config {
    pub paths: CfgPaths,
    pub extract: CfgExtract,
}

/// Config for extract
#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgExtract {
    /// Pointer width for the target platform, must be 8, 16, 32 or 64
    pub pointer_width: u8,
    /// Rules for resolving type names
    pub name_resolution: CfgExtractResolution,
}

/// Config for project paths
///
/// For all paths, if it's a relative path, it's resolved relative to the directory
/// containing the config file
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgPaths {
    /// Path for the ELF file for extract.
    pub elf: PathBuf,
    /// Path for the output directory for the extract command.
    pub extract: PathBuf,
    /// Configuration for the functions CSV file
    ///
    /// **This is deprecated and the format for symbol listing will change in the future**
    pub functions_csv: CfgCsv,
    /// Configuration for the data CSV file
    ///
    /// **This is deprecated and the format for symbol listing will change in the future**
    pub data_csv: CfgCsv,
}

/// Configuration for CSV data
///
/// **This is deprecated and the format for symbol listing will change in the future**
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgCsv {
    /// Path to the CSV file
    pub path: PathBuf,
    /// Base address for the address column
    pub base_address: u64,
    /// Which column is the address column, 0-indexed
    pub address_column: usize,
    /// Which column is the symbol column, 0-indexed
    pub symbol_column: usize,
}

fn resolve_path(base: &Path, path: &mut PathBuf) {
    if !path.is_absolute() {
        *path = base.join(&path);
    }
}
