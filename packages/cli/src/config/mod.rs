use std::path::{Path, PathBuf};

use cu::pre::*;

use tyyaml::Prim;

mod extract_resolution;
pub use extract_resolution::*;

use crate::serde_impl::Regex;

/// Load config from a file
pub fn load(path: impl AsRef<Path>) -> cu::Result<Config> {
    let path = path.as_ref();
    let file_content = cu::fs::read_string(path)?;
    let mut config = toml::parse::<Config>(&file_content)?;

    let base = path.parent_abs()?;
    let base_rel = base.try_to_rel();
    resolve_path(&base_rel, &mut config.paths.elf);
    resolve_path(&base_rel, &mut config.paths.extract_output);
    resolve_path(&base_rel, &mut config.paths.compdb);
    config.paths.system_header_paths.iter_mut().for_each(|x| {
        resolve_path(&base_rel, x);
    });
    resolve_path(&base_rel, &mut config.paths.functions_csv.path);
    resolve_path(&base_rel, &mut config.paths.data_csv.path);

    config.extract.name_resolution.test_rules()?;
    match config.extract.pointer_width {
        8 | 16 | 32 | 64 => {}
        _ => cu::bailfyi!("invalid config.extract.pointer-width. must be 8, 16, 32 or 64"),
    }

    if Prim::Void == config.extract.ptmf_repr.0 {
        cu::bailfyi!("PTMF repr type must be sized");
    }
    if config.extract.ptmf_repr.1 == 0 {
        cu::bailfyi!("PTMF repr type must be non-zero size");
    }
    if Prim::Void == config.extract.ptmd_repr.0 {
        cu::bailfyi!("PTMD repr type must be sized");
    }
    if config.extract.ptmd_repr.1 == 0 {
        cu::bailfyi!("PTMD repr type must be non-zero size");
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
    /// Representation of PTMD, as an array of primitive
    pub ptmd_repr: (Prim, u32),
    /// Representation of PTMF, as an array of primitive
    pub ptmf_repr: (Prim, u32),
    /// Rules for resolving type names
    pub name_resolution: CfgExtractResolution,
    /// Regex for the virtual function pointer field
    pub vfptr_field_regex: Regex,
}

impl CfgExtract {
    /// Get the primitive equivalent of a pointer type
    pub fn pointer_type(&self) -> cu::Result<Prim> {
        let pointer_type = match self.pointer_width {
            8 => Prim::U8,
            16 => Prim::U16,
            32 => Prim::U32,
            64 => Prim::U64,
            x => cu::bail!("invalid pointer width in config: {x}"),
        };
        Ok(pointer_type)
    }

    /// Get the byte size of the pointer
    pub fn pointer_size(&self) -> cu::Result<u32> {
        let size = match self.pointer_width {
            8 => 1,
            16 => 2,
            32 => 4,
            64 => 8,
            x => cu::bail!("invalid pointer width in config: {x}"),
        };
        Ok(size)
    }

    pub fn ptmd_size(&self) -> cu::Result<u32> {
        let mut size = cu::check!(self.ptmd_repr.0.byte_size(), "invalid unsized ptmd repr in config")?;
        size *= self.ptmd_repr.1;
        cu::ensure!(size != 0, "invalid zero-sized ptmd repr in config");
        Ok(size)
    }

    pub fn ptmf_size(&self) -> cu::Result<u32> {
        let mut size = cu::check!(self.ptmf_repr.0.byte_size(), "invalid unsized ptmf repr in config")?;
        size *= self.ptmf_repr.1;
        cu::ensure!(size != 0, "invalid zero-sized ptmf repr in config");
        Ok(size)
    }
}

/// Config for project paths
///
/// For all paths, if it's a relative path, it's resolved relative to the directory
/// containing the config file
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgPaths {
    /// Path to the ELF file for extract.
    pub elf: PathBuf,
    /// Path to the output directory for the extract command.
    pub extract_output: PathBuf,
    /// Path to the compile_commands.json
    pub compdb: PathBuf,
    /// Path to include the system headers used by compile commands in compdb.
    ///
    /// This is needed since we need to use a newer clang with the -ast-dump=json
    /// option.
    pub system_header_paths: Vec<PathBuf>,

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
