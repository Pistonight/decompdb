use std::path::{Path, PathBuf};

use cu::pre::*;

use tyyaml::Prim;

mod extract;
pub use extract::*;

/// Load config from a file
pub fn load(path: impl AsRef<Path>) -> cu::Result<Config> {
    let path = path.as_ref();
    let file_content = cu::fs::read_string(path)?;
    let mut config = toml::parse::<Config>(&file_content)?;

    let base = path.parent_abs()?;
    let base_rel = base.try_to_rel();
    resolve_path(&base_rel, &mut config.paths.build_dir)?;
    resolve_path(&base_rel, &mut config.paths.elf)?;
    resolve_path(&base_rel, &mut config.paths.extract_output)?;
    resolve_path(&base_rel, &mut config.paths.compdb)?;
    config
        .paths
        .system_header_paths
        .iter_mut()
        .map(|x| resolve_path(&base_rel, x))
        .collect::<Result<Vec<_>, _>>()?;
    resolve_path(&base_rel, &mut config.paths.functions_csv.path)?;
    resolve_path(&base_rel, &mut config.paths.data_csv.path)?;

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

/// Config for project paths
///
/// For all paths, if it's a relative path, it's resolved relative to the directory
/// containing the config file
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct CfgPaths {
    /// Path to the directory to invoke the build command
    pub build_dir: PathBuf,
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
    /// Skip first X rows when parsing
    #[serde(default)]
    pub skip_rows: usize,
}

fn resolve_path(base: &Path, path: &mut PathBuf) -> cu::Result<()> {
    if !path.is_absolute() {
        *path = base.join(&path).normalize()?;
    }
    Ok(())
}

/// An entry in compile_commands.json
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct CompileCommand {
    /// The file name (usually absolute)
    pub file: String,
    /// The compile args. Does not include the compiler
    #[serde(deserialize_with = "deserialize_compile_command_args")]
    pub command: Vec<String>,
}
fn deserialize_compile_command_args<'de, D: serde::Deserializer<'de>>(d: D) -> Result<Vec<String>, D::Error> {
    return d.deserialize_str(Visitor);
    struct Visitor;
    impl<'de> serde::de::Visitor<'de> for Visitor {
        type Value = Vec<String>;
        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(f, "a POSIX compliant shell command")
        }
        fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
            match shell_words::split(v) {
                Err(e) => Err(serde::de::Error::custom(format!("invalid shell command: {e}"))),
                Ok(mut x) => {
                    if x.is_empty() {
                        return Err(serde::de::Error::custom(format!("command must be non-empty")));
                    }

                    // remove the compiler
                    x.remove(0);
                    Ok(x)
                }
            }
        }
    }
}
