use cu::pre::*;
use fxhash::FxHasher64;

use crate::config::Config;

use super::pre::*;
use super::type_structure::*;

/// Parse string type names into structured template specialization data,
/// using clang. This is needed because DWARF only contains the stringified
/// type name for declarations
pub fn parse_type_names(stage0: TypeStage0, command: &CompileCommand) -> cu::Result<()> {
    let command = cu::check!(
        TypeParseCommand::try_new(&stage0.config, command),
        "failed to create type parse command for {}",
        stage0.name
    )?;

    let mut type_name_and_tokens = Vec::new();
    for (k, t) in &stage0.types {
        match t {
            Type0::Prim(_) => {}
            Type0::Typedef(name, goff) => {}
            Type0::Enum(_, type0_enum) => todo!(),
            Type0::EnumDecl(_) => todo!(),
            Type0::Union(_, type0_union) => todo!(),
            Type0::UnionDecl(_) => todo!(),
            Type0::Struct(_, type0_struct) => todo!(),
            Type0::StructDecl(_) => todo!(),
            Type0::Tree(tree) => todo!(),
            Type0::Alias(goff) => todo!(),
        }
    }

    todo!()
}

pub struct TypeParseCommand {
    pub file: String,
    pub args: Vec<String>,
}
impl TypeParseCommand {
    pub fn try_new(config: &Config, command: &CompileCommand) -> cu::Result<Self> {
        use std::hash::{Hash, Hasher};
        // create a temporary file name based on the file path
        let mut hash = FxHasher64::default();
        command.file.hash(&mut hash);
        let hash = hash.finish();
        let cpp_file = config
            .paths
            .extract_output
            .join("clang-type-parse")
            .join(format!("{}_{hash:16x}.cpp", command.file))
            .into_utf8()?;

        let mut args = vec![
            "-Xclang".to_string(),
            "-ast-dump=json".to_string(),
            "-fsyntax-only".to_string(),
            cpp_file.clone(),
        ];
        for include in &config.paths.system_header_paths {
            args.push(format!("-I{}", include.as_utf8()?))
        }
        let mut last_is_minus_o = false;
        for arg in command.command.0.iter() {
            if last_is_minus_o {
                continue;
            }
            if arg == "-o" {
                last_is_minus_o = true;
                continue;
            }
            if arg == &command.file {
                continue;
            }
            if arg == "-c" {
                continue;
            }
            args.push(arg.to_string());
        }

        Ok(Self { file: cpp_file, args })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct CompileCommand {
    pub file: String,
    pub command: CompileArgs,
}

impl CompileCommand {
    pub fn make_type_parse_command(&mut self, config: &Config) {}
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompileArgs(Vec<String>);
impl<'de> Deserialize<'de> for CompileArgs {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        return d.deserialize_str(Visitor);
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = CompileArgs;
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
                        Ok(CompileArgs(x))
                    }
                }
            }
        }
    }
}
