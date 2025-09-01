use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;

use cu::pre::*;
use fxhash::FxHasher64;
        use clang_ast::Node;

use crate::config::Config;

use super::pre::*;
use super::type_structure::*;

const PARSE_TYPE_TOKEN: &str = "____stage0_clang_type_parse";

/// Parse string type names into structured template specialization data,
/// using clang. This is needed because DWARF only contains the stringified
/// type name for declarations
pub async fn parse_type(stage0: TypeStage0, command: &CompileCommand) -> cu::Result<TypeStage1> {
    let command = cu::check!(
        TypeParseCommand::try_new(&stage0.config, command),
        "failed to create type parse command for {}",
        stage0.name
    )?;

    let mut type_name_and_tokens = Vec::new();
    for (k, t) in &stage0.types {
        match t {
            Type0::Typedef(name, _) |
            Type0::EnumDecl(name) |
            Type0::UnionDecl(name) |
            Type0::StructDecl(name) => {
                type_name_and_tokens.push((*k, name))
            }
            _ => {}
        }
    }

    let original_cpp_file = Path::new(&stage0.name);
    cu::ensure!(original_cpp_file.is_absolute(), "compilation unit name must be absolute: {}", stage0.name);
    cu::ensure!(original_cpp_file.exists(), "cannot find source file for compilation unit: {}", stage0.name);

    let mut final_names = BTreeMap::default();
    let mut requests = BTreeMap::default();
    let mut source = format!(r##"#include "{}""##, stage0.name);
    for (k, name) in type_name_and_tokens {
        use std::fmt::Write;
        // no need to process untemplated
        if name.basename().contains('<') {
            let name_source = name.to_cpp_typedef_source()?;
            write!(source, "\ntypedef\n{name_source}\n{PARSE_TYPE_TOKEN}_{k};")?;
            requests.insert(name_source, k);
        } else {
            final_names.insert(k,NamespacedTemplatedName::new(name));
        }
    }

    if !requests.is_empty() {
        let parsed_types = match command.try_read_existing_output(&source, &requests) {
            Some(x) => x,
            None => cu::check!(command.invoke(&source, &requests).await, "failed to invoke type parse command for: {}", stage0.name)?,
        };

        let parsed_keys: BTreeSet<&str> = parsed_types.keys().map(|x| x.as_str()).collect();
        let request_keys: BTreeSet<&str> = requests.keys().map(|x| x.as_str()).collect();
        cu::ensure!(parsed_keys == request_keys);

        for (name, parsed) in parsed_types {
            let expected_k = *requests.get(&name).unwrap();
            let actual_k = parsed.goff;
            cu::ensure!(Goff::from(actual_k) == expected_k);
            final_names.insert(expected_k, parsed.name);
        }
    }

    let mut types = BTreeMap::default();
    for (k, t) in stage0.types {
        match t {
            Type0::Prim(prim) => {
                types.insert(k, Type1::Prim(prim));
            }
            Type0::Typedef(_, goff) => {
                match final_names.remove(&k) {
                    Some(name) => {
                        types.insert(k, Type1::Typedef(name, goff));
                    }
                    None => {
                        // cannot resolve the name: this means this typedef
                        // might be a private one (`using` inside a class or function).
                        // So in this case, we delete the name
                        types.insert(k, Type1::Alias(goff));
                    }
                }
            }
            Type0::EnumDecl(_) => {
                let name = cu::check!(final_names.remove(&k), "was not able to resolve enum decl name for {k}")?;
                types.insert(k, Type1::EnumDecl(name));
            }
            Type0::Enum(name, data) => {
                types.insert(k, Type1::Enum(name, data));
            }
            Type0::UnionDecl(_) => {
                let name = cu::check!(final_names.remove(&k), "was not able to resolve union decl name for {k}")?;
                types.insert(k, Type1::UnionDecl(name));
            }
            Type0::Union(name, data) => {
                types.insert(k, Type1::Union(name, data));
            }
            Type0::StructDecl(_) => {
                let name = cu::check!(final_names.remove(&k), "was not able to resolve struct decl name for {k}")?;
                types.insert(k, Type1::StructDecl(name));
            }
            Type0::Struct(name, data) => {
                types.insert(k, Type1::Struct(name, data));
            }
            Type0::Tree(tree) => {
                types.insert(k, Type1::Tree(tree));
            }
            Type0::Alias(goff) => {
                types.insert(k, Type1::Alias(goff));
            }
        }
    }

    if !requests.is_empty() {
        cu::bail!("this is a test error");
    }

    Ok(TypeStage1 { 
        offset: stage0.offset, 
        name: stage0.name, 
        types, config: stage0.config })
}

pub struct TypeParseCommand {
    pub cpp_file: String,
    pub out_file: String,
    pub args: Vec<String>,
}
impl TypeParseCommand {
    pub fn try_new(config: &Config, command: &CompileCommand) -> cu::Result<Self> {
        use std::hash::{Hash, Hasher};
        // create a temporary file name based on the file path
        let mut hash = FxHasher64::default();
        command.file.hash(&mut hash);
        let hash = hash.finish();
        let base_name = Path::new(&command.file).file_name_str()?;
        let cpp_file = config
            .paths
            .extract_output
            .join("clang-type-parse")
            .join(format!("{base_name}_{hash:16x}.cpp"))
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
        let out_file = format!("{cpp_file}.json");

        Ok(Self { cpp_file, out_file, args })
    }

    pub fn try_read_existing_output(&self, 
        new_source: &str,
        new_requests: &BTreeMap<String, Goff>
    ) -> Option<GoffMap<ParsedType>> {
        let cpp_file = Path::new(&self.cpp_file);
        if !cpp_file.exists() {
            return None;
        }
        let out_file = Path::new(&self.out_file);
        if !out_file.exists() {
            return None;
        }
        let Ok(old_content) = cu::fs::read_string(cpp_file) else {
            return None;
        };
        if old_content != new_source {
            // need to recompile
            return None;
        }
        let Ok(old_output) = cu::fs::read_string(out_file) else {
            return None;
        };
        let Ok(old_output) = json::parse::<ParsedTypes>(&old_output) else {
            return None;
        };
        let old_requests = old_output.iter().map(|(n, t)| (n.to_string(), t.goff.into()))
            .collect::<BTreeMap<_, _>>();
        if &old_requests != new_requests {
            return None;
        }
        Some(old_output)
    }

    pub async fn invoke(&self, source: &str, requests: &BTreeMap<String, Goff>) -> cu::Result<ParsedTypes> {
        // write the source file
        cu::fs::write(&self.cpp_file, source)?;
        cu::fs::remove(&self.out_file)?;

        // call clang
        let clang = cu::bin::find("clang", [
            cu::bin::from_env("CLANG"),
            cu::bin::in_PATH(),
        ])?;
        let (child, out, err) = clang.command()
        .args(&self.args)
            .stdout(cu::pio::string())
            .stderr(cu::pio::string())
            .stdin_null()
            .co_spawn().await?;
        if let Err(e) = child.co_wait_nz().await {
            let err = err.co_join().await??;
            cu::error!("stderr from clang:\n{err}");
            return Err(e);
        }

        let out = out.co_join().await??;
        let node = json::parse::<Node<Ast>>(&out)?;
        // force out to be deleted since it could be large
        drop(out);

        // parse the node.
        cu::ensure!(node.kind == Ast::TranslationUnitDecl, "outermost AST node must be TranslationUnitDecl");

        let mut parsed: ParsedTypes = Default::default();
        let mut token_to_request: BTreeMap<String, (Goff, String)> = 
        requests.iter().map(|(n, k)| (format!("{PARSE_TYPE_TOKEN}_{k}"), (*k, n.clone()))).collect();
        // let mut remaining_tokens: BTreeSet<&str, Goff> = token_to_request.keys().map(|x| x.as_str()).collect();
        for n in &node.inner {
            let Ast::TypedefDecl { name } = &n.kind else {
                continue;
            };
            let token = name.as_str();
            let Some((goff, name)) = token_to_request.remove(token) else {
                continue;
            };
            match parse_ast(&n) {
                Ok(pname) => {
                    parsed.insert(name, ParsedType {
                        goff: goff.into(),
                        name: pname
                    });
                }
                Err(e) => {
                    match json::stringify_pretty(&node) {
                        Err(e) => {
                            cu::error!("error while serializing errored node to json: {e}");
                        }
                        Ok(s) => {
                            let out_file = format!("{}.err.json", self.cpp_file);
                            if let Err(e) = cu::fs::write(out_file, s) {
                                cu::error!("error while saving errored node json: {e}");
                            }
                        }
                    }
                    cu::rethrow!(e, "failed to parse result node for type name: {name}");
                }
            }
        }

        if let Err(e) = cu::fs::write_json_pretty(&self.out_file, &parsed) {
            cu::error!("failed to save clang type parse cache: {e}");
        }

        Ok(parsed)
    }

}

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct CompileCommand {
    pub file: String,
    pub command: CompileArgs,
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

#[derive(Serialize, Deserialize)]
struct ParsedType {
    source: String,
    parsed: NamespacedTemplatedName,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
enum Ast {
    TranslationUnitDecl,
    TypedefDecl {
        name: String
    },
    ElaboratedType {
        qualifier: String
    },
    TemplateSpecializationType{
        /// The "untemplated name", but could have template in the qualifier
        template_name: Option<String>
    },
    TemplateArgument,
    ConstantExpr {
        value: String,
    },
    Other,
}

fn parse_ast(node: &Node<Ast>, namespace: &Namespace) -> cu::Result<NamespacedTemplatedName> {
    cu::ensure!(node.inner.len() == 1, "TypedefDecl node should have inner length 1");
    let node = &node.inner[0];
    let Ast::ElaboratedType { qualifier } = &node.kind else {
        cu::bail!("TypedefDecl node have one ElaboratedType inner node");
    };
    cu::ensure!(node.inner.len() == 1, "ElaboratedType node should have inner length 1");
    let node = &node.inner[0];
    let Ast::TemplateSpecializationType { template_name } = &node.kind else {
        cu::bail!("expecting TemplateSpecializationType");
    };

    let template_name = cu::check!(template_name.as_ref(), "unexpected empty template_name")?;
    let Some(base_name) = template_name.strip_prefix(qualifier) else {
        cu::bail!("template_name must starts with qualifier. qualifier='{qualifier}', template_name='{template_name}'");
    };

    let mut template_args = Vec::new();
    // iterate through the templates
    for n in &node.inner {
        template_args.push(parse_template_arg_ast(n)?);
    }

    Ok(NamespacedTemplatedName { 
        base: NamespacedName::namespaced(&namespace, base_name),
        templates: template_args
    })

}

fn parse_template_arg_ast(node: &Node<Ast>) -> cu::Result<TemplateArg<NamespaceLiteral>> {
    match &node.kind {
        Ast::ConstantExpr { value } => {
            let v_i64 = cu::check!(cu::parse::<i64>(value), "failed to parse ConstantExpr as i64, value: '{value}'")?;
            Ok(TemplateArg::Const(v_i64))
        }
        _ => {
            cu::bail!("unexpected node while parsing template args: {node:#?}");
        }
    }
}
