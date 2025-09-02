use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::path::Path;
use std::path::PathBuf;

use cu::pre::*;
use fxhash::FxHasher64;
        use clang_ast::Node;
        use tyyaml::Tree;

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
    let mut source = format!(r##"
// this is a hack for private declarations
#define private public
#define protected public
#include "{}"
"##, stage0.name);
    for (k, name) in type_name_and_tokens {
        use std::fmt::Write;
        // no need to process untemplated
        if name.basename().contains('<') {
            let mut name_source = name.to_cpp_typedef_source()?;
            clean_up_name_cpp_source(&mut name_source);
            write!(source, "\ntypedef\n{name_source}\n{PARSE_TYPE_TOKEN}_{k};")?;
            let request = ParseRequest {
                goff: k,
                source: name_source,
                namespace: name.namespace()
            };
            requests.insert(k, request);
        } else {
            final_names.insert(k,NamespacedTemplatedName::new(name));
        }
    }

    if !requests.is_empty() {
        let parsed_results = match command.try_read_existing_output(&source, &requests) {
            Some(x) => x,
            None => cu::check!(command.invoke(&source, &requests).await, "failed to invoke type parse command for: {}", stage0.name)?,
        };

        let result_keys: BTreeSet<(Goff, &str)> = 
        parsed_results.iter().map(|(k, r)| (*k, r.source.as_str())).collect();
        let request_keys: BTreeSet<(Goff, &str)> = 
        requests.iter().map(|(k, r)| (*k, r.source.as_str())).collect();
        cu::ensure!(result_keys == request_keys);

        for (k, result) in parsed_results {
            final_names.insert(k, result.parsed);
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

    Ok(TypeStage1 { 
        offset: stage0.offset, 
        name: stage0.name, 
        types, config: stage0.config })
}

fn clean_up_name_cpp_source(name: &mut String) {
    *name = name.replace("::(anonymous namespace)::", "::");
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

    fn try_read_existing_output(&self, 
        new_source: &str,
        new_requests: &GoffMap<ParseRequest<'_>>
    ) -> Option<GoffMap<ParseResult>> {
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
        let Ok(old_output) = json::parse::<GoffMap<ParseResult>>(&old_output) else {
            return None;
        };
        let old_sources = old_output.iter().map(|(k, t)| (*k, &t.source))
            .collect::<BTreeMap<_, _>>();
        let new_sources = new_requests.iter().map(|(k, t)| (*k, &t.source))
            .collect::<BTreeMap<_, _>>();
        if old_sources != new_sources {
            return None;
        }
        Some(old_output)
    }

    async fn invoke(&self, source: &str, requests: &GoffMap<ParseRequest<'_>>) -> cu::Result<GoffMap<ParseResult>> {
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

        let mut results = GoffMap::default();
        let mut token_to_request: BTreeMap<_, _> = 
        requests.into_iter().map(|(k, r)| (format!("{PARSE_TYPE_TOKEN}_{k}"), r)).collect();
        for n in &node.inner {
            let Ast::TypedefDecl { name } = &n.kind else {
                continue;
            };
            let token = name.as_str();
            let Some(request) = token_to_request.remove(token) else {
                continue;
            };
            match parse_ast(&n, request.namespace) {
                Ok(parsed) => {
                    results.insert(request.goff, ParseResult {
                        source: request.source.clone(),
                        parsed
                    });
                }
                Err(e) => {
                    match json::stringify_pretty(n) {
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

        if let Err(e) = cu::fs::write_json_pretty(&self.out_file, &results) {
            cu::error!("failed to save clang type parse cache: {e}");
        }

        Ok(results)
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

struct ParseRequest<'a> {
    /// The goff of the type
    goff: Goff,
    /// The CPP source code that represents this type
    source: String,
    /// The namespace this type is in
    namespace: &'a Namespace,
}

#[derive(Serialize, Deserialize)]
struct ParseResult {
    /// The CPP source code that represents this type
    source: String,
    /// The parsed type data
    parsed: NamespacedTemplatedName,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
enum Ast {
    TranslationUnitDecl,
    TypedefDecl {
        name: String
    },
    ElaboratedType {
        #[serde(default)]
        qualifier: String
    },
    #[serde(rename_all = "camelCase")]
    TemplateSpecializationType{
        /// The "untemplated name", but could have template in the qualifier
        template_name: String
    },
    TemplateArgument,
    ConstantExpr {
        value: String,
    },
    QualType,
    PointerType,
    BuiltinType {
        r#type: AstType
    },
    TypedefType {
        decl: AstDecl,
    },
    RecordType {
        decl: AstDecl,
    },
    EnumType {
        decl: AstDecl,
    },
    // Other,
    Other {
        kind: Option<String>,
    },
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AstType {
    qual_type: String
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AstDecl {
    name: String
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

    // let template_name = cu::check!(template_name.as_ref(), "unexpected empty template_name")?;
    let Some(base_name) = template_name.strip_prefix(qualifier) else {
        cu::bail!("template_name must starts with qualifier. qualifier='{qualifier}', template_name='{template_name}'");
    };

    let template_args = cu::check!(parse_template_spec_ast(node), "failed to parse outermost templates")?;

    Ok(NamespacedTemplatedName { 
        base: NamespacedName::namespaced(&namespace, base_name),
        templates: template_args
    })

}
fn parse_template_spec_ast(node: &Node<Ast>) -> cu::Result<Vec<TemplateArg<NamespacedTemplatedArg>>> {
    let mut template_args = Vec::new();
    // iterate through the templates
    for n in &node.inner {
        if n.kind != Ast::TemplateArgument {
            continue;
        }
        cu::ensure!(n.inner.len() == 1, "TemplateArgument node should have inner length 1");
        template_args.push(parse_template_arg_ast(&n.inner[0])?);
    }

    Ok(template_args)
}
fn parse_template_arg_ast(node: &Node<Ast>) -> cu::Result<TemplateArg<NamespacedTemplatedArg>> {
    parse_template_arg_ast_recur(node, "")
}

fn parse_template_arg_ast_recur(node: &Node<Ast>, qualifier: &str) -> cu::Result<TemplateArg<NamespacedTemplatedArg>> {
    match &node.kind {
        Ast::ConstantExpr { value } => {
            match value.as_str() {
                "true" => Ok(TemplateArg::Const(1)),
                "false" => Ok(TemplateArg::Const(0)),
                v => {
                    let v_i64 = cu::check!(cu::parse::<i64>(v), "failed to parse ConstantExpr as i64, value: '{value}'")?;
                Ok(TemplateArg::Const(v_i64))
                }
            }
        }
        Ast::BuiltinType {r#type: ty } => {
            match ty.qual_type.as_str() {
                "void"|"bool" => {
                    Ok(TemplateArg::Type(
                        NamespacedTemplatedArg::new(
                        Tree::Base(NamespaceLiteral::new(&ty.qual_type)))))
                }
                "char" => {
                    Ok(TemplateArg::Type(
                        NamespacedTemplatedArg::new(
                        Tree::Base(NamespaceLiteral::new("i8")))))
                }
                "int" => {
                    Ok(TemplateArg::Type(
                        NamespacedTemplatedArg::new(
                        Tree::Base(NamespaceLiteral::new("i32")))))
                }
                "float" => {
                    Ok(TemplateArg::Type(
                        NamespacedTemplatedArg::new(
                        Tree::Base(NamespaceLiteral::new("f32")))))
                }
                _ => {
                    cu::bail!("unexpected builtin qual_type: {} (please add it if you need).", ty.qual_type);
                }
            }
        }
        Ast::ElaboratedType { qualifier: q } => {
            let new_qualifier = format!("{qualifier}{q}");
            cu::ensure!(node.inner.len() == 1, "ElaboratedType node should have inner length 1");
            parse_template_arg_ast_recur(&node.inner[0], &new_qualifier)
        }
        Ast::TemplateSpecializationType {template_name} => {
    let Some(base_name) = template_name.strip_prefix(qualifier) else {
        cu::bail!("template_name must starts with qualifier. qualifier='{qualifier}', template_name='{template_name}'");
    };
            cu::ensure!(!base_name.contains('<'), "template base name should not contain templates");
            cu::ensure!(!base_name.contains(':'), "template base name should not contain namespaces");
            let template_args = cu::check!(parse_template_spec_ast(node), "failed to parse nested templates at {base_name}")?;
            Ok(TemplateArg::Type(
                NamespacedTemplatedArg::with_templates(
                    Tree::Base(NamespaceLiteral::parse(template_name)?),
                    template_args
                )))
        }
        Ast::RecordType {decl} => {
            cu::ensure!(!decl.name.contains('<'), "RecordType declaration should not contain templates");
            cu::ensure!(!decl.name.contains(':'), "RecordType declaration should not contain namespaces");
            let full_name = format!("{qualifier}{}", decl.name);
            cu::ensure!(node.inner.is_empty(), "RecordType node should have no inner nodes");
            Ok(TemplateArg::Type(
                NamespacedTemplatedArg::new(
                    Tree::Base(NamespaceLiteral::parse(&full_name)?))))
        }
        Ast::EnumType {decl} => {
            cu::ensure!(!decl.name.contains('<'), "EnumType declaration should not contain templates");
            cu::ensure!(!decl.name.contains(':'), "EnumType declaration should not contain namespaces");
            let full_name = format!("{qualifier}{}", decl.name);
            cu::ensure!(node.inner.is_empty(), "EnumType node should have no inner nodes");
            Ok(TemplateArg::Type(
                NamespacedTemplatedArg::new(
                    Tree::Base(NamespaceLiteral::parse(&full_name)?))))
        }
        Ast::PointerType => {
            cu::ensure!(node.inner.len() == 1, "PointerType node should have inner length 1");
            let node = &node.inner[0];
            let pointee = cu::check!(parse_template_arg_ast_recur(node, qualifier), "failed to parse pointee type")?;
            let TemplateArg::Type(mut ty) = pointee else {
                cu::bail!("cannot have pointer to constexpr");
            };
            ty.base = Tree::Ptr(Box::new(ty.base));
            Ok(TemplateArg::Type(ty))
        }
        Ast::TypedefType{ decl } => {
            cu::ensure!(!decl.name.contains('<'), "TypedefType declaration should not contain templates");
            cu::ensure!(!decl.name.contains(':'), "TypedefType declaration should not contain namespaces");
            let full_name = format!("{qualifier}{}", decl.name);
            Ok(TemplateArg::Type(
                NamespacedTemplatedArg::new(
                    Tree::Base(NamespaceLiteral::parse(&full_name)?))))
            // cu::ensure!(node.inner.len() == 1, "TypedefType node should have inner length 1");
            // let node = &node.inner[0];
            // parse_template_arg_ast_recur(node, qualifier)
        }
        Ast::QualType => {
            // qualifier (const, volatile, restrict...)
            // while it's technically possible for these to produce different template
            // specializations, for now, we treat them as the same
            cu::ensure!(node.inner.len() == 1, "QualType node should have inner length 1");
            let node = &node.inner[0];
            parse_template_arg_ast_recur(node, qualifier)
        }
        _ => {
            cu::bail!("unexpected node while parsing template args: {node:#?}");
        }
    }
}
