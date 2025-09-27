use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use cu::pre::*;
use tyyaml::Tree;
use clang_ast::Node;
use fxhash::FxHasher64;

use crate::config::{Config, CompileCommand};

use super::pre::*;

/// Parse the names that are raw strings in DWARF into structured names
pub async fn parse_names(stage: &Stage0, command: &CompileCommand) -> cu::Result<GoffMap<NamespacedTemplatedName>> {
    let command = cu::check!(
        TypeParseCommand::try_new(&stage.config, command),
        "failed to create type parse command for {}",
        stage.name
    )?;
    let original_cpp_file = Path::new(&stage.name);
    cu::ensure!(
        original_cpp_file.is_absolute(),
        "compilation unit name must be absolute: {}",
        stage.name
    );
    cu::ensure!(
        original_cpp_file.exists(),
        "cannot find source file for compilation unit: {}",
        stage.name
    );

    let mut final_names = GoffMap::default();
    let mut requests = GoffMap::default();
    // put the command into the output cpp file for debugging
    let args = shell_words::join(&command.args);
    let mut source = format!(
        r##"
// clang {args}

// this is a hack for private declarations
#define private public
#define protected public
#include "{}"
"##,
        stage.name
    );

    // load up the source
    for (k, t) in &stage.types {
        use std::fmt::Write;
        let (name, namespace) = match t {
            Type0::Typedef(name, _) => (name, None),
            Type0::EnumDecl(ns, name) | Type0::UnionDecl(ns, name) | Type0::StructDecl(ns, name) => {
                // type_name_and_tokens.push((*k, name))
                (name, Some(ns))
            }
            _ => continue,
        };
        // no need to process untemplated
        if !name.basename().contains('<') {
            final_names.insert(*k, NamespacedTemplatedName::new(name.clone()));
            continue;
        }
        let mut name_source = name.to_cpp_typedef_source()?;
        clean_up_name_cpp_source(&mut name_source);
        if let Some(ns) = namespace {
            let ns_source = ns.to_cpp_typedef_source()?;
            // no need to clean, since we already processed anonymous
            // as part of the DWARF tree
            write!(source, "\nnamespace {ns_source}{{")?;
            write!(source, "\ntypedef\n{name_source}\n{PARSE_TYPE_TOKEN}_{k};")?;
            write!(source, "\n}}")?;
        } else {
            write!(source, "\ntypedef\n{name_source}\n{PARSE_TYPE_TOKEN}_{k};")?;
        }
        let request = ParseRequest {
            goff: *k,
            source: name_source,
            namespace: name.namespace(),
        };
        requests.insert(*k, request);
    }

    if !requests.is_empty() {
        let parsed_results = match command.try_read_existing_output(&source, &requests) {
            Some(x) => x,
            None => cu::check!(
                command.invoke(&source, &requests, &stage.ns).await,
                "failed to invoke type parse command for: {}",
                stage.name
            )?,
        };

        let result_keys: BTreeSet<(Goff, &str)> = parsed_results.iter().map(|(k, r)| (*k, r.source.as_str())).collect();
        let request_keys: BTreeSet<(Goff, &str)> = requests.iter().map(|(k, r)| (*k, r.source.as_str())).collect();
        cu::ensure!(result_keys == request_keys, "did not resolve all types");

        for (k, result) in parsed_results {
            final_names.insert(k, result.parsed);
        }
    }

    Ok(final_names)
}

const PARSE_TYPE_TOKEN: &str = "____stage0_clang_type_parse";


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
        for arg in &command.command {
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

        Ok(Self {
            cpp_file,
            out_file,
            args,
        })
    }

    fn try_read_existing_output(
        &self,
        new_source: &str,
        new_requests: &GoffMap<ParseRequest<'_>>,
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
        let old_output = match json::parse::<GoffMap<ParseResult>>(&old_output) {
            Err(e) => {
                cu::error!("failed to parse cached output from {}: {e:?}", self.out_file);
                return None;
            }
            Ok(x) => x,
        };
        let old_sources = old_output
            .iter()
            .map(|(k, t)| (*k, &t.source))
            .collect::<BTreeMap<_, _>>();
        let new_sources = new_requests
            .iter()
            .map(|(k, t)| (*k, &t.source))
            .collect::<BTreeMap<_, _>>();
        if old_sources != new_sources {
            return None;
        }
        cu::debug!("using cached output");
        Some(old_output)
    }

    async fn invoke(
        &self,
        source: &str,
        requests: &GoffMap<ParseRequest<'_>>,
        ns: &NamespaceMaps,
    ) -> cu::Result<GoffMap<ParseResult>> {
        // write the source file
        cu::fs::write(&self.cpp_file, source)?;
        cu::fs::remove(&self.out_file)?;

        // call clang
        let clang = cu::bin::find("clang", [cu::bin::from_env("CLANG"), cu::bin::in_PATH()])?;
        let (child, out, err) = clang
            .command()
            .args(&self.args)
            .stdout(cu::pio::string())
            .stderr(cu::pio::string())
            .stdin_null()
            .co_spawn()
            .await?;
        if let Err(e) = child.co_wait_nz().await {
            let err = err.co_join().await??;
            cu::error!("stderr from clang:\n{err}");
            cu::hint!(
                "failed to compile the source for type parsing - this usually means the type expression has unparsable syntax."
            );
            cu::hint!("consider using extract.type-parser.abandon-typedefs config to exclude this name");
            return Err(e);
        }

        let out = out.co_join().await??;
        let node = json::parse::<Node<Ast>>(&out)?;
        // force out to be deleted since it could be large
        drop(out);

        // parse the node.
        cu::ensure!(
            node.kind == Ast::TranslationUnitDecl,
            "outermost AST node must be TranslationUnitDecl"
        );
        let tu_node = node;

        let mut results = GoffMap::default();
        let mut token_to_request: BTreeMap<_, _> = requests
            .into_iter()
            .map(|(k, r)| (format!("{PARSE_TYPE_TOKEN}_{k}"), r))
            .collect();
        let mut stack = vec![];
        for n in tu_node.inner.iter().rev() {
            stack.push(n);
        }
        while let Some(node) = stack.pop() {
            if let Ast::NamespaceDecl { .. } = &node.kind {
                for n in node.inner.iter().rev() {
                    stack.push(n);
                }
                continue;
            }
            let Ast::TypedefDecl { name } = &node.kind else {
                continue;
            };
            let token = name.as_str();
            let Some(request) = token_to_request.remove(token) else {
                continue;
            };
            match parse_ast(&node, request.namespace, ns) {
                Ok(parsed) => {
                    results.insert(
                        request.goff,
                        ParseResult {
                            source: request.source.clone(),
                            parsed,
                        },
                    );
                }
                Err(e) => {
                    match json::stringify_pretty(node) {
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


struct ParseRequest<'a> {
    /// The goff of the type
    goff: Goff,
    /// The CPP source code that represents this type
    source: String,
    /// The qualifying namespace to add this type to
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
    NamespaceDecl {
        name: Option<String>,
    },
    TypedefDecl {
        name: String,
    },
    ElaboratedType {
        #[serde(default)]
        qualifier: String,
        #[serde(rename = "type")]
        ty: AstType,
    },
    #[serde(rename_all = "camelCase")]
    TemplateSpecializationType {
        /// The "untemplated name", but could have template in the qualifier
        template_name: String,
    },
    TemplateArgument,
    ConstantExpr {
        value: String,
    },
    QualType,
    PointerType,
    MemberPointerType,
    LValueReferenceType,
    RValueReferenceType,
    ParenType, // indicate the inner is a function type
    FunctionProtoType,
    BuiltinType {
        #[serde(rename = "type")]
        ty: AstType,
    },
    TypedefType {
        #[serde(rename = "type")]
        ty: AstType,
        // decl: AstDecl,
    },
    RecordType {
        #[serde(rename = "type")]
        ty: AstType,
        // decl: AstDecl,
    },
    EnumType {
        #[serde(rename = "type")]
        ty: AstType,
        // decl: AstDecl,
    },
    // Other,
    Other {
        kind: Option<String>,
    },
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AstType {
    qual_type: String,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct AstDecl {
    name: String,
}

fn parse_ast(node: &Node<Ast>, namespace: &Namespace, nsmaps: &NamespaceMaps) -> cu::Result<NamespacedTemplatedName> {
    cu::ensure!(node.inner.len() == 1, "TypedefDecl node should have inner length 1");
    let node = &node.inner[0];
    let Ast::ElaboratedType { qualifier, .. } = &node.kind else {
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

    let template_args = cu::check!(
        parse_template_spec_ast(node, nsmaps),
        "failed to parse outermost templates"
    )?;

    Ok(NamespacedTemplatedName {
        base: NamespacedName::namespaced(&namespace, base_name),
        templates: template_args,
    })
}
fn parse_template_spec_ast(
    node: &Node<Ast>,
    ns: &NamespaceMaps,
) -> cu::Result<Vec<TemplateArg<NamespacedTemplatedName>>> {
    let mut template_args = Vec::new();
    // iterate through the templates
    for n in &node.inner {
        if n.kind != Ast::TemplateArgument {
            continue;
        }
        cu::ensure!(n.inner.len() == 1, "TemplateArgument node should have inner length 1");
        template_args.push(parse_template_arg_ast(&n.inner[0], ns)?);
    }

    Ok(template_args)
}
fn parse_template_arg_ast(node: &Node<Ast>, ns: &NamespaceMaps) -> cu::Result<TemplateArg<NamespacedTemplatedName>> {
    parse_template_arg_ast_recur(node, ns, "", None)
}

fn parse_template_arg_ast_recur(
    node: &Node<Ast>,
    ns: &NamespaceMaps,
    qualifier: &str,
    elaborated_qual_type: Option<&str>,
) -> cu::Result<TemplateArg<NamespacedTemplatedName>> {
    match &node.kind {
        Ast::ConstantExpr { value } => match value.as_str() {
            "true" => Ok(TemplateArg::Const(1)),
            "false" => Ok(TemplateArg::Const(0)),
            v => {
                let v_i64 = cu::check!(
                    cu::parse::<i64>(v),
                    "failed to parse ConstantExpr as i64, value: '{value}'"
                )?;
                Ok(TemplateArg::Const(v_i64))
            }
        },
        Ast::BuiltinType { ty } => {
            let prim_name = match ty.qual_type.as_str() {
                "void" => "void",
                "bool" => "bool",
                "unsigned char" => "u8",
                "unsigned short" => "u16",
                "unsigned int" => "u32",
                "unsigned long" => "u64", // in most cases
                "char" => "i8",
                "short" => "i16",
                "int" => "i32",
                "long" => "i64", // in most cases
                "float" => "f32",
                "double" => "f64",
                // implementation defined:
                "wchar_t" => "i16",
                _ => {
                    cu::bail!(
                        "unexpected builtin qual_type: {} (please add it if you need).",
                        ty.qual_type
                    );
                }
            };
            Ok(TemplateArg::Type(Tree::Base(NamespacedTemplatedName::new(
                NamespacedName::unnamespaced(prim_name),
            ))))
        }
        Ast::ElaboratedType { qualifier: q, ty } => {
            cu::ensure!(node.inner.len() == 1, "ElaboratedType node should have inner length 1");
            let new_qualifier = format!("{qualifier}{q}");
            let qual_type = ty.qual_type.as_str();
            parse_template_arg_ast_recur(&node.inner[0], ns, &new_qualifier, Some(qual_type))
        }
        Ast::TemplateSpecializationType { template_name } => {
            let Some(base_name) = template_name.strip_prefix(qualifier) else {
                cu::bail!(
                    "template_name must starts with qualifier. qualifier='{qualifier}', template_name='{template_name}'"
                );
            };
            cu::ensure!(
                !base_name.contains('<'),
                "template base name should not contain templates"
            );
            cu::ensure!(
                !base_name.contains(':'),
                "template base name should not contain namespaces"
            );
            let template_args = cu::check!(
                parse_template_spec_ast(node, ns),
                "failed to parse nested templates at {base_name}"
            )?;
            let name = cu::check!(
                to_namespaced_name(ns, template_name),
                "failed to convert template name to namespaced name"
            )?;
            Ok(TemplateArg::Type(Tree::Base(NamespacedTemplatedName::with_templates(
                name,
                template_args,
            ))))
        }
        Ast::RecordType { ty } => {
            cu::ensure!(node.inner.is_empty(), "RecordType node should have no inner nodes");
            let name = cu::check!(
                to_namespaced_name_with_fallback(ns, &ty.qual_type, elaborated_qual_type),
                "failed to convert record qualified name to namespaced name"
            )?;
            Ok(TemplateArg::Type(Tree::Base(NamespacedTemplatedName::new(name))))
        }
        Ast::EnumType { ty } => {
            cu::ensure!(node.inner.is_empty(), "EnumType node should have no inner nodes");
            let name = cu::check!(
                to_namespaced_name_with_fallback(ns, &ty.qual_type, elaborated_qual_type),
                "failed to convert enum qualified name to namespaced name"
            )?;
            Ok(TemplateArg::Type(Tree::Base(NamespacedTemplatedName::new(name))))
        }
        // *, &, &&
        Ast::PointerType | Ast::LValueReferenceType | Ast::RValueReferenceType => {
            cu::ensure!(node.inner.len() == 1, "{:?} node should have inner length 1", node.kind);
            let node = &node.inner[0];
            let pointee = cu::check!(
                parse_template_arg_ast_recur(node, ns, qualifier, None),
                "failed to parse pointee type"
            )?;
            let TemplateArg::Type(ty) = pointee else {
                cu::bail!("cannot have pointer or reference to constexpr");
            };
            Ok(TemplateArg::Type(Tree::ptr(ty)))
        }
        Ast::TypedefType { ty } => {
            // TODO: do we need fallback here?
            let name = cu::check!(
                to_namespaced_name(ns, &ty.qual_type),
                "failed to convert typedef qualified name to namespaced name"
            )?;
            Ok(TemplateArg::Type(Tree::Base(NamespacedTemplatedName::new(name))))
        }
        Ast::QualType => {
            // qualifier (const, volatile, restrict...)
            // while it's technically possible for these to produce different template
            // specializations, for now, we treat them as the same
            cu::ensure!(node.inner.len() == 1, "QualType node should have inner length 1");
            let node = &node.inner[0];
            parse_template_arg_ast_recur(node, ns, qualifier, None)
        }
        Ast::ParenType => {
            cu::ensure!(node.inner.len() == 1, "ParenType node should have inner length 1");
            let node = &node.inner[0];
            parse_template_arg_ast_recur_paren_type(node, ns)
        }
        Ast::FunctionProtoType => {
            cu::ensure!(
                node.inner.len() >= 1,
                "function proto type must have at least 1 inner node"
            );
            let mut func_types = Vec::with_capacity(node.inner.len());
            for node in &node.inner {
                let inner = cu::check!(
                    parse_template_arg_ast_recur(node, ns, "", None),
                    "failed to parse function type argument type"
                )?;
                let TemplateArg::Type(ty) = inner else {
                    cu::bail!("cannot have constexpr as function arg type");
                };
                func_types.push(ty);
            }
            Ok(TemplateArg::Type(Tree::Sub(func_types)))
        }
        Ast::MemberPointerType => {
            cu::ensure!(
                node.inner.len() == 2,
                "member pointer type must have exactly 2 inner nodes"
            );
            let thisty = cu::check!(
                parse_template_arg_ast_recur(&node.inner[0], ns, "", None),
                "failed to parse this type for member pointer type"
            )?;
            let TemplateArg::Type(thisty) = thisty else {
                cu::bail!("cannot have constexpr as the this type for member pointer type");
            };
            let Tree::Base(thisty) = thisty else {
                cu::bail!("cannot have composite type as the this type for member pointer type");
            };
            let pointee = cu::check!(
                parse_template_arg_ast_recur(&node.inner[1], ns, "", None),
                "failed to parse pointee type for member pointer type"
            )?;
            let TemplateArg::Type(pointee) = pointee else {
                cu::bail!("cannot have constexpr as the pointee type for member pointer type");
            };
            let tree = match pointee {
                Tree::Sub(pointee) => Tree::Ptmf(thisty, pointee),
                other => Tree::Ptmd(thisty, Box::new(other)),
            };
            Ok(TemplateArg::Type(tree))
        }
        _ => {
            cu::bail!("unexpected node while parsing template args: {node:#?}");
        }
    }
}
fn parse_template_arg_ast_recur_paren_type(
    node: &Node<Ast>,
    ns: &NamespaceMaps,
) -> cu::Result<TemplateArg<NamespacedTemplatedName>> {
    match &node.kind {
        Ast::FunctionProtoType => parse_template_arg_ast_recur(node, ns, "", None),
        _ => {
            cu::bail!("unexpected node while parsing ParenType: {node:#?}");
        }
    }
}

fn to_namespaced_name_with_fallback(
    ns: &NamespaceMaps,
    source: &str,
    fallback: Option<&str>,
) -> cu::Result<NamespacedName> {
    match to_namespaced_name(ns, source) {
        Ok(x) => Ok(x),
        Err(e) => {
            let Some(source) = fallback else {
                return Err(e);
            };
            match to_namespaced_name(ns, source) {
                Ok(x) => Ok(x),
                Err(e2) => {
                    cu::error!(
                        "failed to convert source to namespaced name! source={source:?}, fallback={fallback:?}, error1={e:?}, error2={e2:?}"
                    );
                    cu::bail!("failed to convert source to namespaced name with fallback, please see logged errors");
                }
            }
        }
    }
}

fn to_namespaced_name(ns: &NamespaceMaps, source: &str) -> cu::Result<NamespacedName> {
    let (namespace, base) = cu::check!(
        split_namespace(source),
        "failed to split namespace from type name in '{source}'"
    )?;
    if namespace.is_empty() {
        return Ok(NamespacedName::unnamespaced(base));
    }
    match ns.by_src.get(namespace) {
        Some(namespace) => Ok(NamespacedName::namespaced(namespace, base)),
        None => {
            // if the namespace does not contain any templates, we can make a new one
            let namespace = cu::check!(
                Namespace::parse_untemplated(namespace),
                "failed to get namespace by name, and failed to parse new one"
            )?;
            Ok(NamespacedName::namespaced(&namespace, base))
        }
    }
}

fn split_namespace(source: &str) -> cu::Result<(&str, &str)> {
    // this won't work if the last segment
    // has templates, which we don't allow
    match source.rfind("::") {
        Some(i) => {
            let ns = &source[..i];
            let base = &source[i + 2..];
            cu::ensure!(
                !base.contains('>'),
                "base name of type cannot contain templates: {source}"
            );
            Ok((ns, base))
        }
        None => Ok(("", source)),
    }
}
