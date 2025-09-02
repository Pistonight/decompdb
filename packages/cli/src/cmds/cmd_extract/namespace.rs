use std::sync::Arc;

use cu::pre::*;

use super::pre::*;

use crate::serde_impl::ArcStr;

pub struct NamespaceMaps {
    pub qualifiers: GoffMap<Namespace>,
    pub namespaces: GoffMap<Namespace>,
}

/// Load the namespaces in this compilation unit as a global offset map
pub fn load_namespaces(unit: &Unit) -> cu::Result<NamespaceMaps> {
    cu::debug!("loading namespaces for {unit}");
    let mut ctx = LoadNamespaceCtx::default();
    cu::check!(
        load_namespaces_root(unit, &mut ctx),
        "failed to load namespaces for {unit}"
    )?;
    Ok(NamespaceMaps { qualifiers: ctx.offset_to_qual, namespaces: ctx.offset_to_ns })
}

fn load_namespaces_root(
    unit: &Unit,
    ctx: &mut LoadNamespaceCtx
) -> cu::Result<()> {
    let mut tree = unit.tree()?;
    let root = tree.root()?;
    load_namespace_recur(root, ctx)?;
    Ok(())
}

fn load_namespace_recur(
    node: DieNode<'_, '_>,
    ctx: &mut LoadNamespaceCtx
) -> cu::Result<()> {
    let entry = node.entry();
    let offset = entry.goff();
    let tag = entry.tag();
    if is_type_tag(tag) {
        // types could be defined inside a type
        ctx.register_current_at_offset(offset);
        // only push the qualifier stack for types
        match entry.name_opt()? {
            Some(name) => {
                ctx.current_qualifier.push(NameSeg::Type(offset, name.into()));
            }
            None => {
                ctx.current_qualifier.push(NameSeg::Anonymous);
            }
        }
        node.for_each_child(|child| load_namespace_recur(child, ctx))?;
        ctx.current_qualifier.pop();
    } else {
        match tag {
            // simply recurse
            DW_TAG_variable | DW_TAG_compile_unit => {
                node.for_each_child(|child| load_namespace_recur(child, ctx))?;
                return Ok(());
            }
            // types could be defined inside a function
            DW_TAG_subprogram => {
                ctx.register_current_at_offset(offset);
                ctx.current_qualifier.push(NameSeg::Subprogram(offset));
                node.for_each_child(|child| load_namespace_recur(child, ctx))?;
                ctx.current_qualifier.pop();
            }
            DW_TAG_namespace => {
                // doesn't need to add to map for namespace nodes
                match entry.name_opt()? {
                    Some(name) => {
                        let seg = NameSeg::Name(name.into());
                        ctx.current_qualifier.push(seg.clone());
                        ctx.current_namespace.push(seg);
                    }
                    None => {
                        // let id = ANONYMOUS_COUNT.fetch_add(1, Ordering::SeqCst);
                        ctx.current_qualifier.push(NameSeg::Anonymous);
                        ctx.current_namespace.push(NameSeg::Anonymous);
                    }
                };
                node.for_each_child(|child| load_namespace_recur(child, ctx))?;
                ctx.current_qualifier.pop();
                ctx.current_namespace.pop();
            }
            _ => {
                // ignore
                return Ok(());
            }
        }
    }

    Ok(())
}
impl Die<'_, '_> {
    /// Get the name of the entry with namespace prefix, without templated args
    pub fn untemplated_qual_name(&self, namespaces: &NamespaceMaps) -> cu::Result<NamespacedName> {
        let name = self.untemplated_name()?;
        Self::make_qual_name(namespaces, self.goff(), name)
    }
    /// Get the name of the entry with namespace prefix, without templated args
    pub fn untemplated_qual_name_opt(&self, nsmaps: &NamespaceMaps) -> cu::Result<Option<NamespacedName>> {
        let Some(name) = self.untemplated_name_opt()? else {
            return Ok(None);
        };
        Self::make_qual_name(nsmaps, self.goff(), name).map(Some)
    }
    /// Get the name of the entry with namespace prefix
    pub fn qual_name(&self, nsmaps: &NamespaceMaps) -> cu::Result<NamespacedName> {
        let name = self.name()?;
        Self::make_qual_name(nsmaps, self.goff(), name)
    }

    /// Get the name of the entry with namespace prefix, optional
    pub fn qual_name_opt(&self, nsmaps: &NamespaceMaps) -> cu::Result<Option<NamespacedName>> {
        let Some(name) = self.name_opt()? else {
            return Ok(None);
        };
        Self::make_qual_name(nsmaps, self.goff(), name).map(Some)
    }

    fn make_qual_name(nsmaps: &NamespaceMaps, offset: Goff, name: &str) -> cu::Result<NamespacedName> {
        let namespace = cu::check!(
            nsmaps.qualifiers.get(&offset),
            "cannot find namespace for entry {offset}, with name '{name}'"
        )?;
        Ok(NamespacedName::namespaced(namespace, name))
    }
}

#[derive(Clone, PartialEq, Eq, Hash, DebugCustom, Serialize, Deserialize)]
#[debug("{}", self)]
pub struct NamespacedName(Namespace, ArcStr);
impl NamespacedName {
    pub fn unnamespaced(name: &str) -> Self {
        Self(Default::default(), name.into())
    }

    pub fn namespaced(namespace: &Namespace, name: &str) -> Self {
        Self(namespace.clone(), name.into())
    }

    pub fn basename(&self) -> &str {
        &self.1
    }
    
    pub fn basename_owned(&self) -> Arc<str> {
        Arc::clone(&self.1)
    }

    pub fn namespace(&self) -> &Namespace {
        &self.0
    }

    /// Convert the namespaced name to string that can be used as a type
    /// in CPP. If the namespace involves a subprogram, Err is returned
    pub fn to_cpp_typedef_source(&self) -> cu::Result<String> {
        let mut s = self.namespace().to_cpp_typedef_source()?;
        if !s.is_empty() {
            s.push_str("::");
        }
        s.push_str(&self.1);
        Ok(s)
    }
}
impl std::fmt::Display for NamespacedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() {
            self.1.fmt(f)
        } else {
            write!(f, "{}::{}", self.0, self.1)
        }
    }
}

#[derive(Default, Clone, PartialEq, Eq, Hash, DebugCustom, Serialize, Deserialize)]
#[debug("{}", self)]
pub struct Namespace(Vec<NameSeg>);
impl Namespace {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn to_cpp_typedef_source(&self) -> cu::Result<String> {
        let mut s = String::new();
        for n in &self.0 {
            if let Some(x) = n.to_cpp_source()? {
                if !s.is_empty() {
                    s.push_str("::");
                }
                s.push_str(x);
            }
        }
        Ok(s)
    }
}

#[derive(Default, Clone, PartialEq, Eq, Hash, DebugCustom)]
pub struct NamespaceLiteral(Vec<ArcStr>);
impl NamespaceLiteral {
    pub fn new(name: &str) -> Self {
        Self(vec![name.into()])
    }
    pub fn parse(name: &str) -> cu::Result<Self> {
        let parts: Vec<ArcStr> = name.split("::").map(|x| x.trim().into()).collect();
        cu::ensure!(!parts.is_empty(), "namespaced name must be non-empty");
        Ok(Self(parts))
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Display, DebugCustom, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum NameSeg {
    #[display("{}", _0)]
    #[debug("{}", _0)]
    Name(ArcStr),

    #[display("[ty={}]", _0)]
    #[debug("[ty={}]", _0)]
    Type(Goff, ArcStr),

    #[display("[subprogram={}]", _0)]
    #[debug("[subprogram={}]", _0)]
    Subprogram(Goff),

    #[display("[anonymous]")]
    #[debug("[anonymous]")]
    Anonymous,
}

impl NameSeg {
    pub fn to_cpp_source(&self) -> cu::Result<Option<&str>> {
        match self {
            NameSeg::Name(s) => Ok(Some(s.as_ref())),
            NameSeg::Type(_, s) => Ok(Some(s.as_ref())),
            NameSeg::Subprogram(_) => {
                cu::bail!("to_cpp_source does not support subprogram as namespace");
            }
            NameSeg::Anonymous => Ok(None)
        }
    }
}

// // used to make unique anonymous namespaces. These need to be completely
// // removed in the final output, so it's safe to make this not stable between runs
// static ANONYMOUS_COUNT: AtomicUsize = AtomicUsize::new(0);
//
#[derive(Default)]
struct LoadNamespaceCtx {
    // the difference between qualifier and namespace
    // is that qualifier contains types/subprograms,
    // while namespace only contains namespaces
    current_qualifier: NamespaceStack,
    current_namespace: NamespaceStack,
    offset_to_ns: GoffMap<Namespace>,
    offset_to_qual: GoffMap<Namespace>,
}

impl LoadNamespaceCtx {
    fn register_current_at_offset(&mut self, off: Goff) {
        self.offset_to_qual.insert(off, self.current_qualifier.curr());
        self.offset_to_ns.insert(off, self.current_namespace.curr());
    }
}

#[derive(Default)]
struct NamespaceStack {
    stack: Vec<NameSeg>,
}
impl NamespaceStack {
    pub fn push(&mut self, s: NameSeg) {
        self.stack.push(s);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn curr(&self) -> Namespace {
        Namespace(self.stack.clone())
    }
}

#[rustfmt::skip]
mod __detail {
    use super::*;
    impl std::fmt::Display for Namespace { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        let Some(first) = iter.next() else { return Ok(()); };
        write!(f, "{first}")?; for n in iter { write!(f, "::{n}")?; }
        Ok(())
    } }
    impl std::fmt::Display for NamespaceLiteral { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        let Some(first) = iter.next() else { return Ok(()); };
        write!(f, "{first}")?; for n in iter { write!(f, "::{n}")?; }
        Ok(())
    } }
    impl Serialize for NamespaceLiteral {
        fn serialize<
        S: serde::Serializer 
        >(&self, ser: S) -> Result<S::Ok, S::Error>
        {
            ser.serialize_str(&self.to_string())
            
        }
    }
}
