use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use cu::pre::*;

use crate::serde_impl::ArcStr;

use super::pre::*;

/// Load the namespaces in this compilation unit as a global offset map
pub fn load_namespaces(unit: &Unit) -> cu::Result<GoffMap<Namespace>> {
    cu::debug!("loading namespaces for {unit}");
    let mut namespace = NamespaceStack::default();
    let mut offset_to_ns = GoffMap::new();
    cu::check!(
        load_namespaces_root(unit, &mut namespace, &mut offset_to_ns),
        "failed to load namespaces for {unit}"
    )?;
    cu::debug!("loaded {} namespaces for {unit}", offset_to_ns.len());
    Ok(offset_to_ns)
}

fn load_namespaces_root(
    unit: &Unit,
    namespace: &mut NamespaceStack,
    offset_to_ns: &mut GoffMap<Namespace>,
) -> cu::Result<()> {
    let mut tree = unit.tree()?;
    let root = tree.root()?;
    read_namespace_recur(root, namespace, offset_to_ns)?;
    Ok(())
}

fn read_namespace_recur(
    node: DieNode<'_, '_>,
    namespace: &mut NamespaceStack,
    offset_to_ns: &mut GoffMap<Namespace>,
) -> cu::Result<()> {
    let entry = node.entry();
    let offset = entry.goff();
    let tag = entry.tag();
    if is_type_tag(tag) {
        let name = entry.name()?;
        // types could be defined inside a type
        offset_to_ns.insert(offset, namespace.curr());
        namespace.push(NameSeg::Type(offset, name.into()));
    } else {
        match tag {
            // simply recurse
            DW_TAG_variable | DW_TAG_compile_unit => {
                node.for_each_child(|child| read_namespace_recur(child, namespace, offset_to_ns))?;
                return Ok(());
            }
            // types could be defined inside a function
            DW_TAG_subprogram => {
                offset_to_ns.insert(offset, namespace.curr());
                namespace.push(NameSeg::Subprogram(offset));
            }
            DW_TAG_namespace => {
                // doesn't need to add to map for namespace nodes
                match entry.name_opt()? {
                    Some(name) => namespace.push(NameSeg::Name(name.into())),
                    None => {
                        // let id = ANONYMOUS_COUNT.fetch_add(1, Ordering::SeqCst);
                        namespace.push(NameSeg::Anonymous)
                    }
                };
            }
            _ => {
                // ignore
                return Ok(());
            }
        }
    }
    // recurse into the node with pushed stack
    node.for_each_child(|child| read_namespace_recur(child, namespace, offset_to_ns))?;
    namespace.pop();

    Ok(())
}
impl Die<'_, '_> {
    /// Get the name of the entry with namespace prefix, without templated args
    pub fn namespaced_untemplated_name(&self, namespaces: &GoffMap<Namespace>) -> cu::Result<NamespacedName> {
        let name = self.untemplated_name()?;
        Self::combine_namespace_and_name(namespaces, self.goff(), name)
    }
    /// Get the name of the entry with namespace prefix, without templated args
    pub fn namespaced_untemplated_name_opt(&self, namespaces: &GoffMap<Namespace>) -> cu::Result<Option<NamespacedName>> {
        let Some(name) = self.untemplated_name_opt()? else {
            return Ok(None);
        };
        Self::combine_namespace_and_name(namespaces, self.goff(), name).map(Some)
    }
    /// Get the name of the entry with namespace prefix
    pub fn namespaced_name(&self, namespaces: &GoffMap<Namespace>) -> cu::Result<NamespacedName> {
        let name = self.name()?;
        Self::combine_namespace_and_name(namespaces, self.goff(), name)
    }

    /// Get the name of the entry with namespace prefix, optional
    pub fn namespaced_name_opt(&self, namespaces: &GoffMap<Namespace>) -> cu::Result<Option<NamespacedName>> {
        let Some(name) = self.name_opt()? else {
            return Ok(None);
        };
        Self::combine_namespace_and_name(namespaces, self.goff(), name).map(Some)
    }

    fn combine_namespace_and_name(namespaces: &GoffMap<Namespace>, offset: Goff, name: &str) -> cu::Result<NamespacedName> {
        let namespace = cu::check!(
            namespaces.get(&offset),
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

    pub fn namespace(&self) -> &[NameSeg] {
        self.0.0.as_slice()
    }

    /// Convert the namespaced name to string that can be used as a type
    /// in CPP. If the namespace involves a subprogram, Err is returned
    pub fn to_cpp_typedef_source(&self) -> cu::Result<String> {
        let mut s = String::new();
        for n in self.namespace() {
            if let Some(x) = n.to_cpp_source()? {
                if !s.is_empty() {
                    s.push_str("::");
                }
                s.push_str(x);
            }
        }
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
}

#[derive(Default, Clone, PartialEq, Eq, Hash, DebugCustom, Serialize, Deserialize)]
pub struct NamespaceLiteral(Vec<ArcStr>);
impl NamespaceLiteral {
    pub fn parse(name: &str) -> cu::Result<Self> {
        let parts: Vec<ArcStr> = name.split("::").map(|x| x.trim().into()).collect();
        cu::ensure!(!parts.is_empty(), "namespaced name must be non-empty");
        Ok(Self(parts))
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Display, DebugCustom, Serialize, Deserialize)]
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

// used to make unique anonymous namespaces. These need to be completely
// removed in the final output, so it's safe to make this not stable between runs
static ANONYMOUS_COUNT: AtomicUsize = AtomicUsize::new(0);

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
}
