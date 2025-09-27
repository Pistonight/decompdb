use std::{collections::BTreeMap, sync::Arc};

use cu::pre::*;

use super::pre::*;

use crate::serde_impl::ArcStr;

pub struct NamespaceMaps {
    /// Goff to the qualifier that goff is in
    pub qualifiers: GoffMap<Namespace>,
    /// Goff to the namespace that goff is in (does not include types, etc)
    pub namespaces: GoffMap<Namespace>,
    /// Source string to namespace
    pub by_src: BTreeMap<String, Namespace>,
}

/// Load the namespaces in this compilation unit as a global offset map
pub fn load_namespaces(unit: &Unit) -> cu::Result<NamespaceMaps> {
    cu::debug!("loading namespaces for {unit}");
    let mut ctx = LoadNamespaceCtx::default();
    cu::check!(
        load_namespaces_root(unit, &mut ctx),
        "failed to load namespaces for {unit}"
    )?;
    let mut by_src_map: BTreeMap<String, Namespace> = Default::default();
    for namespace in ctx.offset_to_qual.values() {
        use std::collections::btree_map::Entry;

        if namespace.contains_anonymous() {
            continue;
        }
        let Ok(source) = namespace.to_cpp_typedef_source() else {
            continue;
        };

        match by_src_map.entry(source) {
            Entry::Vacant(e) => {
                e.insert(namespace.clone());
            }
            Entry::Occupied(e) => {
                let existing = e.get();
                cu::ensure!(
                    existing.source_segs_equal(namespace),
                    "namespace with the same source does not have the same segments: {existing:?} and {namespace:?}"
                )
            }
        }
    }
    Ok(NamespaceMaps {
        qualifiers: ctx.offset_to_qual,
        namespaces: ctx.offset_to_ns,
        by_src: by_src_map,
    })
}

fn load_namespaces_root(unit: &Unit, ctx: &mut LoadNamespaceCtx) -> cu::Result<()> {
    let mut tree = unit.tree()?;
    let root = tree.root()?;
    load_namespace_recur(root, ctx)?;
    Ok(())
}

fn load_namespace_recur(node: DieNode<'_, '_>, ctx: &mut LoadNamespaceCtx) -> cu::Result<()> {
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
            DW_TAG_compile_unit => {
                node.for_each_child(|child| load_namespace_recur(child, ctx))?;
            }
            DW_TAG_variable => {
                ctx.register_current_at_offset(offset);
                node.for_each_child(|child| load_namespace_recur(child, ctx))?;
            }
            // types could be defined inside a function
            DW_TAG_subprogram => {
                ctx.register_current_at_offset(offset);
                ctx.current_qualifier.push(NameSeg::Subprogram(offset));
                node.for_each_child(|child| load_namespace_recur(child, ctx))?;
                ctx.current_qualifier.pop();
            }
            DW_TAG_namespace => {
                ctx.register_current_at_offset(offset);
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

    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        cu::check!(self.0.map_goff(&f), "failed to map namespaced name")
    }
}

#[derive(Default, Clone, PartialEq, Eq, Hash, DebugCustom, Serialize, Deserialize)]
#[debug("{}", self)]
pub struct Namespace(Vec<NameSeg>);
impl Namespace {
    pub fn parse_untemplated(s: &str) -> cu::Result<Self> {
        cu::ensure!(
            !s.contains(['<', '>', '*', '&']),
            "Namespace::parse_untemplated: cannot parse templated namespace: {s}"
        );
        Ok(Self(s.split("::").map(|x| NameSeg::Name(x.trim().into())).collect()))
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn contains_anonymous(&self) -> bool {
        self.0.iter().any(|x| x == &NameSeg::Anonymous)
    }
    pub fn source_segs_equal(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }
        for (a, b) in std::iter::zip(&self.0, &other.0) {
            if !a.source_segs_equal(b) {
                return false;
            }
        }
        true
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
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        for seg in &mut self.0 {
            seg.map_goff(&f)?;
        }
        Ok(())
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
            NameSeg::Anonymous => Ok(None),
        }
    }
    pub fn source_segs_equal(&self, other: &Self) -> bool {
        match (self, other) {
            (NameSeg::Name(a), NameSeg::Name(b)) => a == b,
            (NameSeg::Type(_, a), NameSeg::Type(_, b)) => a == b,
            (NameSeg::Subprogram(a), NameSeg::Subprogram(b)) => a == b,
            (NameSeg::Anonymous, NameSeg::Anonymous) => true,
            _ => false,
        }
    }
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        match self {
            Self::Type(goff, _) => {
                *goff = cu::check!(f(*goff), "failed to map type in namespace")?;
            }
            Self::Subprogram(goff) => {
                *goff = cu::check!(f(*goff), "failed to map subprogram in namespace")?;
            }
            _ => {}
        }
        Ok(())
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
    impl std::fmt::Display for NamespacedName { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0.is_empty() { self.1.fmt(f) } else { write!(f, "{}::{}", self.0, self.1) }
    } }
    // impl TreeRepr for NamespacedName {
    //     fn serialize_spec(&self) -> cu::Result<String> { Ok(json::stringify(self)?) }
    //     fn deserialize_void() -> Self { Self::unnamespaced("void") }
    //     fn deserialize_spec(spec: &str) -> cu::Result<Self> { Ok(json::parse(spec)?) }
    // }
    impl std::fmt::Display for Namespace { fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        let Some(first) = iter.next() else { return Ok(()); };
        write!(f, "{first}")?; for n in iter { write!(f, "::{n}")?; }
        Ok(())
    } }
    // impl Serialize for NamespaceLiteral {
    //     fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
    //         ser.serialize_str(&self.to_string())
    //     }
    // }
}
