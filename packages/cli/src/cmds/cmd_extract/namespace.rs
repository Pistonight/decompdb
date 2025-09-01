use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use cu::pre::*;

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
        // types could be defined inside a type
        offset_to_ns.insert(offset, namespace.curr());
        namespace.push(NamespaceSegment::Type(offset));
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
                namespace.push(NamespaceSegment::Subprogram(offset));
            }
            DW_TAG_namespace => {
                // doesn't need to add to map for namespace nodes
                match entry.name_opt()? {
                    Some(name) => namespace.push(NamespaceSegment::Namespace(name.into())),
                    None => {
                        let id = ANONYMOUS_COUNT.fetch_add(1, Ordering::SeqCst);
                        namespace.push(NamespaceSegment::Anonymous(id))
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

    if tag == DW_TAG_subprogram {
    } else if tag == DW_TAG_variable {
    }
    if is_type_tag(tag) || matches!(tag, DW_TAG_subprogram | DW_TAG_variable) {
        let offset = entry.goff();
        offset_to_ns.insert(offset, namespace.curr());
        // can have inner types, and may be anonymous
        let mut name = entry.name_opt()?;

        // for types inside subroutines, the subroutine name might be from abstract_origin
        // TODO: move this to dwarf_parse when doing (recursive) function parsing
        let mut subprogram_name = None;
        if tag == DW_TAG_subprogram && name.is_none() {
            name = entry.str_opt(DW_AT_linkage_name)?;
            if name.is_none() {
                let mut entries = vec![];
                let loff = entry.loff_opt(DW_AT_specification)?;
                if let Some(loff) = loff {
                    let spec_entry = entry.unit().entry_at(loff)?;
                    entries.push(spec_entry);
                }
                let loff = entry.loff_opt(DW_AT_abstract_origin)?;
                if let Some(loff) = loff {
                    let origin_entry = entry.unit().entry_at(loff)?;
                    entries.push(origin_entry);
                }
                while subprogram_name.is_none() {
                    let Some(entry) = entries.pop() else {
                        break;
                    };
                    subprogram_name = entry.name_opt()?.map(|x| x.to_string());
                    if subprogram_name.is_none() {
                        subprogram_name = entry.str_opt(DW_AT_linkage_name)?.map(|x| x.to_string());
                    }
                    if subprogram_name.is_none() {
                        let loff = entry.loff_opt(DW_AT_specification)?;
                        if let Some(loff) = loff {
                            let spec_entry = entry.unit().entry_at(loff)?;
                            entries.push(spec_entry);
                        }
                        let loff = entry.loff_opt(DW_AT_abstract_origin)?;
                        if let Some(loff) = loff {
                            let origin_entry = entry.unit().entry_at(loff)?;
                            entries.push(origin_entry);
                        }
                    }
                }
                name = subprogram_name.as_deref();
            }
        }

        namespace.push(name);
        node.for_each_child(|child| read_namespace_recur(child, namespace, offset_to_ns))?;
        namespace.pop();
        return Ok(());
    }

    Ok(())
}
impl Die<'_, '_> {
    /// Get the name of the entry with namespace prefix
    pub fn namespaced_name(&self, namespaces: &GoffMap<Namespace>) -> cu::Result<String> {
        let name = self.name()?;
        let offset = self.goff();
        let Some(namespace) = namespaces.get(&offset) else {
            cu::bail!("cannot find namespace for entry {offset}, with name '{name}'");
        };
        if namespace.is_empty() {
            return Ok(name.to_string());
        }
        Ok(format!("{namespace}::{name}"))
    }

    /// Get the name of the entry with namespace prefix, optional
    pub fn namespaced_name_opt(&self, namespaces: &GoffMap<Namespace>) -> cu::Result<Option<String>> {
        let Some(name) = self.name_opt()? else {
            return Ok(None);
        };
        let offset = self.goff();
        let Some(namespace) = namespaces.get(&offset) else {
            cu::bail!("cannot find namespace for entry {offset}, with name '{name}'");
        };
        if namespace.is_empty() {
            return Ok(Some(name.to_string()));
        }
        Ok(Some(format!("{namespace}::{name}")))
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Namespace(Vec<NamespaceSegment>);
#[derive(Clone, PartialEq, Eq)]
pub enum NamespaceSegment {
    Namespace(Arc<str>),
    Type(Goff),
    Subprogram(Goff),
    Anonymous(usize),
}
impl std::fmt::Display for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        let Some(first) = iter.next() else {
            return Ok(());
        };
        write!(f, "{first}");
        for n in iter {
            write!(f, "::{n}");
        }
        Ok(())
    }
}
impl std::fmt::Debug for Namespace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
impl std::fmt::Display for NamespaceSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamespaceSegment::Namespace(n) => n.fmt(f),
            NamespaceSegment::Type(goff) => write!(f, "[[ty={goff}]]"),
            NamespaceSegment::Subprogram(goff) => write!(f, "[[subprogram={goff}]]"),
            NamespaceSegment::Anonymous(i) => write!(f, "[[anonymous={i}]]"),
        }
    }
}
impl std::fmt::Debug for NamespaceSegment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

// used to make unique anonymous namespaces. These need to be completely
// removed in the final output, so it's safe to make this not stable between runs
static ANONYMOUS_COUNT: AtomicUsize = AtomicUsize::new(0);

#[derive(Default)]
struct NamespaceStack {
    stack: Vec<NamespaceSegment>,
}
impl NamespaceStack {
    pub fn push(&mut self, s: NamespaceSegment) {
        self.stack.push(s);
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn curr(&self) -> Namespace {
        Namespace(self.stack.clone())
    }
}
