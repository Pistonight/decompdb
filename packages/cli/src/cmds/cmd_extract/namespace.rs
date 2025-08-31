use std::sync::{
    Arc,
    atomic::{AtomicUsize, Ordering},
};

use cu::pre::*;

use super::pre::*;

/// Load the namespaces in this compilation unit as a global offset map
pub fn load_namespaces(unit: &Unit) -> cu::Result<GoffMap<Namespace>> {
    cu::debug!("loading namespaces for {unit}");
    let mut namespace = NamespaceStack::new();
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
    let tag = entry.tag();
    if is_type_tag(tag) || matches!(tag, DW_TAG_subprogram | DW_TAG_variable) {
        let offset = entry.goff();
        offset_to_ns.insert(offset, namespace.curr_owned());
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
    match tag {
        DW_TAG_compile_unit => {
            // top-most case
            node.for_each_child(|child| read_namespace_recur(child, namespace, offset_to_ns))?;
        }
        DW_TAG_namespace => {
            // doesn't need to add to map for namespace nodes
            let name = entry.name_opt()?;
            namespace.push(name);
            node.for_each_child(|child| read_namespace_recur(child, namespace, offset_to_ns))?;
            namespace.pop();
        }
        _ => {
            // ignore
        }
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

pub type Namespace = Arc<str>;

// used to make unique anonymous namespaces. These need to be completely
// removed in the final output, so it's safe to make this not stable between runs
static ANONYMOUS_COUNT: AtomicUsize = AtomicUsize::new(0);

struct NamespaceStack {
    empty: Arc<str>,
    stack: Vec<Arc<str>>,
}
impl NamespaceStack {
    pub fn new() -> Self {
        Self {
            empty: Arc::from(""),
            stack: Default::default(),
        }
    }
    pub fn push(&mut self, s: Option<&str>) {
        match (s, self.stack.last()) {
            (None, last) => {
                let count = ANONYMOUS_COUNT.fetch_add(1, Ordering::SeqCst);
                let ns = format!("<anonymous_{count}>");
                match last {
                    None => {
                        self.stack.push(Arc::from(ns.as_str()));
                    }
                    Some(last) => {
                        self.stack.push(Arc::from(format!("{last}::{ns}").as_str()));
                    }
                }
            }
            (Some(s), None) => self.stack.push(Arc::from(s)),
            (Some(s), Some(last)) => self.stack.push(Arc::from(format!("{last}::{s}").as_str())),
        }
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn curr(&self) -> &Arc<str> {
        match self.stack.last() {
            None => &self.empty,
            Some(last) => last,
        }
    }

    pub fn curr_owned(&self) -> Arc<str> {
        Arc::clone(self.curr())
    }
}
