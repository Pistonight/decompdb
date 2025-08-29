use std::sync::Arc;

use cu::pre::*;

use super::pre::*;

macro_rules! namespace_scope {
    ($namespace:ident, $name:expr, $block:block) => {{
        match $name {
            Some(name) => {
                $namespace.push(name);
                $block
                $namespace.pop();
            }
            None => {
                $block
            }
        }
    }};
}

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
        let name = entry.name_opt()?;
        namespace_scope! {
            namespace, name, {
                node.for_each_child(|child| {
                    read_namespace_recur(child, namespace, offset_to_ns)
                })?;
            }
        };
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
            namespace_scope! {
                namespace, name, {
                    node.for_each_child(|child| {
                        read_namespace_recur(child, namespace, offset_to_ns)
                    })?;
                }
            };
        }
        _ => {
            // ignore
        }
    }

    Ok(())
}
impl Die<'_, '_> {
    /// Get the name of the entry with namespace prefix
    pub fn namespaced_name(
        &self,
        namespaces: &GoffMap<Namespace>,
    ) -> cu::Result<String> {
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
    pub fn namespaced_name_opt(
        &self,
        namespaces: &GoffMap<Namespace>,
    ) -> cu::Result<Option<String>> {
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
    pub fn push(&mut self, s: &str) {
        match self.stack.last() {
            None => self.stack.push(Arc::from(s)),
            Some(last) => self.stack.push(Arc::from(format!("{last}::{s}").as_str())),
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
