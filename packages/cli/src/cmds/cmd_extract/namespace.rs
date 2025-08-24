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
impl<'i> CompUnit<'_, 'i> {
    /// Load the namespaces in this compilation unit as a global offset map
    pub fn load_namespaces(&self) -> cu::Result<GoffMap<Namespace>> {
        cu::debug!("loading namespaces for {self}");
        let mut namespace = NamespaceStack::new();
        let mut offset_to_ns = GoffMap::new();
        cu::check!(
            self.load_namespaces_root(&mut namespace, &mut offset_to_ns),
            "failed to load namespaces for {self}"
        )?;
        cu::debug!("loaded {} namespaces for {self}", offset_to_ns.len());
        Ok(offset_to_ns)
    }

    /// Get the name of the entry with namespace prefix
    pub fn namespaced_entry_name(
        &self,
        entry: &Die<'i, '_, '_>,
        namespaces: &GoffMap<Namespace>,
    ) -> cu::Result<String> {
        let offset = self.entry_goff(entry);
        let name = self.entry_name(entry)?;
        let Some(namespace) = namespaces.get(&offset) else {
            cu::bail!("cannot find namespace for entry {offset}, with name '{name}'");
        };
        if namespace.is_empty() {
            return Ok(name.to_string());
        }
        Ok(format!("{namespace}::{name}"))
    }

    /// Get the name of the entry with namespace prefix, optional
    pub fn namespaced_entry_name_opt(
        &self,
        entry: &Die<'i, '_, '_>,
        namespaces: &GoffMap<Namespace>,
    ) -> cu::Result<Option<String>> {
        let offset = self.entry_goff(entry);
        let Some(name) = self.entry_name_opt(entry)? else {
            return Ok(None);
        };
        let Some(namespace) = namespaces.get(&offset) else {
            cu::bail!("cannot find namespace for entry {offset}, with name '{name}'");
        };
        if namespace.is_empty() {
            return Ok(Some(name.to_string()));
        }
        Ok(Some(format!("{namespace}::{name}")))
    }

    fn load_namespaces_root(
        &self,
        namespace: &mut NamespaceStack,
        offset_to_ns: &mut GoffMap<Namespace>,
    ) -> cu::Result<()> {
        let mut tree = self.tree()?;
        let root = tree.root()?;
        self.read_namespace_recur(root, namespace, offset_to_ns)?;
        Ok(())
    }

    fn read_namespace_recur(
        &self,
        node: Node<'i, '_, '_, '_>,
        namespace: &mut NamespaceStack,
        offset_to_ns: &mut GoffMap<Namespace>,
    ) -> cu::Result<()> {
        let entry = node.entry();
        let tag = entry.tag();
        if is_type_tag(tag) || matches!(tag, DW_TAG_subprogram | DW_TAG_variable) {
            let offset = self.entry_goff(entry);
            offset_to_ns.insert(offset, namespace.curr_owned());
            // can have inner types, and may be anonymous
            let name = self.entry_name_opt(entry)?;
            namespace_scope! {
                namespace, name, {
                    self.for_each_child(node, |child| {
                        self.read_namespace_recur(child, namespace, offset_to_ns)
                    })?;
                }
            };
            return Ok(());
        }
        match tag {
            DW_TAG_compile_unit => {
                // top-most case
                self.for_each_child(node, |child| self.read_namespace_recur(child, namespace, offset_to_ns))?;
            }
            DW_TAG_namespace => {
                // doesn't need to add to map for namespace nodes
                let name = self.entry_name_opt(entry)?;
                namespace_scope! {
                    namespace, name, {
                        self.for_each_child(node, |child| {
                            self.read_namespace_recur(child, namespace, offset_to_ns)
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
