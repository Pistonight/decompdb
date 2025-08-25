use std::collections::BTreeMap;
use std::sync::Arc;

use spin::RwLock as SpinRwLock;

use crate::config::CfgExtract;

#[derive(Clone)]
pub struct NameComparator {
    config: Arc<CfgExtract>,
    cache: Arc<SpinRwLock<BTreeMap<String, usize>>>,
}

impl NameComparator {
    pub fn new(config: Arc<CfgExtract>) -> Self {
        Self {
            config,
            cache: Default::default(),
        }
    }

    pub fn compare(&self, a: &str, b: &str) -> std::cmp::Ordering {
        self.get_key(a).cmp(&self.get_key(b))
    }

    pub fn get_key(&self, name: &str) -> usize {
        let read_guard = match self.cache.try_upgradeable_read() {
            None => return self.config.name_resolution.rules.get_sort_key(name),
            Some(g) => g,
        };
        if let Some(x) = read_guard.get(name) {
            return *x;
        }
        let value = self.config.name_resolution.rules.get_sort_key(name);
        if let Ok(mut write_guard) = read_guard.try_upgrade() {
            write_guard.insert(name.to_string(), value);
        }
        value
    }
}
