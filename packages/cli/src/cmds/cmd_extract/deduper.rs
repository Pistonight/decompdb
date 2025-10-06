use std::collections::{BTreeMap, BTreeSet};
use std::hash::{Hash, Hasher};

use cu::pre::*;
use fxhash::FxHasher;

use super::bucket::GoffBuckets;
use super::pre::*;
use super::type_structure::*;

/// Dedupe goffs that map to the same type data
pub fn dedupe<T: Eq + Hash, F: Fn(&mut T, &GoffBuckets) -> cu::Result<()>>(
    mut map: GoffMap<T>,
    mut buckets: GoffBuckets,
    symbols: &mut BTreeMap<String, SymbolInfo>,
    namespace: Option<&mut NamespaceMaps>,
    mapper: F,
) -> cu::Result<GoffMap<T>> {
    loop {
        // must run mapper first to make sure collision and merge check
        // picks up the change
        let mut new_map = GoffMap::default();
        for (goff, mut t) in map {
            use std::collections::btree_map::Entry;

            let k = buckets.primary_fallback(goff);
            cu::check!(
                mapper(&mut t, &buckets),
                "failed to run mapper for {goff} (primary: {k})"
            )?;
            match new_map.entry(k) {
                Entry::Occupied(e) => {
                    cu::ensure!(
                        e.get() == &t,
                        "failed to merge {goff} into {k}: the data are not equal after mapping, please check the mapper implementation"
                    );
                }
                Entry::Vacant(e) => {
                    e.insert(t);
                }
            }
        }

        map = new_map;

        let mut hash_map = BTreeMap::default();
        let mut has_collision = false;
        for (goff, t) in &map {
            let mut hasher = FxHasher::default();
            t.hash(&mut hasher);
            let h = hasher.finish();
            let set = hash_map.entry(h).or_insert_with(|| {
                has_collision = true;
                BTreeSet::new()
            });
            set.insert(*goff);
        }

        let mut has_merges = false;
        if has_collision {
            for keys in hash_map.into_values() {
                let keys = keys.into_iter().collect::<Vec<_>>();
                for (i, k) in keys.iter().copied().enumerate() {
                    for j in keys.iter().skip(i + 1).copied() {
                        let t1 = map.get(&k).unwrap();
                        let t2 = map.get(&j).unwrap();
                        if t1 == t2 {
                            cu::check!(buckets.merge(k, j), "failed to merge goff {k} and {j}")?;
                            has_merges = true;
                        }
                    }
                }
            }
        }

        if has_merges {
            continue;
        }

        let f: GoffMapFn = Box::new(|k| Ok(buckets.primary_fallback(k)));
        for symbol in symbols.values_mut() {
            cu::check!(symbol.map_goff(&f), "symbol mapping failed when deduping")?;
        }

        if let Some(ns) = namespace {
            for n in ns.qualifiers.values_mut() {
                cu::check!(n.map_goff(&f), "qualifier mapping failed when deduping")?;
            }
            for n in ns.namespaces.values_mut() {
                cu::check!(n.map_goff(&f), "namespace mapping failed when deduping")?;
            }
            for n in ns.by_src.values_mut() {
                cu::check!(n.map_goff(&f), "namespace by_src mapping failed when deduping")?;
            }
        }

        return Ok(map);
    }
}
