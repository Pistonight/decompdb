use std::cmp::Reverse;
use std::collections::{BTreeSet, BinaryHeap};

use cu::pre::*;
use tyyaml::Prim;

use super::type_compiler::TypeCompilerUnit;
use super::pre::*;

pub async fn link_types(types: Vec<TypeCompilerUnit>) -> cu::Result<TypeCompilerUnit> {
    // The type linking is done in 3 phases:
    // 1. Merge small TCUs together, until the type counts in one CU becomes
    //    large enough that optimization is inefficient
    // 2. "zip" types by name between 2 TCUs. For example, 2 structs of the same name
    //    must have all of their members also be the same type. Then, run
    //    optimizations again.

    struct Entry(usize, TypeCompilerUnit);
    impl PartialEq for Entry {
        fn eq(&self, other: &Self) -> bool {
            self.0 == other.0
        }
    }
    impl Eq for Entry {}
    impl PartialOrd for Entry {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            self.0.partial_cmp(&other.0)
        }
    }
    impl Ord for Entry {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.0.cmp(&other.0)
        }
    }

    let bar = cu::progress_unbounded(format!("linking {} type compiler units", types.len()));
    let mut total = types.len();

    // use a heap to continually merge 2 smallest units
    let mut heap = BinaryHeap::new();
    for compiler in types {
        let count = compiler.compiled.buckets().count();
        heap.push(Reverse(Entry(count, compiler)));
    }

    let mut merge_count_limit = 100;
    while heap.len() > 1 {
        let mut handles = Vec::with_capacity(heap.len() / 2 + 1);
        let pool = cu::co::pool(0);
        loop {
            let Some(unit1) = heap.pop() else  {
                break;
            };
            if unit1.0.0 > merge_count_limit {
                heap.push(unit1);
                break;
            }
            let Some(unit2) = heap.pop() else {
                heap.push(unit1);
                break;
            };
            if unit2.0.0 > merge_count_limit {
                heap.push(unit2);
                break;
            }
            let mut unit1 = unit1.0.1;
            let unit2 = unit2.0.1;
            let handle = pool.spawn(async move {
                let unit2_name = unit2.name.clone();
                if merge_count_limit >= 12800 {
                    cu::check!(unit1.link_zip_with(unit2), "failed to link zip types from {} with {unit2_name}", unit1.name)?;
                } else {
                    cu::check!(unit1.link_with(unit2), "failed to link types from {} with {unit2_name}", unit1.name)?;
                }
                cu::Ok(unit1)
            });
            // join_set.spawn(async move {handle.co_join().await});
            handles.push(handle);
        }
        if handles.is_empty() {
            merge_count_limit *= 2;
            continue;
        }
        let mut set = cu::co::set(handles);
        while let Some(result) = set.next().await {
            let unit = result.flatten()?;
            total -= 1;
            let count = unit.compiled.buckets().count();
            cu::progress!(&bar, (), "remaining units: {total}, merge size: {merge_count_limit}");
            heap.push(Reverse(Entry(count, unit)));
        };
    }

    let final_units = heap.into_iter().map(|x| x.0.1).collect::<Vec<_>>();
    // drop(bar);
    // let bar = cu::progress_bar(final_units.len(), "merging all types into one unit");
    // let mut iter = final_units.into_iter();
    // let mut unit = cu::check!(iter.next(), "unexpected empty vec")?;
    // for (i, u) in iter.enumerate() {
    //     unit.merge_with(u)?;
    //     cu::progress!(&bar, i+1);
    // }
    // drop(bar);
    // let bar = cu::progress_unbounded("optimizing types");
    // cu::check!(unit.optimize(Some(Arc::clone(&bar))), "final optimization failed")?;

    let mut unit = final_units.into_iter().next().unwrap();

    cu::info!("optimizing layouts");
    cu::check!(unit.optimize(true), "final optimization failed")?;

    let stat = unit.type_stats();
    cu::info!("{stat:#?}");

    Ok(unit.link_done())
}

impl TypeCompilerUnit {
    pub fn link_with(&mut self, other: Self) -> cu::Result<()> {
        self.merge_with(other)?;
        cu::check!(self.optimize(false), "optimization failed")?;
        Ok(())
    }
    pub fn link_zip_with(&mut self, other: Self) -> cu::Result<()> {
        self.link_with(other)
        // self.merge_with(other)?;
        // let mut pass = 1;
        // loop {
        //     cu::debug!("link_zip pass {pass}");
        //     let zip_changed = cu::check!(self.zip(), "zip failed")?;
        //     let opt_changed = cu::check!(self.optimize_once(false), "optimization failed")?;
        //     if !zip_changed && !opt_changed {
        //         break;
        //     }
        //     pass += 1;
        // }
        // Ok(())
    }

    /// Link the compiled types from 2 compiler units
    pub fn merge_with(&mut self, mut other: Self) -> cu::Result<()> {
        self.types.extend(other.types);
        self.sizes.clear();
        // since both compilers have a copy of primitive types, we need to dedupe it
        for p in Prim::iter() {
            let goff = Goff::prim(p);
            if let Some(bucket) = other.compiled.remove(goff) {
                cu::check!(other.compiled.check(), "bucket map is invalid after removing primitive key {goff}")?;
                let bucket_key = bucket.canonical_key();
                let mut keys = bucket.other_keys;
                keys.insert(bucket_key);
                keys.remove(&goff);

                for k in keys {
                    cu::check!(self.compiled.add_key(goff, k), "error while adding primitive key {goff} with alias key {k}")?;
                }
            }
        }
        self.compiled.extend(other.compiled).context("failed to merge 2 type compiler units")?;
        cu::ensure!(self.merges.is_empty(), "cannot link with type compiler unit with leftover merges!");
        cu::ensure!(other.merges.is_empty(), "cannot link with type compiler unit with leftover merges!");
        cu::ensure!(self.changes.is_empty(), "cannot link with type compiler unit with leftover changes!");
        cu::ensure!(other.changes.is_empty(), "cannot link with type compiler unit with leftover changes!");

        // cu::check!(self.optimize(), "optimization failed")?;
        // if self.name != "<linked>" {
        //     self.name = "<linked>".to_string();
        // }
        Ok(())
    }

    fn zip(&mut self) -> cu::Result<bool> {
        let typed_keys = self.categorized_type_keys();
        let mut merges = BTreeSet::new();
        macro_rules! zip_type {
            ($set:ident) => {
                for j in &typed_keys.$set {
                    let j_unit = &self.compiled.get_unwrap(*j)?.value;
                    for k in &typed_keys.$set {
                        if k <= j {
                            continue;
                        }
                        let k_unit = &self.compiled.get_unwrap(*k)?.value;
                        let zipped = cu::check!(j_unit.zip(k_unit, &self.compiled, &mut merges), "zip types {j} and {k} failed")?;
                        if zipped {
                            merges.insert((*j, *k));
                        }
                    }
                }
            }
        }
        zip_type!(unions);
        zip_type!(structs);
        // zip_type!(tree_arrays);
        // zip_type!(tree_ptrs);
        // zip_type!(tree_subs);
        // zip_type!(tree_ptmds);
        // zip_type!(tree_ptmfs);
        if !merges.is_empty() {
            // cu::debug!("zip merges: {merges:?}");
            self.apply_merges_from(merges.into_iter().filter(|(k, j)| k!=j))?;
            cu::check!(self.canonicalize(), "failed to canonicalize after zipping")?;
            return Ok(true);
        }
        Ok(false)

    }
    pub fn link_done(mut self) -> Self {
        // remove names for pritimive types
        for p in Prim::iter() {
            let goff = Goff::prim(p);
            if let Some(data) = self.compiled.get_mut(goff) {
                data.typedef_names.clear();
                data.declared_names.clear();
            }
        }
        self
    }
}
