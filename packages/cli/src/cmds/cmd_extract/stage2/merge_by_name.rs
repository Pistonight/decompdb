use std::collections::{BTreeMap, BTreeSet};

use cu::pre::*;

use super::super::bucket::GoffBuckets;
use super::super::deduper;
use super::pre::*;

/// Merge types that have the same name
pub fn merge_by_name(stage: &mut Stage1) -> cu::Result<()> {
    let mut goff2names: GoffMap<Vec<StructuredName>> = GoffMap::default();
    for (k, t) in &stage.types {
        match t {
            Type1::Prim(prim) => {
                let e = goff2names.entry(*k).or_default();
                e.push(StructuredName::Name(NamespacedTemplatedName::new(
                    NamespacedName::prim(*prim),
                )));
            }
            Type1::Enum(name, _, other_names) => {
                let e = goff2names.entry(*k).or_default();
                if let Some(name) = name {
                    // empty vec - enum doesn't have templates (checked in earlier stages)
                    e.push(StructuredName::Goff(name.clone(), vec![]));
                }
                for n in other_names {
                    e.push(StructuredName::Name(n.clone()));
                }
            }
            Type1::Union(name, data, other_names) => {
                let e = goff2names.entry(*k).or_default();
                if let Some(name) = name {
                    e.push(StructuredName::Goff(name.clone(), data.template_args.clone()));
                }
                for n in other_names {
                    e.push(StructuredName::Name(n.clone()));
                }
            }
            Type1::Struct(name, data, other_names) => {
                let e = goff2names.entry(*k).or_default();
                if let Some(name) = name {
                    e.push(StructuredName::Goff(name.clone(), data.template_args.clone()));
                }
                for n in other_names {
                    e.push(StructuredName::Name(n.clone()));
                }
            }
            Type1::EnumDecl(name, other_names)
            | Type1::UnionDecl(name, other_names)
            | Type1::StructDecl(name, other_names) => {
                let name = StructuredName::Name(name.clone());
                let e = goff2names.entry(*k).or_default();
                e.push(name);
                for n in other_names {
                    e.push(StructuredName::Name(n.clone()));
                }
            }
        }
    }

    let mut permutater = StructuredNamePermutater::new(goff2names);
    let mut name2goffs_enum = BTreeMap::<String, GoffSet>::new();
    let mut name2goffs_union = BTreeMap::<String, GoffSet>::new();
    let mut name2goffs_struct = BTreeMap::<String, GoffSet>::new();
    for (k, t) in &stage.types {
        let map = match t {
            Type1::Prim(_) => continue,
            Type1::Enum(_, _, _) | Type1::EnumDecl(_, _) => &mut name2goffs_enum,
            Type1::Union(_, _, _) | Type1::UnionDecl(_, _) => &mut name2goffs_union,
            Type1::Struct(_, _, _) | Type1::StructDecl(_, _) => &mut name2goffs_struct,
        };
        let k = *k;
        let names = cu::check!(
            permutater.permutated_string_reprs_goff(k),
            "failed to permutate names for type {k}"
        )?;
        for name in names {
            map.entry(name).or_default().insert(k);
        }
    }

    let mut merge_tasks = {
        let mut to_merge = vec![];
        for (merging_name, goffs) in name2goffs_enum
            .iter()
            .chain(name2goffs_union.iter())
            .chain(name2goffs_struct.iter())
        {
            let v = goffs.iter().copied().collect::<Vec<_>>();
            for (i, k1) in v.iter().copied().enumerate() {
                for k2 in v.iter().skip(i + 1).copied() {
                    to_merge.push((k1, k2, merging_name));
                }
            }
        }
        let mut merge_tasks = BTreeMap::default();
        for (k1, k2, merging_name) in to_merge {
            let key = GoffPair::from((k1, k2));
            if merge_tasks.contains_key(&key) {
                continue;
            }
            let mut task = MergeTask::new(k1, k2);
            let t1 = stage.types.get(&k1).unwrap();
            let t2 = stage.types.get(&k2).unwrap();
            if let Err(e) = t1.add_merge_deps(t2, &mut task) {
                let k1_names = permutater.structured_names(k1);
                let k2_names = permutater.structured_names(k2);
                cu::rethrow!(
                    e,
                    "failed to add merge deps for {k1} and {k2}\n- merging_name={merging_name}, k1_names={k1_names:#?}, k2_names={k2_names:#?}"
                );
            }
            merge_tasks.insert(key, task);
        }
        // detect orphan deps (deps that aren't in merge tasks), and merge them if possible
        // orphan deps can happen if a type is anonymous and does not have a typedef,
        // for example, an anonymous member
        // struct Foo {
        //     union {
        //          int a;
        //          float b;
        //     };
        // };
        let mut changed = true;
        while changed {
            changed = false;
            let mut depmap = BTreeMap::default();
            for task in merge_tasks.values() {
                task.track_deps(&mut depmap);
            }
            let all_deps = depmap.values().flatten().copied().collect::<BTreeSet<_>>();
            let keys = depmap.keys().copied().collect();
            let mut real_orphan_deps = BTreeSet::default();
            for orphan_dep in all_deps.difference(&keys) {
                let (k1, k2) = orphan_dep.to_pair();
                let k1_names = permutater.structured_names(k1);
                let k2_names = permutater.structured_names(k2);
                // one is anonymous, qualified for merging
                // (sometimes a typedef or using is only there in some CU but not others)
                if k1_names.is_empty() || k2_names.is_empty() {
                    let mut task = MergeTask::new(k1, k2);
                    let t1 = stage.types.get(&k1).unwrap();
                    let t2 = stage.types.get(&k2).unwrap();
                    cu::check!(
                        t1.add_merge_deps(t2, &mut task),
                        "failed to add merge deps (from orphan deps) for {k1} and {k2}"
                    )?;
                    merge_tasks.insert((k1, k2).into(), task);
                    changed = true;
                    continue;
                }
                real_orphan_deps.insert(*orphan_dep);
            }
            if !real_orphan_deps.is_empty() {
                let len = real_orphan_deps.len();
                let mut error_string = "orphan deps found:\n".to_string();
                for pair in real_orphan_deps {
                    let (k1, k2) = pair.to_pair();
                    error_string += &format!("- a: {k1} names={:#?}\n", permutater.structured_names(k1));
                    error_string += &format!("  b: {k2} names={:#?}\n", permutater.structured_names(k2));
                    error_string += &format!("  a perm={:#?}\n", permutater.permutated_string_reprs_goff(k1)?);
                    error_string += &format!("  b perm={:#?}\n", permutater.permutated_string_reprs_goff(k2)?);
                    let mut dep_chain = vec![];
                    let mut current = pair;
                    loop {
                        let mut found = vec![];
                        for (key, deps) in &depmap {
                            if !deps.contains(&current) {
                                continue;
                            }
                            found.push(*key);
                        }
                        if found.is_empty() {
                            break;
                        }
                        current = found[0];
                        let more_than_one = found.len() > 1;
                        dep_chain.push(found);
                        if more_than_one {
                            break;
                        }
                    }
                    error_string += &format!("  dep stack: {dep_chain:#?}");
                }
                cu::bail!("{error_string}\n{len} orphan deps found");
            }
        }

        merge_tasks.into_values().collect::<Vec<_>>()
    };

    let mut buckets = GoffBuckets::default();
    loop {
        let len_before = merge_tasks.len();
        for mut task in std::mem::take(&mut merge_tasks) {
            let can_merge = task.update_deps(&buckets);
            if !can_merge {
                merge_tasks.push(task);
                continue;
            }
            task.execute(&mut stage.types, &mut buckets)?;
        }
        if len_before == merge_tasks.len() {
            break;
        }
    }
    if !merge_tasks.is_empty() {
        // remove circular dependencies
        let mut depmap = BTreeMap::default();
        for task in &merge_tasks {
            task.track_deps(&mut depmap);
        }
        let keys = depmap.keys().copied().collect::<Vec<_>>();
        let mut changed = true;
        while changed {
            changed = false;
            for merge in keys.iter().copied() {
                let curr_deps = depmap.get(&merge).unwrap();
                let mut new_deps = curr_deps.clone();
                for d in curr_deps {
                    if let Some(d_deps) = depmap.get(d) {
                        new_deps.extend(d_deps.iter().copied());
                    }
                }
                if curr_deps.len() != new_deps.len() {
                    changed = true;
                    depmap.insert(merge, new_deps);
                }
            }
        }
        let mut circular_depmap = BTreeMap::<GoffPair, BTreeSet<GoffPair>>::default();
        for (merge, all_deps) in &depmap {
            let circular_deps = circular_depmap.entry(*merge).or_default();
            for dep in all_deps {
                if let Some(inverse_deps) = depmap.get(dep) {
                    if inverse_deps.contains(merge) {
                        circular_deps.insert(*dep);
                    }
                }
            }
        }
        for task in &mut merge_tasks {
            task.remove_deps(&circular_depmap);
        }
        // continue merging
        loop {
            let len_before = merge_tasks.len();
            for mut task in std::mem::take(&mut merge_tasks) {
                let can_merge = task.update_deps(&buckets);
                if !can_merge {
                    merge_tasks.push(task);
                    continue;
                }
                task.execute(&mut stage.types, &mut buckets)?;
            }
            if len_before == merge_tasks.len() {
                break;
            }
        }
    }

    if !merge_tasks.is_empty() {
        cu::bail!("not all merges were completed! remaining: {merge_tasks:#?}");
    }

    let deduped = deduper::dedupe_with_merger(
        std::mem::take(&mut stage.types),
        buckets,
        &mut stage.symbols,
        None,
        |data, buckets| data.map_goff(|k| Ok(buckets.primary_fallback(k))),
        |t1, t2| t1.get_merged(t2),
    );
    let deduped = cu::check!(deduped, "merge_by_name: dedupe failed")?;
    stage.types = deduped;

    Ok(())
}
