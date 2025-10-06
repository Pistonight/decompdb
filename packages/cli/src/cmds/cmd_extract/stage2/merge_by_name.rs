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
                e.push(
                    StructuredName::Name(NamespacedTemplatedName::new(
                        NamespacedName::prim(*prim)
                    ))
                );
            },
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
            Type1::EnumDecl(name, other_names) |
            Type1::UnionDecl(name, other_names)|
            Type1::StructDecl(name, other_names) 
            => {
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
        let names = cu::check!(permutater.permutated_string_reprs_goff(k), "failed to permutate names for type {k}")?;
        for name in names {
            map.entry(name).or_default().insert(k);
        }
    }

// {
//         for k in name2goffs_struct.keys() {
//             if k.contains("PauseMenuDataMgr") {
//                 cu::print!("{k}");
//             }
//         }
//         let x = name2goffs_struct.get("uking::ui::PauseMenuDataMgr");
//         if let Some(x) = x {
//             cu::print!("PMDM: {x:?}");
//         }
//     }

    let mut to_merge = BTreeSet::new();
    for goffs in name2goffs_enum.values()
        .chain(name2goffs_union.values())
        .chain(name2goffs_struct.values())
    {
        let v = goffs.iter().copied().collect::<Vec<_>>();
        for (i, k1) in v.iter().copied().enumerate() {
            for k2 in v.iter().skip(i+1).copied() {
                to_merge.insert((k1, k2));
            }
        }
    }
    let mut merge_tasks = vec![];
    for (k1, k2) in to_merge {
        let mut task = MergeTask::new(k1, k2);
        let t1 = stage.types.get(&k1).unwrap();
        let t2 = stage.types.get(&k2).unwrap();
        cu::check!(t1.add_merge_deps(t2, &mut task), "failed to add merge deps for {k1} and {k2}")?;
        merge_tasks.push(task);
    }

    let mut buckets = GoffBuckets::default();
    loop {
        let len_before = merge_tasks.len();
        for mut task in std::mem::take(&mut merge_tasks) {
            let can_merge = task.update_deps(&buckets);
            if !can_merge {
                merge_tasks.push(task);
                continue;
            }
            let (k1, k2) = task.merge_pair();
            let t1 = stage.types.get(&k1).unwrap();
            let t2 = stage.types.get(&k2).unwrap();
            let merged = cu::check!(t1.get_merged(t2), "failed to merge types {k1} and {k2}")?;
            stage.types.insert(k1, merged.clone());
            stage.types.insert(k2, merged);
            cu::check!(buckets.merge(k1, k2), "failed to merge {k1} and {k2} in buckets")?;
        }
        if len_before == merge_tasks.len() {
            break;
        }
    }

    if !merge_tasks.is_empty() {
        cu::bail!("not all merges were completed! remaining: {merge_tasks:#?}");
    }

    let deduped = deduper::dedupe(std::mem::take(&mut stage.types), 
        buckets, &mut stage.symbols, None, |data, buckets| {
        data.map_goff(|k| Ok(buckets.primary_fallback(k)))
    });
    let deduped = cu::check!(deduped, "merge_by_name: dedupe failed")?;
    stage.types = deduped;

    Ok(())
}
