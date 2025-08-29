use std::cell::Cell;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

use cu::pre::*;
use fxhash::FxHashMap;
use fxhash::FxHasher64;
use rayon::prelude::*;
use linkme::distributed_slice;
use tyyaml::Tree;

use super::BucketMap;
use super::pre::*;
use super::type_compiler::{TypeUnit, TypeCompilerUnit, TypeUnitData};

pub static OPTIMIZERS: &[fn(&mut TypeCompilerUnit, bool) -> cu::Result<bool>] = &[
    optimize_flatten_trees,

    optimize_single_member_union,
    optimize_two_members_union,
    optimize_single_type_union,
    optimize_single_member_struct,
    optimize_single_base_member_struct,

    optimize_equivalent_trees,
];

// // these run after the above 
// #[distributed_slice]
// pub static LATE_OPTIMIZERS: [fn(&mut TypeCompilerUnit) -> cu::Result<bool>];

/// Flatten nested type tree to its most pritimive types
// #[distributed_slice(LATE_OPTIMIZERS)]
fn optimize_flatten_trees(compiler: &mut TypeCompilerUnit, _: bool) -> cu::Result<bool> {
    fn get_flattened_tree_recur(
        tree: &Tree<Goff>,
        buckets: &BucketMap<Goff, TypeUnit>,
        depth: usize,
    ) -> cu::Result<Option<Tree<Goff>>> {
        // return Some if changed
        if depth > 1000 {
            cu::bail!("max flatten depth limit reached");
        }
        match tree {
            Tree::Base(inner) => {
                let inner_key = *inner;
                let inner_bucket = buckets.get_unwrap(inner_key)?;
                let inner_unit = &inner_bucket.value;
                if let Some(TypeUnitData::Tree(inner_tree)) = &inner_unit.data {
                    return Ok(Some(inner_tree.clone()));
                }
            }
            Tree::Ptr(inner) => {
                let inner_flatten = cu::check!(
get_flattened_tree_recur(inner, buckets, depth + 1),
                    "failed to flatten pointer-to- {inner:#?}"
                )?;
                if let Some(inner_flatten) = inner_flatten {
                    return Ok(Some(Tree::Ptr(Box::new(inner_flatten))));
                }
            }
            Tree::Array(inner, len) => {
                let len = *len;
                let inner_flatten = cu::check!(
                    get_flattened_tree_recur(inner, buckets, depth + 1),
                    "failed to flatten array-of- {inner:#?}"
                )?;
                if let Some(inner_flatten) = inner_flatten {
                    // single element optimization
                    if len == 1 {
                        return Ok(Some(inner_flatten));
                    }
                    return Ok(Some(Tree::Array(Box::new(inner_flatten), len)));
                }
                // single element optimization
                if len == 1 {
                    return Ok(Some(inner.as_ref().clone()));
                }
            }
            Tree::Sub(inners) => {
                let mut new_vec = Vec::with_capacity(inners.len());
                for (i, inner) in inners.iter().enumerate() {
                    let inner_flatten = cu::check!(
                        get_flattened_tree_recur(inner, buckets, depth + 1),
                        "failed to flatten subroutine part {i}: {inner:#?}"
                    )?;
                    if let Some(inner_flatten) = inner_flatten {
                        if new_vec.is_empty() {
                            for old in inners.iter().take(i) {
                                new_vec.push(old.clone());
                            }
                        }
                        new_vec.push(inner_flatten);
                    }
                }
                if !new_vec.is_empty() {
                    return Ok(Some(Tree::Sub(new_vec)));
                }
            }
            Tree::Ptmd(this_, inner) => {
                    let inner_flatten = cu::check!(
                        get_flattened_tree_recur(inner, buckets, depth + 1),
                        "failed to flatten ptmd: {inner:#?}"
                    )?;
                if let Some(inner_flatten) = inner_flatten {
                    return Ok(Some(Tree::Ptmd(this_.clone(), Box::new(inner_flatten))));
                }
            }
            Tree::Ptmf(this_, inners) => {
                let mut new_vec = Vec::with_capacity(inners.len());
                for (i, inner) in inners.iter().enumerate() {
                    let inner_flatten = cu::check!(
                        get_flattened_tree_recur(inner, buckets, depth + 1),
                        "failed to flatten ptmf part {i}: {inner:#?}"
                    )?;
                    if let Some(inner_flatten) = inner_flatten {
                        if new_vec.is_empty() {
                            for old in inners.iter().take(i) {
                                new_vec.push(old.clone());
                            }
                        }
                        new_vec.push(inner_flatten);
                    }
                }
                if !new_vec.is_empty() {
                    return Ok(Some(Tree::Ptmf(this_.clone(), new_vec)));
                }
            }
        }

        Ok(None)
    }
    let mut changed = false;
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Tree(tree)) = &unit.data else {
            continue;
        };
        let bucket_key = bucket.canonical_key();
        match tree {
            Tree::Base(inner) => {
                let inner_key = *inner;
                if inner_key == bucket_key {
                    cu::bail!("unexpected self-referencing tree: {bucket_key}");
                }
                compiler.merges.push((inner_key, bucket_key));
                changed = true;
            }
            _ => {
                let new_tree = cu::check!(
                    get_flattened_tree_recur(tree, &compiler.compiled, 0),
                    "failed to flatten tree at {bucket_key}"
                )?;
                if let Some(new_tree) = new_tree {
                    let new_data = TypeUnitData::Tree(new_tree);
                    compiler.changes.insert(bucket_key, new_data);
                    changed = true;
                }
            }
        }
    }
    Ok(changed)
}

// #[distributed_slice(OPTIMIZERS)]
fn optimize_equivalent_trees(compiler: &mut TypeCompilerUnit, is_linking: bool) -> cu::Result<bool> {
    const PARALLEL_MERGE_THRES: usize = 20000 * 20000;
    let mut changed = false;

    let mut datahash_map = BTreeMap::new();

    // let mut prims = BTreeSet::new();
    let mut enums = BTreeSet::new();
    let mut enum_decls = BTreeSet::new();
    let mut unions = BTreeSet::new();
    let mut union_decls = BTreeSet::new();
    let mut structs = BTreeSet::new();
    let mut struct_decls = BTreeSet::new();
    // let mut tree_arrays = BTreeSet::new();
    // let mut tree_ptrs = BTreeSet::new();
    // let mut tree_subs = BTreeSet::new();
    // let mut tree_ptmds = BTreeSet::new();
    // let mut tree_ptmfs = BTreeSet::new();
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(data) = &unit.data else {
            continue;
        };
        let bucket_key = bucket.canonical_key();
        let mut hasher = FxHasher64::default();
        use std::hash::{Hash, Hasher};
        data.hash(&mut hasher);
        let hash = hasher.finish();
        let merge_bin = datahash_map.entry(hash).or_insert_with(Vec::new);
        merge_bin.push(bucket_key);
        match data {
            // TypeUnitData::Prim(_) => prims.insert(bucket_key),
            TypeUnitData::Enum(_) => enums.insert(bucket_key),
            TypeUnitData::EnumDecl => enum_decls.insert(bucket_key),
            TypeUnitData::Union(_) => unions.insert(bucket_key),
            TypeUnitData::UnionDecl => union_decls.insert(bucket_key),
            TypeUnitData::Struct(_) => structs.insert(bucket_key),
            TypeUnitData::StructDecl => struct_decls.insert(bucket_key),
            _ => {false}
            // TypeUnitData::Tree(tree) => match tree {
            //     TyTree::Base(_) => false,
            //     TyTree::Array(_, _) => tree_arrays.insert(bucket_key),
            //     TyTree::Ptr(_) => tree_ptrs.insert(bucket_key),
            //     TyTree::Sub(_) => tree_subs.insert(bucket_key),
            //     TyTree::Ptmd(_, _) => tree_ptmds.insert(bucket_key),
            //     TyTree::Ptmf(_, _) => tree_ptmfs.insert(bucket_key),
            // },
        };
    }

    datahash_map.values_mut().for_each(|x| x.sort());

    fn compare_and_merge(compiler: &TypeCompilerUnit, set: &Vec<Goff>) -> Vec<(Goff, Goff)> {
        let iter = set.iter().filter_map(|j| {
            let j_data = compiler.compiled.get(*j).map(|x| x.value.data.as_ref()).flatten()?;
            Some(set.iter().filter_map(move |k| {
                if k <= j {
                    return None;
                }
                let k_data = compiler.compiled.get(*k).map(|x| x.value.data.as_ref()).flatten()?;
                Some((*j, *k, j_data, k_data))
            }))
        }).flatten();

        fn mapper((j, k, j_data, k_data): (Goff, Goff, &TypeUnitData, &TypeUnitData)) -> Option<(Goff, Goff)> {
            if j_data == k_data {
                Some((j, k))
            } else {
                None
            }
        }

            if set.len() * set.len() > PARALLEL_MERGE_THRES {
        iter.par_bridge().filter_map(mapper).collect()
        } else {
        iter.filter_map(mapper).collect()
        }
            
    }

    // fn compare_and_link(compiler: &TypeCompilerUnit, set: &BTreeSet<Goff>) -> cu::Result<Vec<(Goff, Goff)>> {
    //     let iter = set.iter().filter_map(|j| {
    //         let j_unit = &compiler.compiled.get(*j)?.value;
    //         Some(set.iter().filter_map(move |k| {
    //             if k <= j {
    //                 return None;
    //             }
    //             let k_unit = &compiler.compiled.get(*k)?.value;
    //             Some((*j, *k, j_unit, k_unit))
    //         }))
    //     }).flatten();
    //
    //     fn mapper((j, k, j_unit, k_unit): (Goff, Goff, &TypeUnit, &TypeUnit)) -> Option<cu::Result<(Goff, Goff)>> {
    //         match j_unit.can_link(k_unit) {
    //             Ok(true) => Some(Ok((j, k))),
    //             Ok(false) => None,
    //             Err(e) => {
    //                 Some(Err(e).context(format!("failed to link types {j} and {k}")))
    //             }
    //         }
    //     }
    //
    //         if set.len() * set.len() > PARALLEL_MERGE_THRES {
    //     iter.par_bridge().filter_map(mapper).collect()
    //     } else {
    //     iter.filter_map(mapper).collect()
    //     }
    //        
    // }

    fn compare_and_merge_decl(compiler: &TypeCompilerUnit, decl_set: &BTreeSet<Goff>, set: &BTreeSet<Goff>) -> Vec<(Goff, Goff)> {
        let iter = decl_set.iter().filter_map(|j| {
            let j_unit = &compiler.compiled.get(*j)?.value;
            let decl_iter = decl_set.iter().filter_map(move |k| {
                if k <= j {
                    return None;
                }
                let k_unit = &compiler.compiled.get(*k)?.value;
                Some((*j, *k, j_unit, k_unit))
            });
            let defn_iter = set.iter().filter_map(move |k| {
                let k_unit = &compiler.compiled.get(*k)?.value;
                Some((*j, *k, j_unit, k_unit))
            });
            Some(decl_iter.chain(defn_iter))
        }).flatten();
        fn mapper((j, k, j_unit, k_unit): (Goff, Goff, &TypeUnit, &TypeUnit)) -> Option<(Goff, Goff)> {
            if j_unit.names_overlap(k_unit) {
                return Some((j, k));
            }
            None
        }
        if decl_set.len() * (set.len().max(decl_set.len())) > PARALLEL_MERGE_THRES {
            iter.par_bridge()
                .filter_map(mapper).collect()
        } else {
            iter
                .filter_map(mapper).collect()
        }
    }

    cu::debug!(
        "structs: {}, enums: {}, unions: {}",
        structs.len(),
        enums.len(),
        unions.len()
    );
    cu::debug!(
        "struct_decls: {}, enum_decls: {}, unions_decl: {}",
        struct_decls.len(),
        enum_decls.len(),
        union_decls.len()
    );
    // if is_linking {
    //     compiler.merges.extend(compare_and_link(compiler, &prims)?);
    //     compiler.merges.extend(compare_and_link(compiler, &enums)?);
    //     compiler.merges.extend(compare_and_link(compiler, &unions)?);
    //     compiler.merges.extend(compare_and_link(compiler, &structs)?);
    //     compiler.merges.extend(compare_and_link(compiler, &tree_arrays)?);
    //     compiler.merges.extend(compare_and_link(compiler, &tree_ptrs)?);
    //     compiler.merges.extend(compare_and_link(compiler, &tree_subs)?);
    //     compiler.merges.extend(compare_and_link(compiler, &tree_ptmds)?);
    //     compiler.merges.extend(compare_and_link(compiler, &tree_ptmfs)?);
    // } else {
    if is_linking {
        let enums = enums.iter().copied().collect();
        compiler.merges.extend(compare_and_merge(compiler, &enums));
        let unions = unions.iter().copied().collect();
        compiler.merges.extend(compare_and_merge(compiler, &unions));
        let structs = structs.iter().copied().collect();
        compiler.merges.extend(compare_and_merge(compiler, &structs));
    }
    for merge_bin in datahash_map.values() {
        compiler.merges.extend(compare_and_merge(compiler, &merge_bin));
    }
        // compiler.merges.extend(compare_and_merge(compiler, &enums));
        // compiler.merges.extend(compare_and_merge(compiler, &unions));
        // compiler.merges.extend(compare_and_merge(compiler, &structs));
        // compiler.merges.extend(compare_and_merge(compiler, &tree_arrays));
        // compiler.merges.extend(compare_and_merge(compiler, &tree_ptrs));
        // compiler.merges.extend(compare_and_merge(compiler, &tree_subs));
        // compiler.merges.extend(compare_and_merge(compiler, &tree_ptmds));
        // compiler.merges.extend(compare_and_merge(compiler, &tree_ptmfs));
    // }

    // compare_and_merge!(prims);
    // compare_and_merge!(enums);
    // compare_and_merge!(unions);
    // compare_and_merge!(structs);
    // compare_and_merge!(tree_arrays);
    // compare_and_merge!(tree_ptrs);
    // compare_and_merge!(tree_subs);
    // compare_and_merge!(tree_ptmds);
    // compare_and_merge!(tree_ptmfs);

    compiler.merges.extend(compare_and_merge_decl(compiler, &enum_decls, &enums));
    compiler.merges.extend(compare_and_merge_decl(compiler, &union_decls, &unions));
    compiler.merges.extend(compare_and_merge_decl(compiler, &struct_decls, &structs));


    Ok(!compiler.merges.is_empty())
}

/// Flatten the union if it only has one member, and is non-recursive
// #[distributed_slice(OPTIMIZERS)]
fn optimize_single_member_union(compiler: &mut TypeCompilerUnit, _: bool) -> cu::Result<bool> {
    let mut changed = false;
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Union(data)) = &unit.data else {
            continue;
        };
        let bucket_key = bucket.canonical_key();
        // if the union only has one member, we can change it to a typedef
        if data.members.len() == 1 {
            let member_type = data.members[0].1;
            let member_type_unit = compiler.compiled.get_unwrap(member_type)?;
            if member_type != bucket_key {
                if let Some(member_data) = &member_type_unit.value.data {
                    if !member_data.is_recursive_to(bucket_key) {
                        compiler.merges.push((bucket_key, member_type));
                        changed = true;
                    }
                }
            }
        }
    }
    Ok(changed)
}

// collapse the union if all members are the same type
fn optimize_single_type_union(compiler: &mut TypeCompilerUnit, _: bool) -> cu::Result<bool> {
    let mut changed = false;
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Union(data)) = &unit.data else {
            continue;
        };
        let bucket_key = bucket.canonical_key();
        let mut found_type = None;
        for member in &data.members {
            let Some(ft) = found_type else {
                found_type = Some(member.1);
                continue;
            };
            if ft != member.1 {
                found_type = None;
                break;
            }
        }
        let Some(member_type) = found_type else {
            continue;
        };
        let member_type_unit = compiler.compiled.get_unwrap(member_type)?;
        if member_type != bucket_key {
            if let Some(member_data) = &member_type_unit.value.data {
                if !member_data.is_recursive_to(bucket_key) {
                    compiler.merges.push((bucket_key, member_type));
                    changed = true;
                }
            }
        }
    }
    Ok(changed)
}

/// If a union has 2 members of the same size, pick one
// #[distributed_slice(OPTIMIZERS)]
fn optimize_two_members_union(compiler: &mut TypeCompilerUnit, _: bool) -> cu::Result<bool> {
    let mut changed = false;
    let mut to_check = vec![];
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Union(data)) = &unit.data else {
            continue;
        };
        if data.members.len() != 2 {
            continue;
        }
        let bucket_key = bucket.canonical_key();
        let member_type = data.members[0].1;
        let member_anon = data.members[0].0.is_none();
        let member_type2 = data.members[1].1;
        let member_anon2 = data.members[1].0.is_none();
        to_check.push((bucket_key, member_type, member_type2, member_anon, member_anon2));
    }

    for (bucket_key, member_type, member_type2, member_anon, member_anon2) in to_check {
        let member_size = compiler.resolve_size(member_type)?;
        let member_size2 = compiler.resolve_size(member_type2)?;
        let mut pick_member = 0;
        if member_size != member_size2 {
            // pick the larger member
            if member_size > member_size2 {
                pick_member = 1;
            } else {
                pick_member = 2;
            }
        }
        // pick the anonymous member
        if pick_member == 0 {
            if member_anon {
                if !member_anon2 {
                    pick_member = 1;
                }
            } else if member_anon2 {
                pick_member = 2
            }
        }
        // pick the "more complex" member
        if pick_member == 0 {
            let complexity1 = compiler.resolve_complexity(member_type)?;
            let complexity2 = compiler.resolve_complexity(member_type2)?;
            if complexity1 > complexity2 {
                pick_member = 1;
            } else if complexity1 < complexity2 {
                pick_member = 2;
            } else {
                cu::debug!("same complexity: {member_type} and {member_type2}: {complexity1}")
            }
        }
        if pick_member == 0 {
            continue;
        }
        if pick_member == 1 {
            if member_type != bucket_key {
                let member_type_unit = compiler.compiled.get_unwrap(member_type)?;
                if let Some(member_data) = &member_type_unit.value.data {
                    if !member_data.is_recursive_to(bucket_key) {
                        compiler.merges.push((bucket_key, member_type));
                        changed = true;
                    }
                }
            }
            continue;
        }
        if member_type2 != bucket_key {
            let member_type_unit = compiler.compiled.get_unwrap(member_type2)?;
            if let Some(member_data) = &member_type_unit.value.data {
                if !member_data.is_recursive_to(bucket_key) {
                    compiler.merges.push((bucket_key, member_type2));
                    changed = true;
                }
            }
        }
    }
    Ok(changed)
}

/// Flatten the struct if it only has one member, and is non-recursive and non-virtual
// #[distributed_slice(OPTIMIZERS)]
fn optimize_single_member_struct(compiler: &mut TypeCompilerUnit, is_linking: bool) -> cu::Result<bool> {
    // if !is_linking {
    //     return Ok(false);
    // }
    let mut changed = false;
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Struct(data)) = &unit.data else {
            continue;
        };
        if !data.vtable.entries.is_empty() {
            continue;
        }
        if data.members.len() != 1 {
            continue;
        }
        let bucket_key = bucket.canonical_key();
        let member_type = data.members[0].ty;
        let member_type_unit = compiler.compiled.get_unwrap(member_type)?;
        if member_type != bucket_key {
            if let Some(member_data) = &member_type_unit.value.data {
                if !member_data.is_recursive_to(bucket_key) {
                    compiler.merges.push((bucket_key, member_type));
                    changed = true;
                }
            }
        }
    }
    Ok(changed)
}

/// Inline the base struct members, if the derived class only has one field
/// that is the base class
// #[distributed_slice(OPTIMIZERS)]
fn optimize_single_base_member_struct(compiler: &mut TypeCompilerUnit, is_linking: bool) -> cu::Result<bool> {
    // if !is_linking {
    //     return Ok(false);
    // }
    let mut changed = false;
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Struct(data)) = &unit.data else {
            continue;
        };
        if data.members.len() != 1 {
            continue;
        }
        let member = &data.members[0];
        if !member.is_base() {
            continue;
        }
        let bucket_key = bucket.canonical_key();
        let base_type = data.members[0].ty;
        let base_type_unit = compiler.compiled.get_unwrap(base_type)?;
        let Some(base_data) = &base_type_unit.value.data else {
            continue;
        };
        let members = if let TypeUnitData::Struct(base_data) = base_data {
            cu::ensure!(
                base_data.byte_size == data.byte_size,
                "unexpected single base member struct having different size than its base member"
            );
            base_data.members.clone()
        } else {
            continue;
        };
        let mut new_data = data.clone();
        new_data.members = members;
        compiler.changes.insert(bucket_key, TypeUnitData::Struct(new_data));
        changed = true;
    }
    Ok(changed)
}
