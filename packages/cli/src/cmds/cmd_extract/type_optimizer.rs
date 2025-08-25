use std::cell::Cell;
use std::collections::BTreeSet;

use cu::pre::*;
use linkme::distributed_slice;
use tyyaml::TyTree;

use super::BucketMap;
use super::pre::*;
use super::type_compiler::{TypeUnit, TypeUnitCompiler, TypeUnitData};

#[distributed_slice]
pub static OPTIMIZERS: [fn(&mut TypeUnitCompiler) -> cu::Result<bool>];

/// Flatten nested type tree to its most pritimive types
#[distributed_slice(OPTIMIZERS)]
fn optimize_flatten_trees(compiler: &mut TypeUnitCompiler) -> cu::Result<bool> {
    fn get_flattened_tree_recur(
        tree: &TyTree<Cell<Goff>>,
        buckets: &BucketMap<Goff, TypeUnit>,
        depth: usize,
    ) -> cu::Result<Option<TyTree<Cell<Goff>>>> {
        // return Some if changed
        if depth > 1000 {
            cu::bail!("max flatten depth limit reached");
        }
        match tree {
            TyTree::Base(inner) => {
                let inner_key = inner.get();
                let inner_bucket = buckets.get_unwrap(inner_key)?;
                let inner_unit = &inner_bucket.value;
                if let Some(TypeUnitData::Tree(inner_tree)) = &inner_unit.data {
                    return Ok(Some(inner_tree.clone()));
                }
            }
            TyTree::Ptr(inner) => {
                if let Some(inner_flatten) = get_flattened_tree_recur(inner, buckets, depth + 1)? {
                    return Ok(Some(TyTree::Ptr(Box::new(inner_flatten))));
                }
            }
            TyTree::Array(inner, len) => {
                if let Some(inner_flatten) = get_flattened_tree_recur(inner, buckets, depth + 1)? {
                    return Ok(Some(TyTree::Array(Box::new(inner_flatten), *len)));
                }
            }
            TyTree::Sub(inners) => {
                let mut new_vec = Vec::with_capacity(inners.len());
                for (i, inner) in inners.iter().enumerate() {
                    if let Some(inner_flatten) = get_flattened_tree_recur(inner, buckets, depth + 1)? {
                        if new_vec.is_empty() {
                            for old in inners.iter().take(i) {
                                new_vec.push(old.clone());
                            }
                        }
                        new_vec.push(inner_flatten);
                    }
                }
                if !new_vec.is_empty() {
                    return Ok(Some(TyTree::Sub(new_vec)));
                }
            }
            TyTree::Ptmd(this_, inner) => {
                if let Some(inner_flatten) = get_flattened_tree_recur(inner, buckets, depth + 1)? {
                    return Ok(Some(TyTree::Ptmd(this_.clone(), Box::new(inner_flatten))));
                }
            }
            TyTree::Ptmf(this_, inners) => {
                let mut new_vec = Vec::with_capacity(inners.len());
                for (i, inner) in inners.iter().enumerate() {
                    if let Some(inner_flatten) = get_flattened_tree_recur(inner, buckets, depth + 1)? {
                        if new_vec.is_empty() {
                            for old in inners.iter().take(i) {
                                new_vec.push(old.clone());
                            }
                        }
                        new_vec.push(inner_flatten);
                    }
                }
                if !new_vec.is_empty() {
                    return Ok(Some(TyTree::Ptmf(this_.clone(), new_vec)));
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
            TyTree::Base(inner) => {
                let inner_key = inner.get();
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

#[distributed_slice(OPTIMIZERS)]
fn optimize_equivalent_trees(compiler: &mut TypeUnitCompiler) -> cu::Result<bool> {
    let mut changed = false;

    let mut prims = BTreeSet::new();
    let mut enums = BTreeSet::new();
    let mut enum_decls = BTreeSet::new();
    let mut unions = BTreeSet::new();
    let mut union_decls = BTreeSet::new();
    let mut structs = BTreeSet::new();
    let mut struct_decls = BTreeSet::new();
    let mut tree_arrays = BTreeSet::new();
    let mut tree_ptrs = BTreeSet::new();
    let mut tree_subs = BTreeSet::new();
    let mut tree_ptmds = BTreeSet::new();
    let mut tree_ptmfs = BTreeSet::new();
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(data) = &unit.data else {
            continue;
        };
        let bucket_key = bucket.canonical_key();
        match data {
            TypeUnitData::Prim(_) => prims.insert(bucket_key),
            TypeUnitData::Enum(_) => enums.insert(bucket_key),
            TypeUnitData::EnumDecl => enum_decls.insert(bucket_key),
            TypeUnitData::Union(_) => unions.insert(bucket_key),
            TypeUnitData::UnionDecl => union_decls.insert(bucket_key),
            TypeUnitData::Struct(_) => structs.insert(bucket_key),
            TypeUnitData::StructDecl => struct_decls.insert(bucket_key),
            TypeUnitData::Tree(tree) => match tree {
                TyTree::Base(_) => false,
                TyTree::Array(_, _) => tree_arrays.insert(bucket_key),
                TyTree::Ptr(_) => tree_ptrs.insert(bucket_key),
                TyTree::Sub(_) => tree_subs.insert(bucket_key),
                TyTree::Ptmd(_, _) => tree_ptmds.insert(bucket_key),
                TyTree::Ptmf(_, _) => tree_ptmfs.insert(bucket_key),
            },
        };
    }

    macro_rules! compare_and_merge {
        ($set:ident) => {
            for j in &$set {
                let Some(j_data) = compiler.compiled.get(*j).map(|x| x.value.data.as_ref()).flatten() else {
                    continue;
                };
                for k in &$set {
                    if k <= j {
                        continue;
                    }
                    let Some(k_data) = compiler.compiled.get(*k).map(|x| x.value.data.as_ref()).flatten() else {
                        continue;
                    };
                    if j_data == k_data {
                        compiler.merges.push((*k, *j));
                        changed = true;
                    }
                }
            }
        };
    }

    cu::debug!(
        "structs: {}, enums: {}, unions: {}",
        structs.len(),
        enums.len(),
        unions.len()
    );

    compare_and_merge!(prims);
    compare_and_merge!(enums);
    compare_and_merge!(unions);
    compare_and_merge!(structs);
    compare_and_merge!(tree_arrays);
    compare_and_merge!(tree_ptrs);
    compare_and_merge!(tree_subs);
    compare_and_merge!(tree_ptmds);
    compare_and_merge!(tree_ptmfs);

    macro_rules! compare_and_merge_decl {
        ($set:ident) => {
            for j in &$set {
                let Some(j_names) = compiler.compiled.get(*j).map(|x| &x.value.declared_names) else {
                    continue;
                };
                for k in &$set {
                    if k <= j {
                        continue;
                    }
                    let Some(k_names) = compiler.compiled.get(*k).map(|x| &x.value.declared_names) else {
                        continue;
                    };
                    if j_names.intersection(k_names).next().is_some() {
                        compiler.merges.push((*k, *j));
                        changed = true;
                    }
                }
            }
        };
    }

    compare_and_merge_decl!(enum_decls);
    compare_and_merge_decl!(union_decls);
    compare_and_merge_decl!(struct_decls);

    Ok(changed)
}

/// Flatten the union if it only has one member, and is non-recursive
#[distributed_slice(OPTIMIZERS)]
fn optimize_single_member_union(compiler: &mut TypeUnitCompiler) -> cu::Result<bool> {
    let mut changed = false;
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Union(data)) = &unit.data else {
            continue;
        };
        let bucket_key = bucket.canonical_key();
        // if the union only has one member, we can change it to a typedef
        if data.members.len() == 1 {
            let member_type = data.members[0].1.get();
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

/// If a union has 2 members of the same size, pick one
#[distributed_slice(OPTIMIZERS)]
fn optimize_two_members_union(compiler: &mut TypeUnitCompiler) -> cu::Result<bool> {
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
        let member_type = data.members[0].1.get();
        let member_anon = data.members[0].0.is_none();
        let member_type2 = data.members[1].1.get();
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
#[distributed_slice(OPTIMIZERS)]
fn optimize_single_member_struct(compiler: &mut TypeUnitCompiler) -> cu::Result<bool> {
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
        let member_type = data.members[0].ty.get();
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
#[distributed_slice(OPTIMIZERS)]
fn optimize_single_base_member_struct(compiler: &mut TypeUnitCompiler) -> cu::Result<bool> {
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
        let base_type = data.members[0].ty.get();
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
