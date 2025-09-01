use std::collections::BTreeMap;
use std::collections::BTreeSet;

use cu::pre::*;
use fxhash::FxHashMap;
use fxhash::FxHasher64;
use tyyaml::{Prim, Tree};

use super::bucket::{GoffBuckets, MergeQueue};
use super::pre::*;
use super::type_structure::*;

pub fn compile_stage0(mut stage0: TypeStage0) -> cu::Result<TypeStageA> {
    cu::debug!("stage0: compiling {} types", stage0.types.len());
    // ensure primitive types exist
    for p in Prim::iter() {
        stage0.types.insert(Goff::prim(p), Type0::Prim(p));
    }
    let mut sizes = GoffMap::new();
    sizes.insert(Goff::pointer(), stage0.config.extract.pointer_size()?);
    sizes.insert(Goff::ptmd(), stage0.config.extract.ptmd_size()?);
    sizes.insert(Goff::ptmf(), stage0.config.extract.ptmf_size()?);

    // resolve enum sizes
    for (goff, type_data) in &stage0.types {
        let Type0::Enum(_, _) = &type_data else { continue };
        resolve_size_stage0(*goff, &stage0, &mut sizes).context("unable to resolve enum sizes")?;
    }

    let mut buckets = GoffBuckets::default();

    let mut merges = MergeQueue::default();
    let mut types = vec![];

    // convert type0 to type1
    for (goff, type_data) in stage0.types {
        match type_data {
            Type0::Prim(p) => {
                if !goff.is_prim() {
                    merges.push(Goff::prim(p), goff)?;
                }
            }
            Type0::Typedef(name, target) => {
                types.push((goff, TypeA::Typedef(name, target)));
            }
            Type0::Enum(name, data) => {
                let size = *cu::check!(
                    sizes.get(&goff),
                    "unexpected: did not resolve enum size for enum {goff}"
                )?;
                let t = TypeA::Enum(
                    name,
                    TypeAEnum {
                        byte_size: size,
                        enumerators: data.enumerators,
                    },
                );
                types.push((goff, t));
            }
            Type0::EnumDecl(name) => types.push((goff, TypeA::EnumDecl(name))),
            Type0::Union(name, data) => types.push((goff, TypeA::Union(name, data))),
            Type0::UnionDecl(name) => types.push((goff, TypeA::UnionDecl(name))),
            Type0::Struct(name, data) => types.push((goff, TypeA::Struct(name, data))),
            Type0::StructDecl(name) => types.push((goff, TypeA::StructDecl(name))),
            Type0::Tree(data) => {
                if let Tree::Base(inner) = &data {
                    merges.push(goff, *inner)?;
                } else {
                    types.push((goff, TypeA::Tree(data)));
                }
            }
            Type0::Alias(target) => {
                merges.push(goff, target)?;
            }
        }
    }
    for (k1, k2) in merges {
        buckets.merge(k1, k2)?;
    }

    let mut types = cu::check!(make_canonicalized(types, &buckets), "initial canonicalization failed")?;
    // ensure primitive types exist
    for p in Prim::iter() {
        types.insert(Goff::prim(p), TypeA::Prim(p));
    }
    let types = cu::check!(optimize(&mut buckets, types), "stage0 optimization failed")?;

    let stage1 = TypeStageA {
        offset: stage0.offset,
        name: stage0.name,
        // buckets,
        types,
    };

    cu::debug!("stage0: compiled {} types into stage1", stage1.types.len());

    Ok(stage1)
}

pub fn into_stage2(stage1: TypeStageA) -> TypeStage2 {
    TypeStage2 { types: stage1.types }
}

pub fn link_stage1(stage2: TypeStage2, stage1: TypeStageA) -> cu::Result<TypeStage2> {
    cu::debug!("stage1: linking {} types into stage2", stage1.types.len());
    let mut types = stage2.types;
    for (k, t) in stage1.types {
        use std::collections::btree_map::Entry;
        match types.entry(k) {
            Entry::Vacant(e) => {
                e.insert(t);
            }
            Entry::Occupied(e) => {
                let t_e = e.get();
                cu::ensure!(
                    t_e == &t,
                    "stage1 has different definition at {k}, it is {t:#?}, but previous is {t_e:#?}"
                );
            }
        }
    }
    // it's ok to construct new buckets for the next stage,
    // because it should all be canonicalized
    let mut buckets = GoffBuckets::default();
    let types = cu::check!(optimize(&mut buckets, types), "stage1 optimization failed")?;
    let stage2 = TypeStage2 { types };
    cu::debug!("stage2: linked {} types", stage2.types.len());
    Ok(stage2)
}

fn resolve_size_stage0(goff: Goff, stage0: &TypeStage0, sizes: &mut GoffMap<u32>) -> cu::Result<u32> {
    // using "0" as speical value for resolving - this is valid because sizeof must return non-zero
    const RESOLVING: u32 = 0;

    if let Some(x) = sizes.get(&goff) {
        if *x == RESOLVING {
            cu::bail!("failed to resolve size: infinite sized type {goff}");
        }
        return Ok(*x);
    }
    // mark the size as resolving
    let data = cu::check!(stage0.types.get(&goff), "unexpected unlinked type {goff}")?;
    let size = match data {
        Type0::Prim(prim) => {
            let size = prim.byte_size().unwrap_or(UNSIZED);
            sizes.insert(goff, size);
            size
        }
        Type0::Typedef(_, inner) => {
            sizes.insert(goff, RESOLVING);
            let inner = *inner;
            let size = cu::check!(
                resolve_size_stage0(inner, stage0, sizes),
                "failed to resolve size for typedef {goff} -> {inner}"
            )?;
            size
        }
        Type0::Alias(inner) => {
            sizes.insert(goff, RESOLVING);
            let inner = *inner;
            let size = cu::check!(
                resolve_size_stage0(inner, stage0, sizes),
                "failed to resolve size for alias {goff} -> {inner}"
            )?;
            size
        }
        Type0::Enum(_, data) => {
            let size = match data.byte_size_or_base {
                Ok(size) => size,
                Err(inner) => {
                    sizes.insert(goff, RESOLVING);
                    let size = cu::check!(
                        resolve_size_stage0(inner, stage0, sizes),
                        "failed to resolve size for enum base type {goff} -> {inner}"
                    )?;
                    size
                }
            };
            cu::ensure!(size != 0, "unexpected zero-sized enum: {goff}");
            cu::ensure!(size != UNSIZED, "unexpected unsized enum: {goff}");
            size
        }
        Type0::EnumDecl(_) => {
            cu::bail!("encountered declaration while resolving size: enum decl {goff}");
        }
        Type0::Union(_, data) => {
            // verify size is the same as largest member
            sizes.insert(goff, RESOLVING);
            let size = data.byte_size;
            let mut max_size = 0;
            for member in &data.members {
                let size = cu::check!(
                    resolve_size_stage0(member.ty, stage0, sizes),
                    "failed to resolve size for union member type {goff} -> {}",
                    member.ty
                )?;
                max_size = size.max(max_size);
            }
            cu::ensure!(
                max_size == size,
                "unexpected union size mismatch: largest member size is 0x{max_size:x}, but self size is 0x{size:x}"
            );
            cu::ensure!(size != 0, "unexpected zero-sized union: {goff}");
            cu::ensure!(size != UNSIZED, "unexpected unsized union: {goff}");
            size
        }
        Type0::UnionDecl(_) => {
            cu::bail!("encountered declaration while resolving size: union decl {goff}");
        }
        Type0::Struct(_, data) => {
            let size = data.byte_size;
            cu::ensure!(size != 0, "unexpected zero-sized struct: {goff}");
            cu::ensure!(size != UNSIZED, "unexpected unsized struct: {goff}");
            size
        }
        Type0::StructDecl(_) => {
            cu::bail!("encountered declaration while resolving size: struct decl {goff}");
        }
        Type0::Tree(ty_tree) => {
            fn resolve_tree(tree: &Tree<Goff>, stage0: &TypeStage0, sizes: &mut GoffMap<u32>) -> cu::Result<u32> {
                match tree {
                    Tree::Base(inner) => {
                        let inner = *inner;
                        cu::check!(
                            resolve_size_stage0(inner, stage0, sizes),
                            "failed to resolve size for type {inner}"
                        )
                    }
                    Tree::Array(elemty, len) => {
                        let len = *len;
                        cu::ensure!(len != 0, "unexpected 0-length array");
                        let elem_size = cu::check!(
                            resolve_tree(elemty, stage0, sizes),
                            "failed to resolve array element size"
                        )?;
                        cu::ensure!(elem_size != UNSIZED, "array element must be sized");
                        Ok(elem_size * (len as u32))
                    }
                    Tree::Ptr(_) => Ok(*sizes.get(&Goff::pointer()).unwrap()),
                    Tree::Sub(_) => Ok(UNSIZED),
                    Tree::Ptmd(_, _) => Ok(*sizes.get(&Goff::ptmd()).unwrap()),
                    Tree::Ptmf(_, _) => Ok(*sizes.get(&Goff::ptmf()).unwrap()),
                }
            }
            sizes.insert(goff, RESOLVING);
            let tree = ty_tree.clone();
            let size = cu::check!(
                resolve_tree(&tree, stage0, sizes),
                "failed to resolve size for type tree: {goff}"
            )?;
            size
        }
    };

    // insert the actual size
    cu::ensure!(size != RESOLVING, "unexpected invalid size for type {goff}");
    sizes.insert(goff, size);
    Ok(size)
}

fn optimize(buckets: &mut GoffBuckets, types: BTreeMap<Goff, TypeA>) -> cu::Result<BTreeMap<Goff, TypeA>> {
    let mut types = types;
    loop {
        let mut merges = MergeQueue::default();
        cu::check!(dedupe_by_hash(&types, &mut merges), "failed to dedupe by hash")?;
        if !merges.is_empty() {
            types = cu::check!(
                process_merges(buckets, merges, types),
                "type merge failed after deduping by hash"
            )?;
            continue;
        }
        cu::check!(link_by_decl(&types, &mut merges), "failed to link by decl")?;
        if !merges.is_empty() {
            types = cu::check!(
                process_merges(buckets, merges, types),
                "type merge failed after linking by decl"
            )?;
            continue;
        }
        cu::check!(link_by_name(&types, &mut merges), "failed to link by name")?;
        if !merges.is_empty() {
            types = cu::check!(
                process_merges(buckets, merges, types),
                "type merge failed after linking by name"
            )?;
            continue;
        }
        break;
    }
    Ok(types)
}

fn make_canonicalized<I: IntoIterator<Item = (Goff, TypeA)>>(
    iter: I,
    buckets: &GoffBuckets,
) -> cu::Result<BTreeMap<Goff, TypeA>> {
    make_canonicalized_inner(iter.into_iter().collect(), buckets)
}

fn make_canonicalized_inner(mut input: GoffMap<TypeA>, buckets: &GoffBuckets) -> cu::Result<GoffMap<TypeA>> {
    loop {
        let old = input.clone();
        let mut map = BTreeMap::default();
        for (k, mut type1) in input {
            use std::collections::btree_map::Entry;

            let k_primary = buckets.primary_fallback(k);
            cu::check!(
                type1.map_goff(|x| Ok(buckets.primary_fallback(x))),
                "canonicalization of {k} (canonicalized to {k_primary}) failed"
            )?;
            match map.entry(k_primary) {
                Entry::Vacant(e) => {
                    e.insert(type1);
                }
                Entry::Occupied(mut e) => {
                    cu::check!(
                        do_merge_type1(&mut type1, e.get_mut()),
                        "do_merge_type1 failed: {k} to {k_primary} failed"
                    )?;
                }
            }
        }
        if map != old {
            input = map;
            continue;
        }
        return Ok(map);
    }
}

fn dedupe_by_hash(map: &BTreeMap<Goff, TypeA>, merges: &mut MergeQueue) -> cu::Result<()> {
    let mut hashes = BTreeMap::default();
    for (goff, type1) in map {
        use std::hash::{Hash, Hasher};
        let mut hasher = FxHasher64::default();
        type1.hash(&mut hasher);
        let hash = hasher.finish();
        let entry = hashes.entry(hash).or_insert_with(Vec::new);
        entry.push((*goff, type1));
    }

    for keys in hashes.into_values() {
        for (i, (k1, t1)) in keys.iter().enumerate() {
            for (k2, t2) in keys.iter().skip(i + 1) {
                if t1 == t2 {
                    merges.push(*k1, *k2)?;
                }
            }
        }
    }
    Ok(())
}

fn link_by_decl(map: &BTreeMap<Goff, TypeA>, merges: &mut MergeQueue) -> cu::Result<()> {
    let mut enum_by_name = FxHashMap::default();
    let mut enum_decl_by_name = FxHashMap::default();
    let mut union_by_name = FxHashMap::default();
    let mut union_decl_by_name = FxHashMap::default();
    let mut struct_by_name = FxHashMap::default();
    let mut struct_decl_by_name = FxHashMap::default();
    for (goff, type1) in map {
        match type1 {
            TypeA::Enum(Some(name), _) => {
                let entry = enum_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push(*goff);
            }
            TypeA::EnumDecl(name) => {
                let entry = enum_decl_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push(*goff);
            }
            TypeA::Union(Some(name), _) => {
                let entry = union_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push(*goff);
            }
            TypeA::UnionDecl(name) => {
                let entry = union_decl_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push(*goff);
            }
            TypeA::Struct(Some(name), _) => {
                let entry = struct_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push(*goff);
            }
            TypeA::StructDecl(name) => {
                let entry = struct_decl_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push(*goff);
            }
            _ => {}
        }
    }

    for (name, enums) in enum_by_name {
        let Some(enum_decls) = enum_decl_by_name.get(&name) else {
            continue;
        };
        for k1 in enums {
            for k2 in enum_decls {
                merges.push(k1, *k2)?;
            }
        }
    }
    for (name, unions) in union_by_name {
        let Some(union_decls) = union_decl_by_name.get(&name) else {
            continue;
        };
        for k1 in unions {
            for k2 in union_decls {
                merges.push(k1, *k2)?;
            }
        }
    }
    for (name, structs) in struct_by_name {
        let Some(struct_decls) = struct_decl_by_name.get(&name) else {
            continue;
        };
        for k1 in structs {
            for k2 in struct_decls {
                merges.push(k1, *k2)?;
            }
        }
    }
    Ok(())
}

fn link_by_name(map: &BTreeMap<Goff, TypeA>, merges: &mut MergeQueue) -> cu::Result<()> {
    let mut union_by_name = FxHashMap::default();
    let mut struct_by_name = FxHashMap::default();
    for (goff, type1) in map {
        match type1 {
            TypeA::Union(Some(name), data) => {
                let entry = union_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push((*goff, data));
            }
            TypeA::Struct(Some(name), data) => {
                let entry = struct_by_name.entry(name.to_string()).or_insert_with(Vec::new);
                entry.push((*goff, data));
            }
            _ => {}
        }
    }

    // unions and structs with the same name must be merged, otherwise it's an error
    for (name, unions) in union_by_name {
        for (i, (k1, t1)) in unions.iter().enumerate() {
            for (k2, t2) in unions.iter().skip(i + 1) {
                cu::check!(
                    t1.merge_checked(t2, merges),
                    "failed to merge union by name {name:?}: {k1} and {k2}"
                )?;
            }
        }
    }
    for (name, structs) in struct_by_name {
        for (i, (k1, t1)) in structs.iter().enumerate() {
            for (k2, t2) in structs.iter().skip(i + 1) {
                cu::check!(
                    t1.merge_checked(t2, merges),
                    "failed to merge struct by name {name:?}: {k1} and {k2}"
                )?;
            }
        }
    }

    Ok(())
}

fn process_merges(
    buckets: &mut GoffBuckets,
    merges: MergeQueue,
    map: BTreeMap<Goff, TypeA>,
) -> cu::Result<BTreeMap<Goff, TypeA>> {
    fn check_merge(
        k1: Goff,
        k2: Goff,
        map: &BTreeMap<Goff, TypeA>,
        checked: &mut BTreeSet<(Goff, Goff)>,
        new_mq: &mut MergeQueue,
    ) -> cu::Result<()> {
        if !checked.insert((k1, k2)) {
            return Ok(());
        }
        let t1 = cu::check!(map.get(&k1), "failed to get type {k1}")?;
        let t2 = cu::check!(map.get(&k2), "failed to get type {k2}")?;
        match (t1, t2) {
            (TypeA::Prim(a), TypeA::Prim(b)) => {
                cu::ensure!(
                    a == b,
                    "two different primitive types cannot be merged: {k1}={a} and {k2}={b}"
                );
                Ok(())
            }
            (TypeA::Typedef(a_name, a), TypeA::Typedef(b_name, b)) => {
                cu::ensure!(
                    a_name == b_name,
                    "two typedefs of different names cannot be merged:\n- {k1}={a_name:?}\n- {k2}={b_name:?}"
                );
                cu::check!(
                    check_merge(*a, *b, map, checked, new_mq),
                    "cannot merge typedefs {k1} and {k2}, name is {a_name:?}"
                )
            }
            (TypeA::Enum(a_name, a), TypeA::Enum(b_name, b)) => {
                cu::ensure!(
                    a_name == b_name,
                    "two enums of different names cannot be merged:\n- {k1}={a_name:?}\n- {k2}={b_name:?}"
                );
                cu::ensure!(
                    a == b,
                    "two enums of different data cannot be merged: names are {k1}={a_name:?} and {k2}={b_name:?}"
                );
                Ok(())
            }
            (TypeA::Enum(name1, _), TypeA::EnumDecl(name2)) | (TypeA::EnumDecl(name2), TypeA::Enum(name1, _)) => {
                cu::ensure!(
                    name1.as_deref() == Some(name2.as_str()),
                    "cannot merge enum definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
                );
                Ok(())
            }
            (TypeA::EnumDecl(a_name), TypeA::EnumDecl(b_name)) => {
                cu::ensure!(
                    a_name == b_name,
                    "two enum decls of different names cannot be merged: {k1}={a_name:?} and {k2}={b_name:?}"
                );
                Ok(())
            }
            (TypeA::Union(a_name, a), TypeA::Union(b_name, b)) => {
                cu::ensure!(
                    a_name == b_name,
                    "two unions of different names cannot be merged:\n- {k1}={a_name:?}\n- {k2}={b_name:?}"
                );
                cu::check!(
                    a.merge_checked(b, new_mq),
                    "cannot merge unions {k1} and {k2}, name is {a_name:?}"
                )
            }
            (TypeA::Union(name1, _), TypeA::UnionDecl(name2)) | (TypeA::UnionDecl(name2), TypeA::Union(name1, _)) => {
                cu::ensure!(
                    name1.as_deref() == Some(name2.as_str()),
                    "cannot merge union definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
                );
                Ok(())
            }
            (TypeA::UnionDecl(a_name), TypeA::UnionDecl(b_name)) => {
                cu::ensure!(
                    a_name == b_name,
                    "two union decls of different names cannot be merged: {k1}={a_name:?} and {k2}={b_name:?}"
                );
                Ok(())
            }
            (TypeA::Struct(a_name, a), TypeA::Struct(b_name, b)) => {
                cu::ensure!(
                    a_name == b_name,
                    "two structs of different names cannot be merged:\n- {k1}={a_name:?}\n- {k2}={b_name:?}"
                );
                cu::check!(
                    a.merge_checked(b, new_mq),
                    "cannot merge structs {k1} and {k2}, name is {a_name:?}"
                )
            }
            (TypeA::Struct(name1, _), TypeA::StructDecl(name2))
            | (TypeA::StructDecl(name2), TypeA::Struct(name1, _)) => {
                cu::ensure!(
                    name1.as_deref() == Some(name2.as_str()),
                    "cannot merge struct definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
                );
                Ok(())
            }
            (TypeA::StructDecl(a_name), TypeA::StructDecl(b_name)) => {
                cu::ensure!(
                    a_name == b_name,
                    "two struct decls of different names cannot be merged: {k1}={a_name:?} and {k2}={b_name:?}"
                );
                Ok(())
            }
            (TypeA::Tree(a), TypeA::Tree(b)) => {
                cu::check!(
                    tree_merge_checked(a, b, new_mq),
                    "cannot merge type trees {k1} and {k2}"
                )
            }
            _ => {
                cu::bail!("cannot merge types of different shape: {t1:?}, and {t2:?}")
            }
        }
    }

    let mut check_mq = merges.clone();
    let mut checked = BTreeSet::new();
    while let Some((k1, k2)) = check_mq.pop() {
        check_merge(k1, k2, &map, &mut checked, &mut check_mq)?;
    }

    for (k1, k2) in merges {
        buckets.merge(k1, k2)?;
    }

    make_canonicalized(map, buckets)
}

fn do_merge_type1(mut from: &mut TypeA, mut to: &mut TypeA) -> cu::Result<()> {
    match (&mut from, &mut to) {
        (TypeA::Prim(f), TypeA::Prim(t)) => {
            cu::ensure!(f == t, "two different primitive types cannot be merged: {f} and {t}");
        }
        (TypeA::Typedef(name1, f), TypeA::Typedef(name2, t)) => {
            cu::ensure!(
                name1 == name2,
                "two typedefs of different names cannot be merged: {name1:?} and {name2:?}"
            );
            let (k, _) = super::bucket::pick_bucket_primary_key(*f, *t)?;
            *t = k;
        }
        (TypeA::Enum(name1, f), TypeA::Enum(name2, t)) => {
            cu::ensure!(
                name1 == name2,
                "two enums of different names cannot be merged: {name1:?} and {name2:?}"
            );
            cu::check!(t.merge_from(f), "failed to merge enum definitions")?;
        }
        (TypeA::Enum(name1, _), TypeA::EnumDecl(name2)) => {
            cu::ensure!(
                name1.as_deref() == Some(name2.as_str()),
                "cannot merge enum definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
            );
            *to = from.clone();
        }
        (TypeA::EnumDecl(name2), TypeA::Enum(name1, _)) => {
            cu::ensure!(
                name1.as_deref() == Some(name2.as_str()),
                "cannot merge enum definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
            );
        }
        (TypeA::EnumDecl(name1), TypeA::EnumDecl(name2)) => {
            cu::ensure!(
                name1 == name2,
                "two enum decls of different names cannot be merged: {name1:?} and {name2:?}"
            );
        }
        (TypeA::Union(name1, f), TypeA::Union(name2, t)) => {
            cu::ensure!(
                name1 == name2,
                "two unions of different names cannot be merged: {name1:?} and {name2:?}"
            );
            cu::check!(t.merge_from(f), "failed to merge union definitions")?;
        }
        (TypeA::Union(name1, _), TypeA::UnionDecl(name2)) => {
            cu::ensure!(
                name1.as_deref() == Some(name2.as_str()),
                "cannot merge union definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
            );
            *to = from.clone();
        }
        (TypeA::UnionDecl(name2), TypeA::Union(name1, _)) => {
            cu::ensure!(
                name1.as_deref() == Some(name2.as_str()),
                "cannot merge union definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
            );
        }
        (TypeA::UnionDecl(name1), TypeA::UnionDecl(name2)) => {
            cu::ensure!(
                name1 == name2,
                "two union decls of different names cannot be merged: {name1:?} and {name2:?}"
            );
        }
        (TypeA::Struct(name1, f), TypeA::Struct(name2, t)) => {
            cu::ensure!(
                name1 == name2,
                "two structs of different names cannot be merged: {name1:?} and {name2:?}"
            );
            cu::check!(t.merge_from(f), "failed to merge structs definitions")?;
        }
        (TypeA::Struct(name1, _), TypeA::StructDecl(name2)) => {
            cu::ensure!(
                name1.as_deref() == Some(name2.as_str()),
                "cannot merge struct definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
            );
            *to = from.clone();
        }
        (TypeA::StructDecl(name2), TypeA::Struct(name1, _)) => {
            cu::ensure!(
                name1.as_deref() == Some(name2.as_str()),
                "cannot merge struct definition and declaration of different names: defn_name={name1:?}, decl_name={name2:?}"
            );
        }
        (TypeA::StructDecl(name1), TypeA::StructDecl(name2)) => {
            cu::ensure!(
                name1 == name2,
                "two struct decls of different names cannot be merged: {name1:?} and {name2:?}"
            );
        }
        (TypeA::Tree(f), TypeA::Tree(t)) => {
            cu::check!(
                tree_merge_checked(f, t, &mut Default::default()),
                "cannot merge type trees"
            )?;
        }
        _ => {
            cu::bail!("cannot merge types of different shapes: {from:#?}, and {to:#?}");
        }
    }
    Ok(())
}
