use std::collections::{BTreeMap, BTreeSet};

use cu::pre::*;

use super::super::bucket::GoffBuckets;
use super::super::deduper;
use super::pre::*;

/// Optimize (simplify) type layouts
pub fn optimize_layout(stage: &mut Stage1) -> cu::Result<()> {
}

pub static OPTIMIZERS: &[fn(&mut TypeCompilerUnit, bool) -> cu::Result<bool>] = &[

    optimize_single_member_union,
    optimize_two_members_union,
    optimize_single_type_union,
    optimize_single_member_struct,
    optimize_single_base_member_struct,

];

// // these run after the above 
// #[distributed_slice]
// pub static LATE_OPTIMIZERS: [fn(&mut TypeCompilerUnit) -> cu::Result<bool>];

#[derive(Default)]
struct OptimizeOutput {
    merges: Vec<GoffPair>,
    changes: GoffMap<Type1>
}

/// Eliminate unions with fewer than 2 members
fn optimize_little_member_union(stage: &Stage1) -> cu::Result<OptimizeOutput> {
    let mut output = OptimizeOutput::default();
    for (k, t) in &stage.types {
        let Type1::Union(name, data, other_names) = t else {
            continue;
        }
        match data.members.len() {
            0 => {
                // empty union is the same as an empty struct - a ZST (zero sized type, which has a
                // sizeof() of 1
                cu::ensure!(data.byte_size == 1, "expect empty union to be a ZST, but its size is {}", data.byte_size);
                let new_data = Type0Struct { 
                    template_args: data.template_args.clone(), 
                    byte_size: 1, 
                    vtable: vec![], 
                    members: vec![]
                };
                output.changes.insert(*k, Type1::Struct(name.clone(), new_data, other_names.clone()));
            }
            1 => {
                // a union with a single member can be eliminated to that member
                // unless the member contains the union type recursively
                if t.is_layout_directly_recursive(*k) {
                    continue;
                }
                
            }
        }
    }
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
