use std::sync::Arc;

use cu::pre::*;
use tyyaml::{Prim, Tree};

use crate::config::Config;

use super::pre::*;
use super::type_structure::*;

pub fn load_types(unit: &Unit, config: Arc<Config>, namespaces: GoffMap<Namespace>) -> cu::Result<TypeStage0> {
    let pointer_type = config.extract.pointer_type()?;
    let mut ctx = LoadTypeCtx {
        pointer_type,
        config,
        types: Default::default(),
        namespaces,
    };
    cu::debug!("loading types for {unit}");
    cu::check!(load_types_root(unit, &mut ctx), "failed to load types for {unit}")?;
    cu::debug!("loaded {} types from {unit}", ctx.types.len());

    Ok(TypeStage0 {
        offset: unit.offset.into(),
        name: unit.name.to_string(),
        types: ctx.types,
        config: ctx.config,
    })
}
fn load_types_root(unit: &Unit, ctx: &mut LoadTypeCtx) -> cu::Result<()> {
    let mut tree = unit.tree()?;
    let root = tree.root()?;
    load_types_recur(root, ctx)?;
    Ok(())
}
fn load_types_recur(mut node: DieNode<'_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<()> {
    let entry = node.entry();
    let tag = entry.tag();
    if is_type_tag(tag) {
        node = load_type_at(node, ctx)?;
    }

    node.for_each_child(|child| load_types_recur(child, ctx))
}

/// Load the type at the node. The node must be a type
fn load_type_at<'a, 'b>(node: DieNode<'a, 'b>, ctx: &mut LoadTypeCtx) -> cu::Result<DieNode<'a, 'b>> {
    let entry = node.entry();
    let offset = entry.goff();

    if ctx.types.contains_key(&offset) {
        cu::bail!("unexpected already visited type entry at {offset}");
    }

    let ty = match entry.tag() {
        DW_TAG_unspecified_type => {
            let name = entry.name().context("unspecified type entry must have a name")?;
            match name {
                // std::nullptr_t
                "decltype(nullptr)" => Type0::Prim(ctx.pointer_type),
                _ => {
                    cu::bail!("unknown name for unspecified type: {name}, for entry at {offset}");
                }
            }
        }
        DW_TAG_typedef => {
            match cu::check!(entry.loff_opt(DW_AT_type), "failed to read typedef at {offset}")? {
                // void
                None => Type0::Prim(Prim::Void),
                Some(loff) => {
                    let typedef_name = cu::check!(
                        entry.namespaced_name(&ctx.namespaces),
                        "failed to read name of the typedef at {offset}"
                    )?;
                    Type0::Typedef(typedef_name, entry.to_global(loff))
                }
            }
        }
        // T* or T&
        DW_TAG_pointer_type | DW_TAG_reference_type => {
            let pointee = cu::check!(entry.loff_opt(DW_AT_type), "failed to read pointee type at {offset}")?;
            match pointee {
                None => make_ptr(Goff::prim(Prim::Void)),
                Some(loff) => make_ptr(entry.to_global(loff)),
            }
        }
        // modifiers that don't do anything..
        DW_TAG_const_type | DW_TAG_volatile_type | DW_TAG_restrict_type => {
            match cu::check!(entry.loff_opt(DW_AT_type), "failed to read alias type at {offset}")? {
                None => Type0::Prim(Prim::Void),
                Some(loff) => Type0::Alias(entry.to_global(loff)),
            }
        }
        // T[n]
        DW_TAG_array_type => {
            let loff = cu::check!(
                entry.loff_opt(DW_AT_type),
                "failed to read array element type at {offset}"
            )?;
            let loff = cu::check!(loff, "entry {offset} has void[] type, which is not allowed")?;
            let array_len = cu::check!(
                load_array_subrange_count(&entry),
                "failed to get array length for array type at {offset}"
            )?;
            let goff = entry.to_global(loff);
            match array_len {
                // without count, just use ptr type
                None => make_ptr(goff),
                Some(len) => Type0::Tree(Tree::Array(Box::new(Tree::Base(goff)), len)),
            }
        }
        // Subroutine
        DW_TAG_subroutine_type => {
            let subroutine_types = cu::check!(
                load_subroutine_types_from_entry(&entry, false),
                "failed to read subroutine type at {offset}"
            )?;
            Type0::Tree(Tree::Sub(subroutine_types))
        }
        // PTMD/PTMF
        DW_TAG_ptr_to_member_type => {
            let this_ty_loff = cu::check!(
                entry.loff(DW_AT_containing_type),
                "failed to read this type for pointer-to-member type at {offset}"
            )?;
            let this_ty_goff = entry.to_global(this_ty_loff);
            let pointee_ty_loff = cu::check!(
                entry.loff_opt(DW_AT_type),
                "failed to read pointee type for pointer-to-member type at {offset}"
            )?;

            if let Some(pointee_ty_loff) = pointee_ty_loff {
                let pointee_entry = cu::check!(
                    entry.unit().entry_at(pointee_ty_loff),
                    "failed to read pointee type entry for pointer-to-member type at {offset}"
                )?;
                if pointee_entry.tag() == DW_TAG_subroutine_type {
                    // PTMF
                    let subroutine_types = cu::check!(
                        load_subroutine_types_from_entry(&pointee_entry, false),
                        "failed to read pointee subroutine type for pointer-to-member-function type at {offset}"
                    )?;
                    Type0::Tree(Tree::ptmf(this_ty_goff, subroutine_types))
                } else {
                    // PTMD
                    let pointee_ty_goff = entry.to_global(pointee_ty_loff);
                    Type0::Tree(Tree::ptmd(this_ty_goff, Tree::Base(pointee_ty_goff)))
                }
            } else {
                // PTMD to void
                Type0::Tree(Tree::ptmd(this_ty_goff, Tree::Base(Goff::prim(Prim::Void))))
            }
        }
        DW_TAG_base_type => Type0::Prim(entry.prim_type()?),
        DW_TAG_enumeration_type => load_enum_type_from_entry(&entry, ctx)?,
        DW_TAG_union_type => load_union_type_from_entry(&entry, ctx)?,
        DW_TAG_structure_type | DW_TAG_class_type => load_struct_type_from_entry(&entry, ctx)?,
        tag => cu::bail!("unexpected tag {tag} while processing type at {offset}"),
    };
    ctx.types.insert(offset, ty);
    Ok(node)
}

fn load_subroutine_types_from_entry(entry: &Die<'_, '_>, allow_other_tags: bool) -> cu::Result<Vec<Tree<Goff>>> {
    let offset = entry.goff();
    let rettype_loff = cu::check!(
        entry.loff_opt(DW_AT_type),
        "failed to read return type for subroutine-like type at {offset}"
    )?;
    let retty = match rettype_loff {
        None => Tree::Base(Goff::prim(Prim::Void)),
        Some(l) => Tree::Base(entry.to_global(l)),
    };
    let mut types = Vec::with_capacity(16);
    let mut found_void = false;
    types.push(retty);
    entry.for_each_child(|child| {
            let entry = child.entry();
            if entry.tag() != DW_TAG_formal_parameter {
                if !allow_other_tags {
                    cu::bail!("expecting all children of subroutine type to be DW_TAG_formal_parameter, at subroutine-like type at {offset}");
                } else {
                    return Ok(());
                }
            }
            let local_off = cu::check!(entry.loff_opt(DW_AT_type), "failed to read parameter type for subroutine-like type at {offset}")?;
            if let Some(l) = local_off {
                // skip void parameters
                types.push(Tree::Base(entry.to_global(l)));
            } else {
                found_void = true;
            }
            Ok(())
        })?;
    if found_void && types.len() != 1 {
        cu::bail!("unexpected void parameter in subroutine-like type at {offset}");
    }

    Ok(types)
}

fn load_enum_type_from_entry(entry: &Die<'_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<Type0> {
    let offset = entry.goff();
    let name = cu::check!(
        entry.namespaced_name_opt(&ctx.namespaces),
        "failed to get enum name at {offset}"
    )?;
    let is_decl = cu::check!(
        entry.flag(DW_AT_declaration),
        "failed to check if enum is declaration at {offset}"
    )?;
    if is_decl {
        let name = cu::check!(name, "unexpected enum decl without name at {offset}")?;
        return Ok(Type0::EnumDecl(name));
    }
    let byte_size_or_base = match cu::check!(entry.loff_opt(DW_AT_type), "failed to get enum base type at {offset}")? {
        None => {
            // does not have base, check byte size
            let byte_size = cu::check!(entry.uint(DW_AT_byte_size), "failed to get enum byte size at {offset}")?;
            if byte_size > u32::MAX as u64 {
                cu::bail!("enum at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
            }
            Ok(byte_size as u32)
        }
        Some(l) => Err(entry.to_global(l)),
    };
    let mut enumerators = Vec::with_capacity(16);
    let result = entry.for_each_child(|child| {
        let entry = child.entry();
        let offset = entry.goff();
        match entry.tag() {
            DW_TAG_enumerator => {
                let name = cu::check!(entry.name(), "failed to get enumerator name at {offset}")?;
                let value = cu::check!(
                    entry.int(DW_AT_const_value),
                    "failed to get enumerator value at {offset}"
                )?;
                enumerators.push(Enumerator {
                    name: Arc::from(name),
                    value,
                });
            }
            tag => {
                cu::bail!("expecting all enum children entries to be DW_TAG_enumerator, but got {tag}")
            }
        }
        Ok(())
    });
    cu::check!(result, "failed to collect enumerators for enum type at {offset}")?;
    Ok(Type0::Enum(
        name,
        Type0Enum {
            byte_size_or_base,
            enumerators,
        },
    ))
}

fn load_union_type_from_entry(entry: &Die<'_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<Type0> {
    let offset = entry.goff();
    let name = cu::check!(
        entry.namespaced_name_opt(&ctx.namespaces),
        "failed to get union name at {offset}"
    )?;
    let is_decl = cu::check!(
        entry.flag(DW_AT_declaration),
        "failed to check if union is declaration at {offset}"
    )?;
    if is_decl {
        let name = cu::check!(name, "unexpected union decl without name at {offset}")?;
        return Ok(Type0::UnionDecl(name));
    }

    let byte_size = cu::check!(entry.uint(DW_AT_byte_size), "failed to get union byte size at {offset}")?;
    if byte_size > u32::MAX as u64 {
        cu::bail!("union at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
    }
    let byte_size = byte_size as u32;

    let mut members = Vec::<Type0Member>::with_capacity(16);
    entry.for_each_child(|child| {
        let entry = child.entry();
        let offset = entry.goff();
        match entry.tag() {
            DW_TAG_member => {
                let name = entry.name_opt()?.map(Arc::from);
                let type_loff = cu::check!(
                    entry.loff_opt(DW_AT_type),
                    "failed to get type for union member at {offset}"
                )?;
                let type_loff = cu::check!(type_loff, "unexpected void-typed union member at {offset}")?;
                let type_offset = entry.to_global(type_loff);
                // if type is duplicated, just ignore it
                match members.iter_mut().find(|x| x.ty == type_offset) {
                    None => members.push(Type0Member {
                        offset: 0,
                        name,
                        ty: type_offset,
                        special: None,
                    }),
                    Some(old) => {
                        // update the name if we have it now
                        if old.name.is_none() {
                            old.name = name;
                        }
                    }
                }
            }
            DW_TAG_structure_type
            | DW_TAG_class_type
            | DW_TAG_union_type
            | DW_TAG_enumeration_type
            | DW_TAG_typedef
            | DW_TAG_template_type_parameter
            | DW_TAG_template_value_parameter
            | DW_TAG_GNU_template_parameter_pack => {
                // ignore subtypes
            }
            DW_TAG_subprogram => {
                // unions can't be virtual for now
                cu::ensure!(
                    entry.vtable_index()?.is_none(),
                    "unsupported virtual function in union at {offset}"
                );
            }
            tag => {
                cu::bail!("unexpected tag {tag} at {offset} while processing union");
            }
        }
        Ok(())
    })?;

    Ok(Type0::Union(name, Type0Union { byte_size, members }))
}

fn load_struct_type_from_entry(entry: &Die<'_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<Type0> {
    let offset = entry.goff();
    let name = cu::check!(
        entry.namespaced_name_opt(&ctx.namespaces),
        "failed to get struct name at {offset}"
    )?;
    let is_decl = cu::check!(
        entry.flag(DW_AT_declaration),
        "failed to check if struct is declaration at {offset}"
    )?;
    if is_decl {
        let name = cu::check!(name, "unexpected struct decl without name at {offset}")?;
        return Ok(Type0::StructDecl(name));
    }

    let byte_size = cu::check!(
        entry.uint(DW_AT_byte_size),
        "failed to get struct byte size at {offset}"
    )?;
    if byte_size > u32::MAX as u64 {
        cu::bail!("struct at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
    }
    let byte_size = byte_size as u32;

    let mut vtable = Vec::default();
    let mut members = Vec::<Type0Member>::with_capacity(16);

    let result = entry.for_each_child(|child| {
        let entry = child.entry();
        let offset = entry.goff();
        match entry.tag() {
            DW_TAG_member => {
                if entry.flag(DW_AT_external)? {
                    // static member
                    return Ok(());
                }
                // member might be anonymous union
                let name = cu::check!(entry.name_opt(), "failed to get struct member name at {offset}")?;
                let type_loff = cu::check!(
                    entry.loff_opt(DW_AT_type),
                    "failed to get struct member type at {offset}"
                )?;
                let type_loff = cu::check!(type_loff, "unexpected void-typed struct member at {offset}")?;
                let type_offset = entry.to_global(type_loff);
                let member_offset = cu::check!(
                    entry.uint(DW_AT_data_member_location),
                    "failed to get struct member offset at {offset}"
                )?;
                cu::ensure!(
                    member_offset < u32::MAX as u64,
                    "member_offset is too big for member at {offset}. This is unlikely to be correct."
                );
                let member_offset = member_offset as u32;

                // for vfptr fields, we change the loaded type to pointer primitive,
                // to reduce complexity. It is assumed that vfptr must be at offset 0,
                // since any other vptr field should be contained in the base class
                let mut member = if let Some(n) = name
                    && ctx.config.extract.vfptr_field_regex.is_match(n)
                {
                    cu::ensure!(
                        member_offset == 0,
                        "unexpected vfptr field at non-zero offset, for member at {offset}"
                    );
                    Type0Member {
                        offset: 0,
                        name: None,
                        ty: Goff::prim(ctx.pointer_type),
                        special: Some(SpecialMember::Vfptr),
                    }
                } else {
                    Type0Member {
                        offset: member_offset,
                        name: name.map(Arc::from),
                        ty: type_offset,
                        special: None,
                    }
                };

                if cu::check!(
                    entry.uint_opt(DW_AT_bit_size),
                    "failed to check if struct member is bitfield at {offset}"
                )?
                .is_some()
                {
                    let bitfield_byte_size = cu::check!(
                        entry.uint(DW_AT_byte_size),
                        "failed to get byte size of struct bitfield member at {offset}"
                    )?;
                    // bitfields are merged into one member of that type
                    // bitfield names are ignored for now
                    cu::ensure!(
                        bitfield_byte_size < u32::MAX as u64,
                        "bitfield_byte_size is too big for member at {offset}. This is unlikely to be correct."
                    );
                    member.special = Some(SpecialMember::Bitfield(bitfield_byte_size as u32));
                    // can merge with last member if it's the same bitfield
                    if let Some(prev) = members.last_mut() {
                        if prev.offset == member.offset && matches!(prev.special, Some(SpecialMember::Bitfield(_))) {
                            *prev = member;
                            return Ok(());
                        }
                    }
                }
                members.push(member);
            }
            DW_TAG_inheritance => {
                let member_offset = cu::check!(
                    entry.uint(DW_AT_data_member_location),
                    "failed to get struct base class offset at {offset}"
                )?;
                cu::ensure!(
                    member_offset < u32::MAX as u64,
                    "member_offset is too big for base class at {offset}. This is unlikely to be correct."
                );
                let member_offset = member_offset as u32;
                let type_loff = cu::check!(
                    entry.loff_opt(DW_AT_type),
                    "failed to get struct base class type at {offset}"
                )?;
                let type_loff = cu::check!(type_loff, "unexpected void-typed struct base class at {offset}")?;
                let type_offset = entry.to_global(type_loff);
                members.push(Type0Member {
                    offset: member_offset,
                    name: None, // we will assign name to base members in a later step
                    ty: type_offset,
                    special: Some(SpecialMember::Base),
                });
            }
            DW_TAG_subprogram => {
                let Some(velem) = cu::check!(
                    entry.vtable_index(),
                    "failed to get struct virtual function vtable index at {offset}"
                )?
                else {
                    // not virtual function, no need to process
                    return Ok(());
                };
                let name = cu::check!(entry.name(), "failed to get virtual function name at {offset}")?;
                let name = Arc::from(name);
                let function_types = cu::check!(
                    load_subroutine_types_from_entry(&entry, false),
                    "failed to read virtual function data at {offset}"
                )?;
                vtable.push((velem, VtableEntry { name, function_types }));
            }
            DW_TAG_structure_type
            | DW_TAG_class_type
            | DW_TAG_union_type
            | DW_TAG_enumeration_type
            | DW_TAG_typedef
            | DW_TAG_template_type_parameter
            | DW_TAG_template_value_parameter
            | DW_TAG_GNU_template_parameter_pack => {
                // ignore subtypes
            }
            tag => cu::bail!("unexpected tag {tag} at {offset}"),
        }
        Ok(())
    });
    cu::check!(result, "failed to process struct data for entry at {offset}")?;

    // members may not come sorted by offset, we do that now
    // and ensure no duplicates
    // we can look for empty base optimization right here
    // since we will know if another member is placed at the same location.
    // this sort put base after other members
    members.sort_by_key(|x| x.is_base());
    members.sort_by_key(|x| x.offset);
    let mut conflicting_member_offset = None;
    let mut prev_offset = u32::MAX;
    members.retain(|member| {
        if member.offset == prev_offset {
            if member.is_base() {
                // empty-base optimization: remove the base class field completely
                // note that this is fine since empty base also means the base
                // has no vtable
                return false;
            }
            if conflicting_member_offset.is_none() {
                conflicting_member_offset = Some(prev_offset);
            }
        }
        prev_offset = member.offset;
        true
    });
    if let Some(x) = conflicting_member_offset {
        cu::bail!("found multiple members at the same offset 0x{x:x} for struct at {offset}: {members:#?}");
    }

    Ok(Type0::Struct(
        name,
        Type0Struct {
            byte_size,
            members,
            vtable,
        },
    ))
}

/// Assert the entry is DW_TAG_array_type, and get the DW_AT_count of the DW_TAG_subrange_type
fn load_array_subrange_count(entry: &Die<'_, '_>) -> cu::Result<Option<u32>> {
    let offset = entry.goff();
    let mut count = None;
    let mut found_subrange = false;
    let result = entry.for_each_child(|child| {
        let entry = child.entry();
        let offset = entry.goff();
        match entry.tag() {
            DW_TAG_subrange_type => {
                found_subrange = true;
                let count_64 = cu::check!(
                    entry.uint_opt(DW_AT_count),
                    "failed to get count for subrange type at {offset}"
                )?;
                count = match count_64 {
                    None => None,
                    Some(count) => {
                        cu::ensure!(
                            count < u32::MAX as u64,
                            "array length is too big: {count}. This is unlikely to be correct."
                        );
                        Some(count as u32)
                    }
                };
            }
            tag => cu::bail!("unexpected tag {tag} at {offset} while processing array type"),
        }
        Ok(())
    });
    cu::check!(result, "failed to process array type at {offset}")?;
    cu::ensure!(
        found_subrange,
        "did not find DW_TAG_subrange_type for array type at {offset}"
    );
    Ok(count)
}

fn make_ptr(goff: Goff) -> Type0 {
    Type0::Tree(Tree::ptr(Tree::Base(goff)))
}

struct LoadTypeCtx {
    pointer_type: Prim,
    config: Arc<Config>,
    types: GoffMap<Type0>,
    namespaces: GoffMap<Namespace>,
}
