use std::collections::BTreeMap;
use std::sync::Arc;

use cu::pre::*;
use tyyaml::{Prim, Tree};

use crate::config::Config;
use crate::symlist::SymbolList;

use super::pre::*;
use super::type_structure::*;

/// Load the type data from DWARF units
pub fn load(
    unit: &Unit,
    config: Arc<Config>,
    nsmaps: NamespaceMaps,
    symbol_list: Arc<SymbolList>,
) -> cu::Result<Stage0> {
    let pointer_type = config.extract.pointer_type()?;
    let mut types = GoffMap::default();
    // add primitive types
    for p in Prim::iter() {
        types.insert(Goff::prim(p), Type0::Prim(p));
    }
    let mut ctx = LoadTypeCtx {
        pointer_type,
        config,
        types,
        nsmaps,
    };
    cu::debug!("loading types for {unit}");
    cu::check!(load_types_root(unit, &mut ctx), "failed to load types for {unit}")?;
    cu::debug!("loaded {} types from {unit}", ctx.types.len());

    let mut ctx2 = LoadSymbolCtx {
        loaded: Default::default(),
        symbol_list,
    };
    cu::debug!("loading symbols for {unit}");
    cu::check!(load_symbols_root(unit, &mut ctx2), "failed to load symbols for {unit}")?;
    cu::debug!("loaded {} symbols from {unit}", ctx2.loaded.len());

    Ok(Stage0 {
        offset: unit.offset.into(),
        name: unit.name.to_string(),
        types: ctx.types,
        config: ctx.config,
        ns: ctx.nsmaps,
        symbols: ctx2.loaded,
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
                        entry.qual_name(&ctx.nsmaps),
                        "failed to read name of the typedef at {offset}"
                    )?;
                    // note: typedef name could be templated with using
                    // for example:
                    // template <bool __b>
                    // using bool_constant = integral_constant<bool, __b>;
                    let mut abandon = false;
                    match typedef_name.to_cpp_typedef_source() {
                        Ok(cpp_name) => {
                            for r in &ctx.config.extract.type_parser.abandon_typedefs {
                                if r.is_match(&cpp_name) {
                                    abandon = true;
                                    break;
                                }
                            }
                        }
                        Err(_) => {
                            abandon = true;
                        }
                    }
                    if abandon {
                        Type0::Alias(entry.to_global(loff))
                    } else {
                        Type0::Typedef(typedef_name, entry.to_global(loff))
                    }
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
        // modifiers that don't affect the type
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
    let is_decl = cu::check!(
        entry.flag(DW_AT_declaration),
        "failed to check if enum is declaration at {offset}"
    )?;
    if is_decl {
        // keep the templates for resolution
        let name = cu::check!(entry.qual_name(&ctx.nsmaps), "failed to get enum decl name at {offset}")?;
        let decl_namespace = cu::check!(
            ctx.nsmaps.namespaces.get(&offset),
            "failed to get namespace for enum decl at {offset}"
        )?;
        return Ok(Type0::EnumDecl(decl_namespace.clone(), name));
    }
    // remove templates from name for definition,
    // since we get those from DWARF nodes
    let name = cu::check!(
        entry.untemplated_qual_name_opt(&ctx.nsmaps),
        "failed to get enum name at {offset}"
    )?;

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
    let is_decl = cu::check!(
        entry.flag(DW_AT_declaration),
        "failed to check if union is declaration at {offset}"
    )?;
    if is_decl {
        // keep the templates for resolution
        let name = cu::check!(
            entry.qual_name(&ctx.nsmaps),
            "failed to get union decl name at {offset}"
        )?;
        let decl_namespace = cu::check!(
            ctx.nsmaps.namespaces.get(&offset),
            "failed to get namespace for union decl at {offset}"
        )?;
        return Ok(Type0::UnionDecl(decl_namespace.clone(), name));
    }
    // remove templates from name for definition,
    // since we get those from DWARF nodes
    let name = cu::check!(
        entry.untemplated_qual_name_opt(&ctx.nsmaps),
        "failed to get union name at {offset}"
    )?;

    let byte_size = cu::check!(entry.uint(DW_AT_byte_size), "failed to get union byte size at {offset}")?;
    if byte_size > u32::MAX as u64 {
        cu::bail!("union at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
    }
    let byte_size = byte_size as u32;

    let mut template_args = Vec::new();
    let mut members = Vec::<Member>::with_capacity(16);
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
                match members.iter_mut().find(|x| x.ty == Tree::Base(type_offset)) {
                    None => members.push(Member {
                        offset: 0,
                        name,
                        ty: Tree::Base(type_offset),
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
            // template args
            DW_TAG_template_type_parameter | DW_TAG_template_value_parameter | DW_TAG_GNU_template_parameter_pack => {
                cu::check!(
                    load_template_parameter(&entry, &mut template_args),
                    "failed to load template parameter for union at {offset}"
                )?;
            }
            DW_TAG_structure_type
            | DW_TAG_class_type
            | DW_TAG_union_type
            | DW_TAG_enumeration_type
            | DW_TAG_typedef => {
                // ignore subtypes, since they will be recursed into later
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

    Ok(Type0::Union(
        name,
        Type0Union {
            template_args,
            byte_size,
            members,
        },
    ))
}

fn load_struct_type_from_entry(entry: &Die<'_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<Type0> {
    let offset = entry.goff();
    let is_decl = cu::check!(
        entry.flag(DW_AT_declaration),
        "failed to check if struct is declaration at {offset}"
    )?;
    if is_decl {
        // keep the templates for resolution
        let name = cu::check!(
            entry.qual_name(&ctx.nsmaps),
            "failed to get struct decl name at {offset}"
        )?;
        let decl_namespace = cu::check!(
            ctx.nsmaps.namespaces.get(&offset),
            "failed to get namespace for struct decl at {offset}"
        )?;
        return Ok(Type0::StructDecl(decl_namespace.clone(), name));
    }
    // remove templates from name for definition,
    // since we get those from DWARF nodes
    let name = cu::check!(
        entry.untemplated_qual_name_opt(&ctx.nsmaps),
        "failed to get struct name at {offset}"
    )?;

    let byte_size = cu::check!(
        entry.uint(DW_AT_byte_size),
        "failed to get struct byte size at {offset}"
    )?;
    if byte_size > u32::MAX as u64 {
        cu::bail!("struct at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
    }
    let byte_size = byte_size as u32;

    let mut vtable = Vec::default();
    let mut template_args = Vec::new();
    let mut members = Vec::<Member>::with_capacity(16);

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
                    Member {
                        offset: 0,
                        name: None,
                        ty: Tree::Base(Goff::prim(ctx.pointer_type)),
                        special: Some(SpecialMember::Vfptr),
                    }
                } else {
                    Member {
                        offset: member_offset,
                        name: name.map(Arc::from),
                        ty: Tree::Base(type_offset),
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
                members.push(Member {
                    offset: member_offset,
                    name: None, // we will assign name to base members in a later step
                    ty: Tree::Base(type_offset),
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
            // template args
            DW_TAG_template_type_parameter | DW_TAG_template_value_parameter | DW_TAG_GNU_template_parameter_pack => {
                cu::check!(
                    load_template_parameter(&entry, &mut template_args),
                    "failed to load template parameter for struct at {offset}"
                )?;
            }
            DW_TAG_structure_type
            | DW_TAG_class_type
            | DW_TAG_union_type
            | DW_TAG_enumeration_type
            | DW_TAG_typedef => {
                // ignore subtypes, since they will be recursed into later
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
            template_args,
            byte_size,
            members,
            vtable,
        },
    ))
}

/// Load template type parameters into the output vec
///
/// The entry should be one of:
/// - DW_TAG_template_type_parameter,
/// - DW_TAG_template_value_parameter
/// - DW_TAG_GNU_template_parameter_pack
fn load_template_parameter(entry: &Die<'_, '_>, out: &mut Vec<TemplateArg<Goff>>) -> cu::Result<()> {
    match entry.tag() {
        DW_TAG_template_type_parameter => {
            let type_loff = cu::check!(
                entry.loff_opt(DW_AT_type),
                "failed to get template type parameter at {}",
                entry.goff()
            )?;
            let type_goff = match type_loff {
                Some(l) => entry.to_global(l),
                None => Goff::prim(Prim::Void),
            };
            out.push(TemplateArg::Type(Tree::Base(type_goff)));
        }
        DW_TAG_template_value_parameter => {
            let value = cu::check!(
                entry.int_opt(DW_AT_const_value),
                "failed to get template value paramter at {}",
                entry.goff()
            )?;
            let arg = match value {
                Some(i) => TemplateArg::Const(i),
                None => TemplateArg::StaticConst,
            };
            out.push(arg);
        }
        DW_TAG_GNU_template_parameter_pack => {
            let result = entry.for_each_child(|child| {
                let entry = child.entry();
                load_template_parameter(&entry, out)
            });
            cu::check!(
                result,
                "failed to process GNU template parameter pack at {}",
                entry.goff()
            )?;
        }
        tag => cu::bail!(
            "unexpected tag {tag} while processing template type parameter at {}",
            entry.goff()
        ),
    }
    Ok(())
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

fn load_symbols_root(unit: &Unit, ctx: &mut LoadSymbolCtx) -> cu::Result<()> {
    let mut tree = unit.tree()?;
    let root = tree.root()?;
    load_symbols_recur(root, ctx)?;
    Ok(())
}

fn load_symbols_recur(mut node: DieNode<'_, '_>, ctx: &mut LoadSymbolCtx) -> cu::Result<()> {
    let entry = node.entry();
    match entry.tag() {
        DW_TAG_subprogram => {
            node = load_func_symbol_at(node, ctx)?;
        }
        DW_TAG_variable => {
            node = load_data_symbol_at(node, ctx)?;
        }
        _ => {}
    }
    node.for_each_child(|child| load_symbols_recur(child, ctx))
}

fn load_data_symbol_at<'a, 'b>(node: DieNode<'a, 'b>, ctx: &mut LoadSymbolCtx) -> cu::Result<DieNode<'a, 'b>> {
    let entry = node.entry();
    let offset = entry.goff();
    let linkage_name = cu::check!(
        entry.str_opt(DW_AT_linkage_name),
        "failed to get linkage name for variable at {offset}"
    )?;
    let Some(linkage_name) = linkage_name else {
        // ignore variables without linkage name
        return Ok(node);
    };

    let loff = cu::check!(
        entry.loff_opt(DW_AT_type),
        "failed to get type offset for data symbol at {offset}"
    )?;
    let loff = match loff {
        Some(l) => l,
        None => {
            // try specification
            let spec = cu::check!(
                entry.loff(DW_AT_specification),
                "failed to get fallback specification for data symbol without type at {offset}"
            )?;
            let spec = cu::check!(
                entry.unit().entry_at(spec),
                "failed to get spec entry for data symbol at {offset}"
            )?;
            cu::check!(
                spec.loff(DW_AT_type),
                "failed to get fallback type offset from spec entry for data symbol at {offset}"
            )?
        }
    };
    let symbol = SymbolInfo::new_data(linkage_name.to_string(), entry.to_global(loff));
    cu::check!(
        merge_symbol(linkage_name, symbol, ctx),
        "failed to merge data symbol at {offset}"
    )?;
    Ok(node)
}

fn load_func_symbol_at<'a, 'b>(node: DieNode<'a, 'b>, ctx: &mut LoadSymbolCtx) -> cu::Result<DieNode<'a, 'b>> {
    let entry = node.entry();
    let offset = entry.goff();
    let is_decl = cu::check!(
        entry.flag(DW_AT_declaration),
        "failed to check if function is declaration at {offset}"
    )?;
    if is_decl {
        // skip declaration nodes
        return Ok(node);
    }
    let linkage_name = cu::check!(
        load_func_linkage_name(&entry),
        "failed to get linkage name for function at {offset}"
    )?;
    let Some(linkage_name) = linkage_name else {
        // ignore functions without linkage name
        return Ok(node);
    };
    // non-declaration should have low_pc and high_pc, or be inlined
    let low_pc = cu::check!(
        entry.uint_opt(DW_AT_low_pc),
        "failed to get low_pc for function definition at {offset}"
    )?;
    if low_pc.is_none() {
        let is_inlined = cu::check!(
            load_func_is_inlined(&entry),
            "failed to check if function definition is inlined at {offset}"
        )?;
        cu::ensure!(
            is_inlined,
            "function at {offset} is not inlined and does not have low_pc"
        );
    }

    let mut types = vec![];

    // return type
    let retty = cu::check!(
        load_func_retty(&entry),
        "failed to get return type for function at {offset}"
    )?;
    let retty = match retty {
        None => Goff::prim(Prim::Void),
        Some(l) => entry.to_global(l),
    };
    types.push(Tree::Base(retty));

    let mut param_names = vec![];
    let mut template_args = vec![];
    let result = entry.for_each_child(|child| {
        let entry = child.entry();
        let offset = entry.goff();
        match entry.tag() {
            // template args
            DW_TAG_template_type_parameter | DW_TAG_template_value_parameter | DW_TAG_GNU_template_parameter_pack => {
                cu::check!(
                    load_template_parameter(&entry, &mut template_args),
                    "failed to load template parameter for function at {offset}"
                )?;
            }
            DW_TAG_formal_parameter => {
                let name = cu::check!(
                    load_func_param_name(&entry),
                    "failed to get function parameter name at {offset}"
                )?;
                let ty_loff = cu::check!(
                    load_func_param_type(&entry),
                    "failed to get function parameter type at {offset}"
                )?;
                let ty_loff = cu::check!(ty_loff, "missing parameter type at {offset}")?;
                let ty = entry.to_global(ty_loff);
                types.push(Tree::Base(ty));
                param_names.push(name.unwrap_or_default());
            }
            // DW_TAG_variable => {
            //     let ty = cu::check!(entry.loff_opt(DW_AT_type), "failed to get function local variable type at {offset}")?;
            //     if let Some(loff) = ty {
            //         let ty = entry.to_global(loff);
            //         referenced_types.insert(ty);
            //     }
            // }
            _ => {
                // ignore others
                // DW_TAG_inlined_subroutine could potentially be useful
                // to add comment about inlined functions
                // DW_TAG_variable could be useful if we want to GC types to avoid
                // removing types needed for function bodies
            }
        }
        Ok(())
    });
    cu::check!(result, "failed to process function body at {offset}")?;

    let symbol = SymbolInfo::new_func(linkage_name.clone(), types, param_names, template_args);
    cu::check!(
        merge_symbol(&linkage_name, symbol, ctx),
        "failed to merge function symbol at {offset}"
    )?;
    Ok(node)
}

fn load_func_is_inlined<'a>(entry: &'a Die<'_, '_>) -> cu::Result<bool> {
    let offset = entry.goff();
    let has_inline_attr = cu::check!(
        entry.is_inlined(),
        "failed to check if function entry has DW_AT_inlined at {offset}"
    )?;
    if has_inline_attr {
        return Ok(true);
    }
    let abstract_origin = cu::check!(
        entry.loff_opt(DW_AT_abstract_origin),
        "failed to read abstract origin for function at {offset}"
    )?;
    if let Some(abstract_origin) = abstract_origin {
        let entry = cu::check!(
            entry.unit().entry_at(abstract_origin),
            "failed to read abstract origin entry for function at {offset}"
        )?;
        let has_inline_attr = cu::check!(
            entry.is_inlined(),
            "failed to check if function entry has DW_AT_inlined from abstract origin at {offset}"
        )?;
        if has_inline_attr {
            return Ok(true);
        }
    }
    let specification = cu::check!(
        entry.loff_opt(DW_AT_specification),
        "failed to read specification for function at {offset}"
    )?;
    if let Some(specification) = specification {
        let entry = cu::check!(
            entry.unit().entry_at(specification),
            "failed to read specification entry for function at {offset}"
        )?;
        let has_inline_attr = cu::check!(
            entry.is_inlined(),
            "failed to check if function entry has DW_AT_inlined from specification at {offset}"
        )?;
        if has_inline_attr {
            return Ok(true);
        }
    }
    Ok(false)
}

fn load_func_linkage_name<'a>(entry: &'a Die<'_, '_>) -> cu::Result<Option<String>> {
    let offset = entry.goff();
    let linkage_name = cu::check!(
        entry.str_opt(DW_AT_linkage_name),
        "failed to read linkage name for function at {offset}"
    )?;
    if let Some(linkage_name) = linkage_name {
        return Ok(Some(linkage_name.to_string()));
    }
    let abstract_origin = cu::check!(
        entry.loff_opt(DW_AT_abstract_origin),
        "failed to read abstract origin for function at {offset}"
    )?;
    if let Some(abstract_origin) = abstract_origin {
        let entry = cu::check!(
            entry.unit().entry_at(abstract_origin),
            "failed to read abstract origin entry for function at {offset}"
        )?;
        let name = cu::check!(
            load_func_linkage_name(&entry),
            "failed to load linkage_name from abstract origin entry, for function at {offset}"
        )?;
        if let Some(name) = name {
            return Ok(Some(name));
        }
    }
    let specification = cu::check!(
        entry.loff_opt(DW_AT_specification),
        "failed to read specification for function at {offset}"
    )?;
    if let Some(specification) = specification {
        let entry = cu::check!(
            entry.unit().entry_at(specification),
            "failed to read specification entry for function at {offset}"
        )?;
        let name = cu::check!(
            load_func_linkage_name(&entry),
            "failed to load linkage_name from specification entry, for function at {offset}"
        )?;
        if let Some(name) = name {
            return Ok(Some(name));
        }
    }
    Ok(None)
}

fn load_func_retty(entry: &Die<'_, '_>) -> cu::Result<Option<Loff>> {
    let offset = entry.goff();
    let loff = cu::check!(
        entry.loff_opt(DW_AT_type),
        "failed to read type for function entry at {offset}"
    )?;
    if let Some(loff) = loff {
        return Ok(Some(loff));
    }
    let abstract_origin = cu::check!(
        entry.loff_opt(DW_AT_abstract_origin),
        "failed to read abstract origin for function at {offset}"
    )?;
    if let Some(abstract_origin) = abstract_origin {
        let entry = cu::check!(
            entry.unit().entry_at(abstract_origin),
            "failed to read abstract origin entry for function at {offset}"
        )?;
        let loff = cu::check!(
            load_func_retty(&entry),
            "failed to load retty from abstract origin entry, for function at {offset}"
        )?;
        if let Some(loff) = loff {
            return Ok(Some(loff));
        }
    }
    let specification = cu::check!(
        entry.loff_opt(DW_AT_specification),
        "failed to read specification for function at {offset}"
    )?;
    if let Some(specification) = specification {
        let entry = cu::check!(
            entry.unit().entry_at(specification),
            "failed to read specification entry for function at {offset}"
        )?;
        let loff = cu::check!(
            load_func_retty(&entry),
            "failed to load retty from specification entry, for function at {offset}"
        )?;
        if let Some(loff) = loff {
            return Ok(Some(loff));
        }
    }
    Ok(None)
}

fn load_func_param_name(entry: &Die<'_, '_>) -> cu::Result<Option<String>> {
    let offset = entry.goff();
    let name = cu::check!(
        entry.name_opt(),
        "failed to read name for function param entry at {offset}"
    )?;
    if let Some(name) = name {
        return Ok(Some(name.to_string()));
    }
    let abstract_origin = cu::check!(
        entry.loff_opt(DW_AT_abstract_origin),
        "failed to read abstract origin for function at {offset}"
    )?;
    if let Some(abstract_origin) = abstract_origin {
        let entry = cu::check!(
            entry.unit().entry_at(abstract_origin),
            "failed to read abstract origin entry for function at {offset}"
        )?;
        let name = cu::check!(
            load_func_param_name(&entry),
            "failed to load param name from abstract origin entry, for function at {offset}"
        )?;
        if let Some(name) = name {
            return Ok(Some(name));
        }
    }
    let specification = cu::check!(
        entry.loff_opt(DW_AT_specification),
        "failed to read specification for function at {offset}"
    )?;
    if let Some(specification) = specification {
        let entry = cu::check!(
            entry.unit().entry_at(specification),
            "failed to read specification entry for function at {offset}"
        )?;
        let name = cu::check!(
            load_func_param_name(&entry),
            "failed to load param name from specification entry, for function at {offset}"
        )?;
        if let Some(name) = name {
            return Ok(Some(name));
        }
    }
    Ok(None)
}

fn load_func_param_type(entry: &Die<'_, '_>) -> cu::Result<Option<Loff>> {
    let offset = entry.goff();
    let loff = cu::check!(
        entry.loff_opt(DW_AT_type),
        "failed to read type for function param entry at {offset}"
    )?;
    if let Some(l) = loff {
        return Ok(Some(l));
    }
    let abstract_origin = cu::check!(
        entry.loff_opt(DW_AT_abstract_origin),
        "failed to read abstract origin for function at {offset}"
    )?;
    if let Some(abstract_origin) = abstract_origin {
        let entry = cu::check!(
            entry.unit().entry_at(abstract_origin),
            "failed to read abstract origin entry for function at {offset}"
        )?;
        let loff = cu::check!(
            load_func_param_type(&entry),
            "failed to load param type from abstract origin entry, for function at {offset}"
        )?;
        if let Some(l) = loff {
            return Ok(Some(l));
        }
    }
    let specification = cu::check!(
        entry.loff_opt(DW_AT_specification),
        "failed to read specification for function at {offset}"
    )?;
    if let Some(specification) = specification {
        let entry = cu::check!(
            entry.unit().entry_at(specification),
            "failed to read specification entry for function at {offset}"
        )?;
        let loff = cu::check!(
            load_func_param_type(&entry),
            "failed to load param type from specification entry, for function at {offset}"
        )?;
        if let Some(l) = loff {
            return Ok(Some(l));
        }
    }
    Ok(None)
}

fn merge_symbol(linkage_name: &str, mut symbol: SymbolInfo, ctx: &mut LoadSymbolCtx) -> cu::Result<()> {
    match ctx.loaded.get_mut(linkage_name) {
        None => {
            let Some(address) = ctx.symbol_list.get_address(linkage_name) else {
                // ignore symbols that aren't listed
                return Ok(());
            };
            symbol.address = address;
            ctx.loaded.insert(linkage_name.to_string(), symbol);
        }
        Some(old_symbol) => {
            cu::check!(
                old_symbol.merge(&symbol),
                "failed to merge symbols: old: {old_symbol:#?}, new: {symbol:#?}"
            )?;
            // let old_addr = old_symbol.address;
            // old_symbol.address = 0;
            // cu::ensure!(&symbol == old_symbol, "symbol info mismatch when merging, old: {old_symbol:#?}, new: {symbol:#?}");
            // old_symbol.address = old_addr;
            // old_symbol.link_name = linkage_name.to_string();
        }
    }
    Ok(())
}

fn make_ptr(goff: Goff) -> Type0 {
    Type0::Tree(Tree::ptr(Tree::Base(goff)))
}

struct LoadTypeCtx {
    pointer_type: Prim,
    config: Arc<Config>,
    types: GoffMap<Type0>,
    nsmaps: NamespaceMaps,
}

struct LoadSymbolCtx {
    // damangler: Arc<Demangler>,
    loaded: BTreeMap<String, SymbolInfo>,
    symbol_list: Arc<SymbolList>,
    // merges: Vec<(Goff, Goff)>,
}
