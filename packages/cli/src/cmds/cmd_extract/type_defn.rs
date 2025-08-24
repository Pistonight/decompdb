use std::collections::{BTreeMap, BTreeSet};

use cu::pre::*;
use fxhash::FxHashSet;
use gimli::AttributeValue;
use tyyaml::{Prim, TyTree};

use super::pre::*;

// pub struct TypeMap {
//     offset_to_canonical_offset: GoffMap<Goff>,
//     canonical_offset_to_entry: GoffMap<TypeEntry>,
// }
//
// impl TypeMap {
//     /// Get the canonical global offset for type at the offset, at the time of calling
//     pub fn canonicalize(&self, offset: Goff) -> Goff {
//         self.offset_to_canonical_offset.get(&offset).copied().unwrap_or(offset)
//     }
//
//     pub fn get_mut_or_insert_with
//     <F: FnOnce(&mut Self) -> cu::Result<TypeEntry>>(
//     &mut self, offset: Goff, insert: F) -> cu::Result<&mut TypeEntry> {
//         let canonical_offset = self.canonicalize(offset);
//         if let Some(entry) = self.canonical_offset_to_entry.get_mut(canonical_offset) {
//             return Ok(entry);
//         }
//         let entry = insert(self)?;
//         let canonical_offset = self.canonicalize(offset);
//         use std::collections::btree_map::Entry;
//         match self.canonical_offset_to_entry.entry(canonical_offset) {
//             Entry::Vacant(mut e) => {
//                 let entry = insert(self)?;
//                 Ok(e.insert(entry))
//             }
//             Entry::Occupied(_) => {
//                 cu::bail!("")
//             }
//         }
//     }
//
//     pub fn insert_typedef_to_or_insert_with
//     <F: FnOnce(&mut Self) -> cu::Result<TypeEntry>>(
//         &mut self,
//         typedef_offset: Goff,
//         name: &str,
//         target_offset: Goff,
//         insert: F
//     ) -> cu::Result<()> {
//     }
// }
//
// pub struct TypeEntry {
//     /// Names this type go by (may be from typedef or optimization that merges types)
//     names: FxHashSet<String>,
// }

/// Definition of types parsed directly from DWARF
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type0 {
    /// Pritimive type
    Prim(Prim),
    /// Typedef <other> name; Other is offset in debug info
    Typedef(String, Goff),
    /// Alias to another type (basically typedef without a name)
    Alias(Goff),
    /// Enum
    Enum(String, Type0Enum),
    /// Anonymous Enum
    EnumAnon(Type0Enum),
    /// Declaration of enum
    EnumDecl(String),
    /// Union
    Union(String, Type0Union),
    /// Anonymous Union
    UnionAnon(Type0Union),
    /// Declaration of union
    UnionDecl(String),
    /// Struct or Class
    Struct(String, Type0Struct),
    /// Anonymous Struct
    StructAnon(Type0Struct),
    /// Declaration of struct or class
    StructDecl(String),
    /// Composition of other types
    Composite(TyTree<Option<Goff>>),
}

impl Type0 {
    fn ptr(offset: Goff) -> Self {
        Type0::Composite(TyTree::ptr(TyTree::Base(Some(offset))))
    }
    fn array(offset: Goff, len: u32) -> Self {
        Type0::Composite(TyTree::array(TyTree::Base(Some(offset)), len as usize))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Enum {
    /// Base type, used to determine the size
    size_or_base: Result<u32, Goff>,
    /// Enumerators of the enum, in the order they appear in DWARF
    enumerators: Vec<(String, i64)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Union {
    /// (name, type) of the union members
    /// Name could be None for anonymous struct as member
    members: Vec<(Option<String>, Goff)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Struct {
    members: Vec<Type0StructMember>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0StructMember {
    /// Offset of the member within the struct
    offset: usize,
    /// Name of the member. Could be None for anonymous union as member
    name: Option<String>,
    /// Type of the member
    ty: Goff,
    /// If the member is a base class
    is_base: bool,
    /// If the member is a bitfield, and the byte size
    bitfield_byte_size: Option<u32>,
}

impl<'i> CompUnit<'_, 'i> {
    pub fn load_types(&self) -> cu::Result<()> {
        todo!()
    }

    pub fn load_types_recur(
        &self,
        node: Node<'i, '_, '_, '_>,
        ctx: &mut LoadTypeCtx, // namespace: &mut NamespaceStack,
                               // offset_to_ns: &mut GoffMap<Namespace>,
    ) -> cu::Result<()> {
        let entry = node.entry();
        let tag = entry.tag();
        if is_type_tag(tag) {
            self.load_type_at(&node, ctx)?;
        }

        self.for_each_child(node, |child| self.load_types_recur(child, ctx))
    }

    /// Load the type at the node. The node must be a type
    pub fn load_type_at(&self, node: &Node<'i, '_, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<()> {
        let entry = node.entry();
        let offset = self.entry_goff(entry);
        let ty = match entry.tag() {
            DW_TAG_unspecified_type => {
                let name = self
                    .entry_name(&entry)
                    .context("unspecified type entry must have a name")?;
                match name {
                    // std::nullptr_t
                    "decltype(nullptr)" => Type0::Prim(ctx.pointer_type),
                    _ => {
                        cu::bail!("unknown name for unspecified type: {name}, for entry at {offset}");
                    }
                }
            }
            DW_TAG_typedef => {
                match cu::check!(self.entry_type_offset_opt(entry), "failed to read typedef at {offset}")? {
                    // void
                    None => Type0::Prim(Prim::Void),
                    Some(local_off) => {
                        let typedef_name = cu::check!(
                            self.namespaced_entry_name(entry, &ctx.namespaces),
                            "failed to read name of the typedef at {offset}"
                        )?;
                        Type0::Typedef(typedef_name, local_off.to_global(self.offset))
                    }
                }
            }
            // T* or T&
            DW_TAG_pointer_type | DW_TAG_reference_type => {
                match cu::check!(
                    self.entry_type_offset_opt(entry),
                    "failed to read pointee type at {offset}"
                )? {
                    None => Type0::Prim(Prim::Void),
                    Some(local_off) => Type0::ptr(local_off.to_global(self.offset)),
                }
            }
            // modifiers that don't do anything..
            DW_TAG_const_type | DW_TAG_volatile_type | DW_TAG_restrict_type => {
                match cu::check!(
                    self.entry_type_offset_opt(entry),
                    "failed to read alias type at {offset}"
                )? {
                    None => Type0::Prim(Prim::Void),
                    Some(local_off) => Type0::Alias(local_off.to_global(self.offset)),
                }
            }
            // T[n]
            DW_TAG_array_type => {
                let Some(local_off) = cu::check!(
                    self.entry_type_offset_opt(entry),
                    "failed to read array element type at {offset}"
                )?
                else {
                    cu::bail!("entry {offset} has void[] type, which is not allowed");
                };
                let array_len = cu::check!(
                    self.array_subrange_count(local_off),
                    "failed to get array length for array type at {offset}"
                )?;
                match array_len {
                    // without count, just use ptr type
                    None => Type0::ptr(local_off.to_global(self.offset)),
                    Some(len) => Type0::array(local_off.to_global(self.offset), len),
                }
            }
            // Subroutine
            DW_TAG_subroutine_type => {
                let subroutine_types = cu::check!(
                    self.load_subroutine_types_from_entry(entry),
                    "failed to read subroutine type at {offset}"
                )?;
                Type0::Composite(TyTree::Sub(subroutine_types))
            }
            // PTMD/PTMF
            DW_TAG_ptr_to_member_type => {
                let this_ty_loff = cu::check!(
                    self.entry_type_offset_attr(entry, DW_AT_containing_type),
                    "failed to read this type for pointer-to-member type at {offset}"
                )?;
                let this_ty_goff = this_ty_loff.to_global(self.offset);
                let pointee_ty_loff = cu::check!(
                    self.entry_type_offset_opt(entry),
                    "failed to read pointee type for pointer-to-member type at {offset}"
                )?;

                if let Some(pointee_ty_loff) = pointee_ty_loff {
                    let pointee_entry = cu::check!(
                        self.entry_at(pointee_ty_loff),
                        "failed to read pointee type entry for pointer-to-member type at {offset}"
                    )?;
                    if pointee_entry.tag() == DW_TAG_subroutine_type {
                        // PTMF
                        let subroutine_types = cu::check!(
                            self.load_subroutine_types_from_entry(&pointee_entry),
                            "failed to read pointee subroutine type for pointer-to-member-function type at {offset}"
                        )?;
                        Type0::Composite(TyTree::ptmf(this_ty_goff, subroutine_types))
                    } else {
                        // PTMD
                        let pointee_ty_goff = pointee_ty_loff.to_global(self.offset);
                        Type0::Composite(TyTree::ptmd(this_ty_goff, TyTree::Base(Some(pointee_ty_goff))))
                    }
                } else {
                    // PTMD to void
                    Type0::Composite(TyTree::ptmd(this_ty_goff, TyTree::Base(None)))
                }
            }
            DW_TAG_base_type => Type0::Prim(self.load_base_type_from_entry(entry)?),
            DW_TAG_enumeration_type => self.load_enum_type_from_entry(entry, ctx)?,
            DW_TAG_union_type => self.load_union_type_from_entry(entry, ctx)?,
            _ => todo!(),
        };
        ctx.types.insert(offset, ty);
        todo!()
    }

    fn load_base_type_from_entry(&self, entry: &Die<'i, '_, '_>) -> cu::Result<Prim> {
        let offset = self.entry_goff(entry);
        let encoding = cu::check!(
            entry.attr_value(DW_AT_encoding),
            "failed to read DW_AT_encoding for primitive type at offset {offset}"
        )?;
        let encoding = cu::check!(encoding, "missing DW_AT_encoding for primitive type at offset {offset}")?;
        let AttributeValue::Encoding(encoding) = encoding else {
            cu::bail!("expecting an Encoding attribute for DW_AT_encoding for primitive type at offset {offset}");
        };
        let byte_size = cu::check!(
            self.entry_unsigned_attr(entry, DW_AT_byte_size),
            "failed to get byte size for primitive type at offset {offset}"
        )?;
        let prim = match (encoding, byte_size) {
            (DW_ATE_boolean, 0x1) => Prim::Bool,
            (DW_ATE_unsigned, 0x1) => Prim::U8,
            (DW_ATE_unsigned_char, 0x1) => Prim::U8,
            (DW_ATE_signed, 0x1) => Prim::I8,
            (DW_ATE_signed_char, 0x1) => Prim::I8,

            (DW_ATE_unsigned, 0x2) => Prim::U16,
            (DW_ATE_signed, 0x2) => Prim::I16,
            (DW_ATE_UTF, 0x2) => Prim::U16,

            (DW_ATE_unsigned, 0x4) => Prim::U32,
            (DW_ATE_signed, 0x4) => Prim::I32,
            (DW_ATE_float, 0x4) => Prim::F32,

            (DW_ATE_unsigned, 0x8) => Prim::U64,
            (DW_ATE_signed, 0x8) => Prim::I64,
            (DW_ATE_float, 0x8) => Prim::F64,

            (DW_ATE_unsigned, 0x10) => Prim::U128,
            (DW_ATE_signed, 0x10) => Prim::I128,
            (DW_ATE_float, 0x10) => Prim::F128,

            _ => cu::bail!("unknown primitive type. encoding: {encoding}, byte size: {byte_size}"),
        };

        Ok(prim)
    }

    fn load_subroutine_types_from_entry(&self, entry: &Die<'i, '_, '_>) -> cu::Result<Vec<TyTree<Option<Goff>>>> {
        let offset = self.entry_goff(entry);
        let rettype_loff = cu::check!(
            self.entry_type_offset_opt(entry),
            "failed to read return type for subroutine-like type at {offset}"
        )?;
        let retty = match rettype_loff {
            // void
            None => TyTree::Base(None),
            Some(local_off) => TyTree::Base(Some(local_off.to_global(self.offset))),
        };
        let mut types = Vec::with_capacity(16);
        let mut found_void = false;
        types.push(retty);
        self.entry_for_each_child(entry, |child| {
            let entry = child.entry();
            if entry.tag() != DW_TAG_formal_parameter {
                cu::bail!("expecting all children of subroutine type to be DW_TAG_formal_parameter, at subroutine-like type at {offset}");
            }
            let local_off = cu::check!(self.entry_type_offset_opt(entry), "failed to read parameter type for subroutine-like type at {offset}")?;
            if let Some(local_off) = local_off {
                // skip void parameters
                types.push(TyTree::Base(Some(local_off.to_global(self.offset))));
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

    fn load_enum_type_from_entry(&self, entry: &Die<'i, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<Type0> {
        let offset = self.entry_goff(entry);
        let name = cu::check!(
            self.namespaced_entry_name_opt(entry, &ctx.namespaces),
            "failed to get enum name at {offset}"
        )?;
        let is_decl = cu::check!(
            self.entry_is_decl(entry),
            "failed to check if enum is declaration at {offset}"
        )?;
        if is_decl {
            let Some(name) = name else {
                cu::bail!("unexpected enum decl without name at {offset}");
            };
            return Ok(Type0::EnumDecl(name));
        }
        let size_or_base = match cu::check!(
            self.entry_type_offset_opt(entry),
            "failed to get enum base type at {offset}"
        )? {
            None => {
                // does not have base, check byte size
                let byte_size = cu::check!(
                    self.entry_unsigned_attr(entry, DW_AT_byte_size),
                    "failed to get enum byte size at {offset}"
                )?;
                if byte_size > u32::MAX as u64 {
                    cu::bail!("enum at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
                }
                Ok(byte_size as u32)
            }
            Some(local_off) => Err(local_off.to_global(self.offset)),
        };
        let mut enumerators = Vec::with_capacity(16);
        let result = self.entry_for_each_child(entry, |child| {
            let child_entry = child.entry();
            let child_offset = self.entry_goff(child_entry);
            match entry.tag() {
                DW_TAG_enumerator => {
                    let name = cu::check!(
                        self.entry_name(entry),
                        "failed to get enumerator name at {child_offset}"
                    )?;
                    let value = cu::check!(
                        self.entry_signed_attr(entry, DW_AT_const_value),
                        "failed to get enumerator value at {child_offset}"
                    )?;
                    enumerators.push((name.to_string(), value));
                }
                tag => {
                    cu::bail!("expecting all enum children entries to be DW_TAG_enumerator, but got {tag}")
                }
            }
            Ok(())
        });
        cu::check!(result, "failed to collect enumerators for enum type at {offset}")?;
        let ty = match name {
            None => Type0::EnumAnon(Type0Enum {
                size_or_base,
                enumerators,
            }),
            Some(name) => Type0::Enum(
                name,
                Type0Enum {
                    size_or_base,
                    enumerators,
                },
            ),
        };
        Ok(ty)
    }

    fn load_union_type_from_entry(&self, entry: &Die<'i, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<Type0> {
        let offset = self.entry_goff(entry);
        let name = cu::check!(
            self.namespaced_entry_name_opt(entry, &ctx.namespaces),
            "failed to get union name at {offset}"
        )?;
        let is_decl = cu::check!(
            self.entry_is_decl(entry),
            "failed to check if union is declaration at {offset}"
        )?;
        if is_decl {
            let Some(name) = name else {
                cu::bail!("unexpected union decl without name at {offset}");
            };
            return Ok(Type0::UnionDecl(name));
        }

        let byte_size = cu::check!(
            self.entry_unsigned_attr(entry, DW_AT_byte_size),
            "failed to get union byte size at {offset}"
        )?;
        if byte_size > u32::MAX as u64 {
            cu::bail!("union at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
        }
        let byte_size = byte_size as u32;

        let mut members = Vec::<(Option<&'_ str>, Goff)>::with_capacity(16);
        self.entry_for_each_child(entry, |child| {
            let child_entry = child.entry();
            let child_offset = self.entry_goff(child_entry);
            match child_entry.tag() {
                DW_TAG_member => {
                    let name = self.entry_name_opt(child_entry)?;
                    let ty_offset = cu::check!(
                        self.entry_type_offset_opt(child_entry),
                        "failed to get type for union member at {child_offset}"
                    )?;
                    let ty_offset = cu::check!(ty_offset, "unexpected void-typed union member at {child_offset}")?;
                    let ty_offset = ty_offset.to_global(self.offset);
                    // if type is duplicated, just ignore it
                    match members.iter_mut().find(|x| x.1 == ty_offset) {
                        None => members.push((name, ty_offset)),
                        Some((old_name, _)) => {
                            // update the name if we have it now
                            if old_name.is_none() {
                                *old_name = name;
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
                    if self.entry_vtable_index(child_entry)?.is_some() {
                        cu::bail!("unsupported virtual function in union at {child_offset}");
                    }
                }
                tag => {
                    cu::bail!("unexpected tag {tag} at {child_offset} while processing union");
                }
            }
            Ok(())
        })?;
        // assign names for anonymous members
        let mut anon_count = 0;
        let mut members2 = Vec::with_capacity(members.len());
        for (name, ty) in &members {
            if let Some(name) = name {
                members2.push((name.to_string(), *ty));
                continue;
            }
            anon_count += 1;
            let mut anon_name = format!("_anon{anon_count}");
            let mut attempt = 0;
            while members.iter().any(|(n, _)| n == &Some(anon_name.as_str())) {
                if attempt > 100 {
                    cu::bail!("failed to assign name to anonymous member for union type at {offset}");
                }
                attempt += 1;
                anon_name.push('_');
            }
            members2.push((anon_name, *ty));
        }

        let members = members2;
        let ty = match name {
            None => Type0::UnionAnon(Type0Union { members }),
            Some(name) => Type0::Union(name, Type0Union { members }),
        };
        Ok(ty)
    }

    fn load_struct_type_from_entry(&self, entry: &Die<'i, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<Type0> {
        let offset = self.entry_goff(entry);
        let name = cu::check!(
            self.namespaced_entry_name_opt(entry, &ctx.namespaces),
            "failed to get struct name at {offset}"
        )?;
        let is_decl = cu::check!(
            self.entry_is_decl(entry),
            "failed to check if struct is declaration at {offset}"
        )?;
        if is_decl {
            let Some(name) = name else {
                cu::bail!("unexpected struct decl without name at {offset}");
            };
            return Ok(Type0::StructDecl(name));
        }

        let byte_size = cu::check!(
            self.entry_unsigned_attr_opt(entry, DW_AT_byte_size),
            "failed to get struct byte size at {offset}"
        )?;
        let byte_size = byte_size.unwrap_or(0); // types can be 0 size?

        todo!()
    }

    /// Read an attribute of a DIE, expecting a type offset, allowing it to be missing
    fn entry_type_offset_attr(&self, entry: &Die<'i, '_, '_>, attr: DwAt) -> cu::Result<Loff> {
        let offset = self.entry_goff(entry);
        let type_value = cu::check!(entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let type_value = cu::check!(type_value, "missing {attr} for entry at offset {offset}")?;
        let type_offset = match type_value {
            AttributeValue::UnitRef(offset) => offset,
            _ => cu::bail!("expecting {attr} to be a unit ref at offset {offset}"),
        };
        Ok(type_offset.into())
    }

    fn entry_type_offset_opt(&self, entry: &Die<'i, '_, '_>) -> cu::Result<Option<Loff>> {
        self.entry_type_offset_attr_opt(entry, DW_AT_type)
    }

    /// Read an attribute of a DIE, expecting a type offset, allowing it to be missing
    fn entry_type_offset_attr_opt(&self, entry: &Die<'i, '_, '_>, attr: DwAt) -> cu::Result<Option<Loff>> {
        let offset = self.entry_goff(entry);
        let type_value = cu::check!(entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let Some(type_value) = type_value else {
            return Ok(None);
        };
        let type_offset = match type_value {
            AttributeValue::UnitRef(offset) => offset,
            _ => cu::bail!("expecting {attr} to be a unit ref at offset {offset}"),
        };
        Ok(Some(type_offset.into()))
    }

    /// Assert the entry at offset is DW_TAG_subrange_type, and get the DW_AT_count
    fn array_subrange_count(&self, local_off: Loff) -> cu::Result<Option<u32>> {
        let offset = local_off.to_global(self.offset);
        let mut tree = self.tree_at(local_off)?;
        let root = tree.root()?;
        let mut children = root.children();
        let subrange = cu::check!(children.next(), "failed to read child for entry at {offset}")?;
        let subrange = cu::check!(subrange, "expecting a child for entry at {offset}")?;
        let subrange = subrange.entry();
        if subrange.tag() != DW_TAG_subrange_type {
            cu::bail!("expecting DW_TAG_subrange_type for entry at {offset}");
        }
        let Some(count) = self.entry_unsigned_attr_opt(subrange, DW_AT_count)? else {
            return Ok(None);
        };
        if count > u32::MAX as u64 {
            cu::bail!("array length is too big: {count}. This is unlikely to be correct.");
        }
        Ok(Some(count as u32))
    }
}

struct LoadTypeCtx {
    pointer_type: Prim,
    types: GoffMap<Type0>,
    namespaces: GoffMap<Namespace>,
}
