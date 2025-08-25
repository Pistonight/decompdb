use std::sync::Arc;

use cu::pre::*;
use gimli::AttributeValue;
use tyyaml::{Prim, TyTree};

use crate::config::CfgExtract;

use super::pre::*;

impl<'i> CompUnit<'_, 'i> {
    pub fn load_types(&self, config: &CfgExtract, namespaces: GoffMap<Namespace>) -> cu::Result<GoffMap<TypePiece>> {
        let pointer_type = config.pointer_type()?;
        let mut ctx = LoadTypeCtx {
            pointer_type,
            types: Default::default(),
            namespaces,
        };
        cu::check!(self.load_types_root(&mut ctx), "failed to load types for {self}")?;
        cu::debug!("loaded {} types for {self}", ctx.types.len());

        // cu::trace!("types: {:#?}", ctx.types);

        Ok(ctx.types)
    }

    fn load_types_root(&self, ctx: &mut LoadTypeCtx) -> cu::Result<()> {
        let mut tree = self.tree()?;
        let root = tree.root()?;
        self.load_types_recur(root, ctx)?;
        Ok(())
    }

    fn load_types_recur(&self, node: Node<'i, '_, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<()> {
        let entry = node.entry();
        let tag = entry.tag();
        if is_type_tag(tag) {
            self.load_type_at(&node, ctx)?;
        }

        self.for_each_child(node, |child| self.load_types_recur(child, ctx))
    }

    /// Load the type at the node. The node must be a type
    fn load_type_at(&self, node: &Node<'i, '_, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<()> {
        let entry = node.entry();
        let offset = self.entry_goff(entry);

        if ctx.types.contains_key(&offset) {
            cu::bail!("unexpected already visited type entry at {offset}");
        }

        let ty = match entry.tag() {
            DW_TAG_unspecified_type => {
                let name = self
                    .entry_name(&entry)
                    .context("unspecified type entry must have a name")?;
                match name {
                    // std::nullptr_t
                    "decltype(nullptr)" => TypePiece::Prim(ctx.pointer_type),
                    _ => {
                        cu::bail!("unknown name for unspecified type: {name}, for entry at {offset}");
                    }
                }
            }
            DW_TAG_typedef => {
                match cu::check!(self.entry_type_offset_opt(entry), "failed to read typedef at {offset}")? {
                    // void
                    None => TypePiece::Prim(Prim::Void),
                    Some(local_off) => {
                        let typedef_name = cu::check!(
                            self.namespaced_entry_name(entry, &ctx.namespaces),
                            "failed to read name of the typedef at {offset}"
                        )?;
                        TypePiece::Typedef(typedef_name, self.goff(local_off))
                    }
                }
            }
            // T* or T&
            DW_TAG_pointer_type | DW_TAG_reference_type => {
                match cu::check!(
                    self.entry_type_offset_opt(entry),
                    "failed to read pointee type at {offset}"
                )? {
                    None => TypePiece::ptr(Goff::prim(Prim::Void)),
                    Some(local_off) => TypePiece::ptr(self.goff(local_off)),
                }
            }
            // modifiers that don't do anything..
            DW_TAG_const_type | DW_TAG_volatile_type | DW_TAG_restrict_type => {
                match cu::check!(
                    self.entry_type_offset_opt(entry),
                    "failed to read alias type at {offset}"
                )? {
                    None => TypePiece::Prim(Prim::Void),
                    Some(local_off) => TypePiece::Alias(self.goff(local_off)),
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
                    self.array_subrange_count(entry),
                    "failed to get array length for array type at {offset}"
                )?;
                match array_len {
                    // without count, just use ptr type
                    None => TypePiece::ptr(self.goff(local_off)),
                    Some(len) => TypePiece::array(self.goff(local_off), len),
                }
            }
            // Subroutine
            DW_TAG_subroutine_type => {
                let subroutine_types = cu::check!(
                    self.load_subroutine_types_from_entry(entry, false),
                    "failed to read subroutine type at {offset}"
                )?;
                TypePiece::Composite(TyTree::Sub(subroutine_types))
            }
            // PTMD/PTMF
            DW_TAG_ptr_to_member_type => {
                let this_ty_loff = cu::check!(
                    self.entry_type_offset_attr(entry, DW_AT_containing_type),
                    "failed to read this type for pointer-to-member type at {offset}"
                )?;
                let this_ty_goff = self.goff(this_ty_loff);
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
                            self.load_subroutine_types_from_entry(&pointee_entry, false),
                            "failed to read pointee subroutine type for pointer-to-member-function type at {offset}"
                        )?;
                        TypePiece::Composite(TyTree::ptmf(this_ty_goff, subroutine_types))
                    } else {
                        // PTMD
                        let pointee_ty_goff = self.goff(pointee_ty_loff);
                        TypePiece::Composite(TyTree::ptmd(this_ty_goff, TyTree::Base(Some(pointee_ty_goff))))
                    }
                } else {
                    // PTMD to void
                    TypePiece::Composite(TyTree::ptmd(this_ty_goff, TyTree::Base(None)))
                }
            }
            DW_TAG_base_type => TypePiece::Prim(self.load_base_type_from_entry(entry)?),
            DW_TAG_enumeration_type => self.load_enum_type_from_entry(entry, ctx)?,
            DW_TAG_union_type => self.load_union_type_from_entry(entry, ctx)?,
            DW_TAG_structure_type | DW_TAG_class_type => self.load_struct_type_from_entry(entry, ctx)?,
            tag => cu::bail!("unexpected tag {tag} while processing type at {offset}"),
        };
        ctx.types.insert(offset, ty);
        Ok(())
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

    fn load_subroutine_types_from_entry(
        &self,
        entry: &Die<'i, '_, '_>,
        allow_other_tags: bool,
    ) -> cu::Result<Vec<TyTree<Option<Goff>>>> {
        let offset = self.entry_goff(entry);
        let rettype_loff = cu::check!(
            self.entry_type_offset_opt(entry),
            "failed to read return type for subroutine-like type at {offset}"
        )?;
        let retty = match rettype_loff {
            // void
            None => TyTree::Base(None),
            Some(local_off) => TyTree::Base(Some(self.goff(local_off))),
        };
        let mut types = Vec::with_capacity(16);
        let mut found_void = false;
        types.push(retty);
        self.entry_for_each_child(entry, |child| {
            let entry = child.entry();
            if entry.tag() != DW_TAG_formal_parameter {
                if !allow_other_tags {
                    cu::bail!("expecting all children of subroutine type to be DW_TAG_formal_parameter, at subroutine-like type at {offset}");
                } else {
                    return Ok(());
                }
            }
            let local_off = cu::check!(self.entry_type_offset_opt(entry), "failed to read parameter type for subroutine-like type at {offset}")?;
            if let Some(local_off) = local_off {
                // skip void parameters
                types.push(TyTree::Base(Some(self.goff(local_off))));
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

    fn load_enum_type_from_entry(&self, entry: &Die<'i, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<TypePiece> {
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
            return Ok(TypePiece::EnumDecl(name));
        }
        let byte_size_or_base = match cu::check!(
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
            Some(local_off) => Err(self.goff(local_off)),
        };
        let mut enumerators = Vec::with_capacity(16);
        let result = self.entry_for_each_child(entry, |child| {
            let entry = child.entry();
            let offset = self.entry_goff(entry);
            match entry.tag() {
                DW_TAG_enumerator => {
                    let name = cu::check!(self.entry_name(entry), "failed to get enumerator name at {offset}")?;
                    let value = cu::check!(
                        self.entry_signed_attr(entry, DW_AT_const_value),
                        "failed to get enumerator value at {offset}"
                    )?;
                    enumerators.push((Arc::from(name), value));
                }
                tag => {
                    cu::bail!("expecting all enum children entries to be DW_TAG_enumerator, but got {tag}")
                }
            }
            Ok(())
        });
        cu::check!(result, "failed to collect enumerators for enum type at {offset}")?;
        let ty_data = Type0Enum {
            byte_size_or_base,
            enumerators,
        };
        let ty = match name {
            None => TypePiece::EnumAnon(ty_data),
            Some(name) => TypePiece::Enum(name, ty_data),
        };
        Ok(ty)
    }

    fn load_union_type_from_entry(&self, entry: &Die<'i, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<TypePiece> {
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
            return Ok(TypePiece::UnionDecl(name));
        }

        let byte_size = cu::check!(
            self.entry_unsigned_attr(entry, DW_AT_byte_size),
            "failed to get union byte size at {offset}"
        )?;
        if byte_size > u32::MAX as u64 {
            cu::bail!("union at {offset} is too big (byte_size={byte_size}). This is unlikely to be correct");
        }
        let byte_size = byte_size as u32;

        let mut members = Vec::<(Option<Arc<str>>, Goff)>::with_capacity(16);
        self.entry_for_each_child(entry, |child| {
            let entry = child.entry();
            let offset = self.entry_goff(entry);
            match entry.tag() {
                DW_TAG_member => {
                    let name = self.entry_name_opt(entry)?.map(Arc::from);
                    let type_loff = cu::check!(
                        self.entry_type_offset_opt(entry),
                        "failed to get type for union member at {offset}"
                    )?;
                    let type_loff = cu::check!(type_loff, "unexpected void-typed union member at {offset}")?;
                    let type_offset = self.goff(type_loff);
                    // if type is duplicated, just ignore it
                    match members.iter_mut().find(|x| x.1 == type_offset) {
                        None => members.push((name, type_offset)),
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
                    if self.entry_vtable_index(entry)?.is_some() {
                        cu::bail!("unsupported virtual function in union at {offset}");
                    }
                }
                tag => {
                    cu::bail!("unexpected tag {tag} at {offset} while processing union");
                }
            }
            Ok(())
        })?;

        let ty_data = Type0Union { byte_size, members };
        let ty = match name {
            None => TypePiece::UnionAnon(ty_data),
            Some(name) => TypePiece::Union(name, ty_data),
        };
        Ok(ty)
    }

    fn load_struct_type_from_entry(&self, entry: &Die<'i, '_, '_>, ctx: &mut LoadTypeCtx) -> cu::Result<TypePiece> {
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
            return Ok(TypePiece::StructDecl(name));
        }

        let byte_size = cu::check!(
            self.entry_unsigned_attr_opt(entry, DW_AT_byte_size),
            "failed to get struct byte size at {offset}"
        )?;
        let byte_size = byte_size.unwrap_or(0); // types can be 0 size?
        cu::ensure!(
            byte_size < u32::MAX as u64,
            "struct byte size is too big at entry {offset}. This is unlikely to be correct"
        );
        let byte_size = byte_size as u32;

        let mut vtable = TypePieceVtable::default();
        let mut members = Vec::<Type0StructMember>::with_capacity(16);

        let result = self.entry_for_each_child(entry, |child| {
            let entry = child.entry();
            let offset = self.entry_goff(entry);
            match entry.tag() {
                DW_TAG_member => {
                    if self.entry_is_extern(entry)? {
                        cu::bail!("unexpected extern member at {offset}");
                    }
                    // member might be anonymous union
                    let name = cu::check!(
                        self.entry_name_opt(entry),
                        "failed to get struct member name at {offset}"
                    )?;
                    let type_loff = cu::check!(
                        self.entry_type_offset_opt(entry),
                        "failed to get struct member type at {offset}"
                    )?;
                    let type_loff = cu::check!(type_loff, "unexpected void-typed struct member at {offset}")?;
                    let type_offset = self.goff(type_loff);
                    let member_offset = cu::check!(
                        self.entry_unsigned_attr(entry, DW_AT_data_member_location),
                        "failed to get struct member offset at {offset}"
                    )?;
                    cu::ensure!(
                        member_offset < u32::MAX as u64,
                        "member_offset is too big for member at {offset}. This is unlikely to be correct."
                    );

                    let mut member = Type0StructMember {
                        offset: member_offset as usize,
                        name: name.map(Arc::from),
                        ty: type_offset,
                        is_base: false,
                        bitfield_byte_size: None,
                    };
                    if cu::check!(
                        self.entry_unsigned_attr_opt(entry, DW_AT_bit_size),
                        "failed to check if struct member is bitfield at {offset}"
                    )?
                    .is_some()
                    {
                        let bitfield_byte_size = cu::check!(
                            self.entry_unsigned_attr(entry, DW_AT_byte_size),
                            "failed to get byte size of struct bitfield member at {offset}"
                        )?;
                        // bitfields are merged into one member of that type
                        // bitfield names are ignored for now
                        cu::ensure!(
                            bitfield_byte_size < u32::MAX as u64,
                            "bitfield_byte_size is too big for member at {offset}. This is unlikely to be correct."
                        );
                        member.bitfield_byte_size = Some(bitfield_byte_size as u32);
                        // can merge with last member if it's the same bitfield
                        if let Some(prev) = members.last_mut() {
                            if prev.offset == member.offset && prev.bitfield_byte_size.is_some() {
                                *prev = member;
                                return Ok(());
                            }
                        }
                    }
                    members.push(member);
                }
                DW_TAG_inheritance => {
                    let member_offset = cu::check!(
                        self.entry_unsigned_attr(entry, DW_AT_data_member_location),
                        "failed to get struct base class offset at {offset}"
                    )?;
                    cu::ensure!(
                        member_offset < u32::MAX as u64,
                        "member_offset is too big for base class at {offset}. This is unlikely to be correct."
                    );
                    let type_loff = cu::check!(
                        self.entry_type_offset_opt(entry),
                        "failed to get struct base class type at {offset}"
                    )?;
                    let type_loff = cu::check!(type_loff, "unexpected void-typed struct base class at {offset}")?;
                    let type_offset = self.goff(type_loff);
                    members.push(Type0StructMember {
                        offset: member_offset as usize,
                        name: None, // we will assign name to base members later
                        ty: type_offset,
                        is_base: true,
                        bitfield_byte_size: None,
                    });
                }
                DW_TAG_subprogram => {
                    let Some(velem) = cu::check!(
                        self.entry_vtable_index(entry),
                        "failed to get struct virtual function vtable index at {offset}"
                    )?
                    else {
                        // not virtual function, no need to process
                        return Ok(());
                    };
                    let name = cu::check!(
                        self.entry_name(entry),
                        "failed to get virtual function name at {offset}"
                    )?;
                    let name = Arc::from(name);
                    let function_types = cu::check!(
                        self.load_subroutine_types_from_entry(entry, false),
                        "failed to read virtual function data at {offset}"
                    )?;
                    let ventry = TypePieceVtableEntry {
                        index: velem,
                        name,
                        function_types,
                    };
                    vtable.entries.push(ventry);
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

        let ty_data = Type0Struct {
            byte_size,
            members,
            vtable,
        };
        let ty = match name {
            None => TypePiece::StructAnon(ty_data),
            Some(name) => TypePiece::Struct(name, ty_data),
        };
        Ok(ty)
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

    /// Assert the entry at offset is DW_TAG_array_type, and get the DW_AT_count of the DW_TAG_subrange_type
    fn array_subrange_count(&self, entry: &Die<'i, '_, '_>) -> cu::Result<Option<u32>> {
        let offset = self.entry_goff(entry);
        let mut count = None;
        let mut found_subrange = false;
        let result = self.entry_for_each_child(entry, |child| {
            let entry = child.entry();
            let offset = self.entry_goff(entry);
            match entry.tag() {
                DW_TAG_subrange_type => {
                    found_subrange = true;
                    let count_64 = cu::check!(
                        self.entry_unsigned_attr_opt(entry, DW_AT_count),
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

    /// Get if an entry is external
    pub fn entry_is_extern(&self, entry: &Die<'i, '_, '_>) -> cu::Result<bool> {
        self.entry_attr_flag(entry, DW_AT_external)
    }
}

struct LoadTypeCtx {
    pointer_type: Prim,
    types: GoffMap<TypePiece>,
    namespaces: GoffMap<Namespace>,
}

/// Definition of types parsed directly from DWARF
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypePiece {
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

impl TypePiece {
    fn ptr(offset: Goff) -> Self {
        TypePiece::Composite(TyTree::ptr(TyTree::Base(Some(offset))))
    }
    fn array(offset: Goff, len: u32) -> Self {
        TypePiece::Composite(TyTree::array(TyTree::Base(Some(offset)), len as usize))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Enum {
    /// Base type, used to determine the size
    pub byte_size_or_base: Result<u32, Goff>,
    /// Enumerators of the enum, in the order they appear in DWARF
    pub enumerators: Vec<(Arc<str>, i64)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Union {
    /// Byte size of the union (should be size of the largest member)
    pub byte_size: u32,
    /// (name, type) of the union members
    /// Name could be None for anonymous struct as member
    pub members: Vec<(Option<Arc<str>>, Goff)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Struct {
    /// Byte size of the struct
    pub byte_size: u32,
    pub vtable: TypePieceVtable,
    pub members: Vec<Type0StructMember>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0StructMember {
    /// Offset of the member within the struct
    pub offset: usize,
    /// Name of the member. Could be None for anonymous union as member
    pub name: Option<Arc<str>>,
    /// Type of the member
    pub ty: Goff,
    /// If the member is a base class
    pub is_base: bool,
    /// If the member is a bitfield, and the byte size
    pub bitfield_byte_size: Option<u32>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct TypePieceVtable {
    /// Entries for the vtable, might not include the functions from base class
    pub entries: Vec<TypePieceVtableEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypePieceVtableEntry {
    /// Index of this entry in the actual vtable.
    /// For dtors (name starts with ~), this is 0
    pub index: usize,
    /// Name of the virtual function
    pub name: Arc<str>,
    /// Types to make up the subroutine type
    pub function_types: Vec<TyTree<Option<Goff>>>,
}
