use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use cu::pre::*;
use fxhash::FxHashSet;
use tyyaml::{Prim, Tree};

use crate::config::CfgExtract;

use super::pre::*;
use super::type_loader::{SpecialMember, TypePiece, TypePieceStruct, TypePieceUnion, TypePieceVtable};
use super::{BucketMap, BucketValue};

pub fn compile_types(name: String, types: GoffMap<TypePiece>, config: Arc<CfgExtract>) -> cu::Result<TypeCompilerUnit> {
    let mut compiler = TypeCompilerUnit::try_new(name, types, config)?;
    let result = compiler.compile();
    cu::check!(result, "failed to compile types for {}", compiler.name)?;
    Ok(compiler)
}

pub struct TypeCompilerUnit {
    pub name: String,
    pub config: Arc<CfgExtract>,
    pub pointer_type: Prim,
    pub types: GoffMap<TypePiece>,
    pub compiled: BucketMap<Goff, TypeUnit>,
    pub sizes: GoffMap<Option<u32>>,
    pub merges: Vec<(Goff, Goff)>, // from, to
    pub changes: GoffMap<TypeUnitData>,
}
impl TypeCompilerUnit {
    fn try_new(name: String, mut types: GoffMap<TypePiece>, config: Arc<CfgExtract>) -> cu::Result<Self> {
        // replace TyTree::Base with typedef
        {
            let mut replacement = Vec::with_capacity(types.len());
            for (goff, piece) in &types {
                if let TypePiece::Tree(Tree::Base(inner)) = piece {
                    let replaced = match inner {
                        None => TypePiece::Prim(Prim::Void),
                        Some(inner) => TypePiece::Alias(*inner),
                    };
                    replacement.push((*goff, replaced))
                }
            }
            for (goff, piece) in replacement {
                types.insert(goff, piece);
            }
        }

        let self_ = Self {
            name,
            pointer_type: config.pointer_type()?,
            config,
            types,
            compiled: Default::default(),
            sizes: Default::default(),
            merges: Default::default(),
            changes: Default::default(),
        };

        Ok(self_)
    }
    fn compile(&mut self) -> cu::Result<()> {
        // insert primitive types
        for p in Prim::iter() {
            let key = Goff::prim(p);
            self.types.insert(key, TypePiece::Prim(p));
        }

        // compile individual types, and resolve typedefs and aliases
        let goffs = self.types.keys().copied().collect::<BTreeSet<_>>();
        for k in goffs {
            self.compile_piece_to_unit(k)?;
        }
        self.apply_merges()?;

        self.optimize(false)
    }
    pub fn categorized_type_keys(&self) -> Categorized<BTreeSet<Goff>>{
        let mut out = Categorized::<BTreeSet<Goff>>::default();
        for bucket in self.compiled.buckets() {
            let unit = &bucket.value;
            let Some(data) = &unit.data else {
                continue;
            };
            let bucket_key = bucket.canonical_key();
            match data {
                TypeUnitData::Prim(_) => out.prims.insert(bucket_key),
                TypeUnitData::Enum(_) => out.enums.insert(bucket_key),
                TypeUnitData::EnumDecl => out.enum_decls.insert(bucket_key),
                TypeUnitData::Union(_) => out.unions.insert(bucket_key),
                TypeUnitData::UnionDecl => out.union_decls.insert(bucket_key),
                TypeUnitData::Struct(_) => out.structs.insert(bucket_key),
                TypeUnitData::StructDecl => out.struct_decls.insert(bucket_key),
                TypeUnitData::Tree(tree) => match tree {
                    Tree::Base(_) => false,
                    Tree::Array(_, _) => out.tree_arrays.insert(bucket_key),
                    Tree::Ptr(_) => out.tree_ptrs.insert(bucket_key),
                    Tree::Sub(_) => out.tree_subs.insert(bucket_key),
                    Tree::Ptmd(_, _) => out.tree_ptmds.insert(bucket_key),
                    Tree::Ptmf(_, _) => out.tree_ptmfs.insert(bucket_key),
                },
            };
        }
        out
    }
    pub fn type_stats(&self) -> Categorized<usize>{
        let mut out = Categorized::<usize>::default();
        for bucket in self.compiled.buckets() {
            let unit = &bucket.value;
            let Some(data) = &unit.data else {
                continue;
            };
            match data {
                TypeUnitData::Prim(_) => out.prims+=1,
                TypeUnitData::Enum(_) => out.enums+=1,
                TypeUnitData::EnumDecl => out.enum_decls+=1,
                TypeUnitData::Union(_) => out.unions+=1,
                TypeUnitData::UnionDecl => out.union_decls+=1,
                TypeUnitData::Struct(_) => out.structs+=1,
                TypeUnitData::StructDecl => out.struct_decls+=1,
                TypeUnitData::Tree(tree) => match tree {
                    Tree::Base(_) => {},
                    Tree::Array(_, _) => out.tree_arrays+=1,
                    Tree::Ptr(_) => out.tree_ptrs+=1,
                    Tree::Sub(_) => out.tree_subs+=1,
                    Tree::Ptmd(_, _) => out.tree_ptmds+=1,
                    Tree::Ptmf(_, _) => out.tree_ptmfs+=1,
                },
            };
        }
        out
    }
    pub fn optimize(&mut self, is_linking: bool) -> cu::Result<()> {
        cu::check!(self.canonicalize(), "failed to canonicalize types before optimization")?;
        cu::debug!("start optimizing, type count: {}", self.compiled.buckets().count());

        // optimize
        let mut changed = true;
        let mut pass = 1;
        while changed {
            changed = self.optimize_once(is_linking)?;
            pass += 1;
            cu::debug!("pass {pass} compiled type count: {}", self.compiled.buckets().count());
            cu::check!(self.compiled.check(), "bucket map is invalid after pass {pass}")?;
        }

        // ensure we did not mess up during optimization
        // and all enums, unions, and structs are sized
        self.sizes.clear();
        let mut expected_sizes = vec![];
        for bucket in self.compiled.buckets() {
            let Some(data) = &bucket.value.data else {
                continue;
            };
            let key = bucket.canonical_key();
            match data {
                TypeUnitData::Enum(data) => expected_sizes.push((key, data.byte_size)),
                TypeUnitData::Union(data) => expected_sizes.push((key, data.byte_size)),
                TypeUnitData::Struct(data) => expected_sizes.push((key, data.byte_size)),
                _ => {}
            }
        }
        for (key, expected_size) in expected_sizes {
            let actual_size = cu::check!(self.resolve_size(key), "size error after optimization for {key}")?;
            cu::ensure!(
                Some(expected_size) == actual_size,
                "size mismatch after optimization for {key}"
            );
        }
        // cu::trace!("final: {:#?}", self.compiled);
        Ok(())
    }
    pub fn optimize_once(&mut self, is_linking: bool) -> cu::Result<bool> {
        let mut changed = false;
        for optimize_fn in super::type_optimizer::OPTIMIZERS {
            self.merges.clear();
            self.changes.clear();
            let mut current_changed = true;
            while current_changed {
                current_changed = optimize_fn(self, is_linking)?;
                if current_changed && (!self.changes.is_empty() || !self.merges.is_empty()) {
                    self.apply_changes()?;
                    self.apply_merges()?;
                    cu::check!(self.canonicalize(), "failed to canonicalize types after applying optimization")?;
                    changed = true;
                }
            }
        }

        Ok(changed)
    }

    fn compile_piece_to_unit(&mut self, goff: Goff) -> cu::Result<()> {
        let piece = cu::check!(self.types.get(&goff), "unlinked type {goff}")?;
        let mut unit = TypeUnit::default();
        match piece {
            TypePiece::Prim(prim) => {
                unit.data = Some(TypeUnitData::Prim(*prim));
                self.merges.push((goff, Goff::prim(*prim)));
            }
            TypePiece::Typedef(name, inner) => {
                unit.typedef_names.insert(name.clone());
                self.merges.push((goff, *inner));
            }
            TypePiece::Alias(inner) => {
                self.merges.push((goff, *inner));
            }
            TypePiece::Enum(name, data) => {
                let name = name.clone();
                let enumerators = data.enumerators.clone();
                let size = cu::check!(
                    self.resolve_size(goff),
                    "failed to resolve size for enum {name} ({goff})"
                )?;
                let size = cu::check!(size, "unexpected unsized enum {name} ({goff})")?;
                unit.data = Some(TypeUnitData::Enum(TypeUnitEnum {
                    byte_size: size,
                    enumerators,
                }));
                unit.declared_names.insert(name.clone());
            }
            TypePiece::EnumAnon(data) => {
                let enumerators = data.enumerators.clone();
                let size = cu::check!(
                    self.resolve_size(goff),
                    "failed to resolve size for anonymous enum ({goff})"
                )?;
                let size = cu::check!(size, "unexpected unsized anonymous enum ({goff})")?;
                unit.data = Some(TypeUnitData::Enum(TypeUnitEnum {
                    byte_size: size,
                    enumerators,
                }));
            }
            TypePiece::EnumDecl(name) => {
                unit.data = Some(TypeUnitData::EnumDecl);
                unit.declared_names.insert(name.clone());
            }
            TypePiece::Union(name, data) => {
                let name = name.clone();
                let new_data = TypeUnitUnion::new(data);
                let size = cu::check!(
                    self.resolve_size(goff),
                    "failed to resolve size for union {name} ({goff})"
                )?;
                let _size = cu::check!(size, "unexpected unsized union {name} ({goff})")?;
                unit.data = Some(TypeUnitData::Union(new_data));
                unit.declared_names.insert(name);
            }
            TypePiece::UnionAnon(data) => {
                let new_data = TypeUnitUnion::new(data);
                let size = cu::check!(
                    self.resolve_size(goff),
                    "failed to resolve size for anonymous union ({goff})"
                )?;
                let _size = cu::check!(size, "unexpected unsized anonymous union ({goff})")?;
                unit.data = Some(TypeUnitData::Union(new_data));
            }
            TypePiece::UnionDecl(name) => {
                unit.data = Some(TypeUnitData::UnionDecl);
                unit.declared_names.insert(name.clone());
            }
            TypePiece::Struct(name, data) => {
                let name = name.clone();
                let vtable = cu::check!(
                    self.compile_vtable(goff),
                    "failed to compile vtable for struct {name} ({goff})"
                )?;
                let vtable = cu::check!(vtable, "unexpected None vtable for struct {name} ({goff})")?;
                let new_data = TypeUnitStruct::new(data, vtable);
                let size = cu::check!(
                    self.resolve_size(goff),
                    "failed to resolve size for struct {name} ({goff})"
                )?;
                let _size = cu::check!(size, "unexpected unsized struct {name} ({goff})")?;
                unit.data = Some(TypeUnitData::Struct(new_data));
                unit.declared_names.insert(name);
            }
            TypePiece::StructAnon(data) => {
                let vtable = cu::check!(
                    self.compile_vtable(goff),
                    "failed to compile vtable for anonymous struct ({goff})"
                )?;
                let vtable = cu::check!(vtable, "unexpected None vtable for anonymous struct ({goff})")?;
                let new_data = TypeUnitStruct::new(data, vtable);
                let size = cu::check!(
                    self.resolve_size(goff),
                    "failed to resolve size for anonymous struct ({goff})"
                )?;
                let _size = cu::check!(size, "unexpected unsized anonymous struct ({goff})")?;
                unit.data = Some(TypeUnitData::Struct(new_data));
            }
            TypePiece::StructDecl(name) => {
                unit.data = Some(TypeUnitData::StructDecl);
                unit.declared_names.insert(name.clone());
            }
            TypePiece::Tree(ty_tree) => {
                unit.data = Some(TypeUnitData::Tree(
                    ty_tree.clone().map(|x| x.unwrap_or(Goff::prim(Prim::Void))),
                ));
            }
        };
        cu::check!(
            self.compiled.insert_new(goff, unit),
            "failed to insert new bucket for type at {goff}"
        )?;
        Ok(())
    }

    pub fn canonicalize(&mut self) -> cu::Result<()> {
        // clone the key map to workaround the borrowing constrained
        // (honestly, might be more efficient this way)
        let canonical = self.compiled.canonical_key_map();
        for unit in self.compiled.buckets_mut() {
            if let Some(data) = &mut unit.data {
                data.canonicalize(&canonical);
            }
        }
        let mut unlinked_goffs = BTreeSet::new();
        for bucket in self.compiled.buckets() {
            if let Some(data) = &bucket.value.data {
                data.check_unlinked(&self.compiled, &mut unlinked_goffs);
            }
        }
        if !unlinked_goffs.is_empty() {
            cu::bail!("found {} unlinked types", unlinked_goffs.len())
        }
        Ok(())
    }

    fn apply_merges(&mut self) -> cu::Result<()> {
        if self.merges.is_empty() {
            return Ok(());
        }
        for (from, to) in self.merges.iter().copied() {
            cu::check!(self.compiled.merge(from, to), "failed to merge type {from} into {to}")?;
        }
        self.merges.clear();
        Ok(())
    }
    pub fn apply_merges_from<I: IntoIterator<Item=(Goff, Goff)>>(&mut self, merges: I) -> cu::Result<()> {
        for (from, to) in merges {
            cu::check!(self.compiled.merge(from, to), "failed to merge type {from} into {to}")?;
        }
        Ok(())
    }

    fn apply_changes(&mut self) -> cu::Result<()> {
        if self.changes.is_empty() {
            return Ok(());
        }
        for (key, data) in std::mem::take(&mut self.changes) {
            let unit = cu::check!(self.compiled.get_mut(key), "failed to get key {key} to apply changes")?;
            unit.data = Some(data);
        }
        Ok(())
    }

    pub fn resolve_size(&mut self, goff: Goff) -> cu::Result<Option<u32>> {
        if let Some(x) = self.sizes.get(&goff) {
            if let Some(x) = x
                && *x == u32::MAX
            {
                cu::bail!("failed to resolve size: infinite sized type {goff}");
            }
            return Ok(*x);
        }
        // mark the size as being resolved
        let piece = cu::check!(self.types.get(&goff), "unlinked type {goff}")?;
        let mut needs_update = false;
        let size = match piece {
            TypePiece::Prim(prim) => {
                let size = prim.byte_size();
                self.sizes.insert(goff, size);
                size
            }
            TypePiece::Typedef(_, inner) => {
                self.sizes.insert(goff, Some(u32::MAX));
                let inner = *inner;
                let size = cu::check!(
                    self.resolve_size(inner),
                    "failed to resolve size for typedef {goff} -> {inner}"
                )?;
                size
            }
            TypePiece::Alias(inner) => {
                self.sizes.insert(goff, Some(u32::MAX));
                let inner = *inner;
                let size = cu::check!(
                    self.resolve_size(inner),
                    "failed to resolve size for alias {goff} -> {inner}"
                )?;
                size
            }
            TypePiece::Enum(_, enum_data) | TypePiece::EnumAnon(enum_data) => match enum_data.byte_size_or_base {
                Ok(size) => Some(size),
                Err(inner) => {
                    needs_update = true;
                    self.sizes.insert(goff, Some(u32::MAX));
                    let size = cu::check!(
                        self.resolve_size(inner),
                        "failed to resolve size for enum base type {goff} -> {inner}"
                    )?;
                    cu::ensure!(size.is_some(), "unexpected unsized enum {goff}");
                    size
                }
            },
            TypePiece::EnumDecl(_) => {
                cu::bail!("encountered declaration while resolving size: enum decl {goff}");
            }
            TypePiece::Union(_, union_data) | TypePiece::UnionAnon(union_data) => {
                self.sizes.insert(goff, Some(u32::MAX));
                let self_size = union_data.byte_size;
                let inners = union_data.members.iter().map(|(_, inner)| *inner).collect::<Vec<_>>();
                // verify size is the same as largest member
                let mut max_size = 0;
                for inner in inners {
                    let size = cu::check!(
                        self.resolve_size(inner),
                        "failed to resolve size for union member type {goff} -> {inner}"
                    )?;
                    let size = cu::check!(size, "unexpected unsized union member {goff} -> {inner}")?;
                    max_size = size.max(max_size);
                }
                cu::ensure!(
                    max_size == self_size,
                    "unexpected union size mismatch: largest member size is 0x{max_size:x}, but self size is 0x{self_size:x}"
                );
                cu::ensure!(self_size != 0, "unexpected zero-sized union {goff}");
                Some(max_size)
            }
            TypePiece::UnionDecl(_) => {
                cu::bail!("encountered declaration while resolving size: union decl {goff}");
            }
            TypePiece::Struct(_, struct_data) | TypePiece::StructAnon(struct_data) => {
                let self_size = struct_data.byte_size;
                cu::ensure!(self_size != 0, "unexpected zero-sized struct {goff}");
                Some(self_size)
            }
            TypePiece::StructDecl(_) => {
                cu::bail!("encountered declaration while resolving size: struct decl {goff}");
            }
            TypePiece::Tree(ty_tree) => {
                self.sizes.insert(goff, Some(u32::MAX));
                let tree = ty_tree.clone();
                let size = cu::check!(
                    self.resolve_size_tree_recur(&tree),
                    "failed to resolve size for type tree: {goff}"
                )?;
                size
            }
        };

        // insert the actual size
        self.sizes.insert(goff, size);
        // update sizes for data
        if needs_update {
            let piece = cu::check!(self.types.get_mut(&goff), "unlinked type {goff}")?;
            match piece {
                TypePiece::Enum(_, data) | TypePiece::EnumAnon(data) => {
                    data.byte_size_or_base = Ok(size.expect("checked above enum is sized"));
                }
                _ => {}
            }
        }

        Ok(size)
    }

    fn resolve_size_tree_recur(&mut self, tree: &Tree<Option<Goff>>) -> cu::Result<Option<u32>> {
        match tree {
            Tree::Base(inner) => {
                let Some(inner) = inner else {
                    return Ok(None);
                };
                let inner = *inner;
                let size = cu::check!(
                    self.resolve_size(inner),
                    "failed to resolve size for inner type {inner}"
                )?;
                Ok(size)
            }
            Tree::Array(elemty, len) => {
                let elem_size = cu::check!(
                    self.resolve_size_tree_recur(elemty),
                    "failed to resolve element size for array type"
                )?;
                let elem_size = cu::check!(elem_size, "array element must be sized")?;
                Ok(Some(elem_size * (*len as u32)))
            }
            Tree::Ptr(_) => Ok(self.pointer_type.byte_size()),
            Tree::Sub(_) => Ok(None),
            Tree::Ptmd(_, _) => {
                let (ty, len) = self.config.ptmd_repr;
                Ok(ty.byte_size().map(|x| x * len))
            }
            Tree::Ptmf(_, _) => {
                let (ty, len) = self.config.ptmf_repr;
                Ok(ty.byte_size().map(|x| x * len))
            }
        }
    }
    fn compile_vtable(&self, goff: Goff) -> cu::Result<Option<TypeUnitStructVtable>> {
        let Some(result) = self.compile_vtable_recur(goff, &mut Default::default())? else {
            return Ok(None);
        };
        let vtable = cu::check!(result.finish(), "failed to finialize vtable for struct at {goff}")?;
        Ok(Some(vtable))
    }

    fn compile_vtable_recur(
        &self,
        goff: Goff,
        seen: &mut BTreeSet<Goff>,
    ) -> cu::Result<Option<TypeUnitStructVtableTemp>> {
        if !seen.insert(goff) {
            cu::bail!("circle detected while compiling vtable for {goff}: {seen:?}");
        }
        let piece = cu::check!(
            self.resolve_piece(goff),
            "failed to resolve piece while compiling vtable for struct at {goff}"
        )?;
        let struct_data = match piece {
            Some(TypePiece::Struct(_, data)) => data,
            Some(TypePiece::StructAnon(data)) => data,
            _ => return Ok(None),
        };

        // find the base member at offset 0 to inherit the vtable from
        let vtable = match struct_data.members.iter().find(|x| x.is_base() && x.offset == 0) {
            Some(base_member) => {
                // resolve the base vtable
                let base_vtable = cu::check!(
                    self.compile_vtable_recur(base_member.ty, seen),
                    "failed to compile base class vtable for struct at {goff} -> {}",
                    base_member.ty
                )?;
                let mut base_vtable = cu::check!(
                    base_vtable,
                    "unexpected non-struct base class {} for struct at {goff}",
                    base_member.ty
                )?;
                cu::check!(
                    base_vtable.make_derived(&struct_data.vtable),
                    "failed to create derived vtable from base class for struct at {goff}"
                )?;
                base_vtable
            }
            None => cu::check!(
                TypeUnitStructVtableTemp::try_new(&struct_data.vtable),
                "failed to create vtable from parsed data for struct at {goff}"
            )?,
        };

        Ok(Some(vtable))
    }

    /// Resolve to a non alias/typedef piece
    pub fn resolve_piece(&self, goff: Goff) -> cu::Result<Option<&TypePiece>> {
        self.resolve_piece_recur(goff, &mut Default::default())
    }

    fn resolve_piece_recur(&self, goff: Goff, resolving: &mut BTreeSet<Goff>) -> cu::Result<Option<&TypePiece>> {
        if !resolving.insert(goff) {
            cu::bail!("circular types detected while resolving {goff} ({resolving:?})");
        }
        let Some(piece) = self.types.get(&goff) else {
            return Ok(None);
        };
        let piece = match piece {
            TypePiece::Alias(goff) => cu::check!(
                self.resolve_piece_recur(*goff, resolving),
                "failed to resolve alias type piece {goff}"
            )?,
            TypePiece::Typedef(_, goff) => cu::check!(
                self.resolve_piece_recur(*goff, resolving),
                "failed to resolve typedef type piece {goff}"
            )?,
            other => Some(other),
        };
        Ok(piece)
    }

    pub fn resolve_complexity(&self, goff: Goff) -> cu::Result<usize> {
        let unit = cu::check!(self.compiled.get_unwrap(goff), "failed to get complexity for {goff}")?;

        let Some(data) = &unit.value.data else {
            return Ok(0);
        };

        let c = match data {
            TypeUnitData::Tree(tree) => {
                tree.complexity(|x| self.resolve_complexity(*x).expect("recur complexity failed"))
            }
            _ => 1,
        };

        Ok(c)
    }
}

#[derive(Default, Debug)]
pub struct Categorized<T> {
    pub prims: T,
    pub enums: T,
    pub enum_decls: T,
    pub unions: T,
    pub union_decls: T,
    pub structs: T,
    pub struct_decls: T,
    pub tree_arrays: T,
    pub tree_ptrs: T,
    pub tree_subs: T,
    pub tree_ptmds: T,
    pub tree_ptmfs: T,
}

#[derive(Debug, Default)]
pub struct TypeUnit {
    pub typedef_names: FxHashSet<String>,
    pub declared_names: FxHashSet<String>,
    pub data: Option<TypeUnitData>,
}

impl BucketValue for TypeUnit {
    fn absorb(&mut self, other: Self) -> cu::Result<()> {
        match &mut self.data {
            Some(type_data) => {
                if let Some(data) = other.data {
                    type_data.absorb(data);
                }
            }
            None => self.data = other.data,
        }
        self.declared_names.extend(other.declared_names);
        self.typedef_names.extend(other.typedef_names);
        // for name in other.typedef_names {
        //     if !self.declared_names.contains(&name) {
        //     }
        // }

        Ok(())
    }
}

impl TypeUnit {
    pub fn zip(&self, other: &Self, map: &BucketMap<Goff, TypeUnit>, merges: &mut BTreeSet<(Goff, Goff)>) -> cu::Result<bool> {
        if self.declared_names.intersection(&other.declared_names).next().is_none() {
            return Ok(false);
        }
        macro_rules! format_print_link_name {
            () => {{
                let v = self.declared_names.intersection(&other.declared_names).collect::<Vec<_>>();
                format!("linking is based on: {v:#?}, all names are {:#?} and {:#?}", self.declared_names, other.declared_names)
            }}
        }
        if let (Some(a), Some(b)) = (&self.data, &other.data) {
            match (a,b) {
                (TypeUnitData::Prim(a), TypeUnitData::Prim(b)) => {
                    cu::ensure!(a == b, "link error: primitives have overlapping names, but data are different.");
                }
                (TypeUnitData::Enum(a), TypeUnitData::Enum(b)) => {
                    cu::ensure!(a==b,"link error: enums have overlapping names, but data are different.");
                }
                (TypeUnitData::Union(a), TypeUnitData::Union(b)) => {
                    if a.byte_size != b.byte_size {
                        return Ok(false);
                    }
                    if a.members.len() != b.members.len() {
                        return Ok(false);
                    }
                    // sanity check
                    // cu::ensure!(a.byte_size == b.byte_size, "link error: unions being zipped have different byte sizes.");
                    // cu::ensure!(a.members.len() == b.members.len(), "link error: unions being zipped have different member sizes.");
                    for (member_a, member_b) in std::iter::zip(&a.members, &b.members) {
                        let (name_a, _) = member_a;
                        let (name_b, _) = member_b;
                        if name_a != name_b {
                            return Ok(false);
                        }
                        // cu::ensure!(name_a == name_b, "link error: unions being linked don't have the same member name.");
                    }
                    for (member_a, member_b) in std::iter::zip(&a.members, &b.members) {
                        let (_, ty_a) = member_a;
                        let (_, ty_b) = member_b;
                        // cu::ensure!(name_a == name_b, "link error: unions being linked don't have the same member name.");
                        cu::check!(do_zip(*ty_a, *ty_b, map, merges), "failed to zip union members")?;
                    }
                    return Ok(true);
                }
                (TypeUnitData::Struct(a), TypeUnitData::Struct(b)) => {
                    if a.byte_size != b.byte_size {
                        return Ok(false);
                    }
                    if a.vtable.entries.len() != b.vtable.entries.len() {
                        return Ok(false);
                    }
                    if a.members.len() != b.members.len() {
                        return Ok(false);
                    }
                    // cu::ensure!(a.byte_size == b.byte_size, "link error: structs being zipped have different byte sizes: {} and {}; {}", a.byte_size, b.byte_size, format_print_link_name!());
                    // cu::ensure!(a.vtable.entries.len() == b.vtable.entries.len(), "link error: structs being zipped have different vtable sizes.");
                    // cu::ensure!(a.members.len() == b.members.len(), "link error: structs being zipped have different member sizes");
                    for (ventry_a, ventry_b) in std::iter::zip(&a.vtable.entries, &b.vtable.entries) {
                        if ventry_a.name != ventry_b.name {
                            // ignore dtor differences for now
                            if ventry_a.name.starts_with('~') != ventry_b.name.starts_with('~') {
                                cu::bail!("link error: structs vtable entries being zipped have different names: {} and {}; {}", ventry_a.name, ventry_b.name, format_print_link_name!());
                            }
                        }
                        // this might be different, i don't know why
                        // cu::ensure!(ventry_a.is_from_base == ventry_b.is_from_base, "link error: structs vtable entries being zipped have different is_from_base");
                        if ventry_a.function_types.len() != ventry_b.function_types.len() {
                            return Ok(false);
                        }
                        // cu::ensure!(ventry_a.function_types.len() == ventry_b.function_types.len(), "link error: structs vtable entries being zipped have different function types");
                    }
                    for (a, b) in std::iter::zip(&a.members, &b.members) {
                        if a.offset != b.offset {
                            return Ok(false);
                        }
                        if a.name != b.name {
                            return Ok(false);
                        }
                        if a.special != b.special {
                            return Ok(false);
                        }
                        // cu::ensure!(a.offset == b.offset, "link error: structs being zipped don't have the same member offset.");
                        // cu::ensure!(a.name == b.name, "link error: structs being zipped don't have the same name.");
                        // cu::ensure!(a.special == b.special, "link error: structs being zipped don't have the same special type.");
                    }
                    // for (ventry_a, ventry_b) in std::iter::zip(&a.vtable.entries, &b.vtable.entries) {
                    //     for (func_a, func_b) in std::iter::zip(&ventry_a.function_types, &ventry_b.function_types) {
                    //         cu::check!(zip_trees(func_a, func_b, map, merges), "failed to zip function types for vtable")?
                    //     }
                    // }
                    for (a, b) in std::iter::zip(&a.members, &b.members) {
                        cu::check!(do_zip(a.ty, b.ty, map, merges), "failed to zip struct members")?;
                    }
                    return Ok(true);
                }
                // (TypeUnitData::Tree(a), TypeUnitData::Tree(b)) => {
                //     zip_trees(a, b, map, merges)?;
                // }
                _ => {}
            }
        }
        Ok(false)
    }
    pub fn names_overlap(&self, other: &Self) -> bool {
            let jn1 = &self.typedef_names;
            let jn2 = &self.declared_names;
            let kn1 = &other.typedef_names;
            let kn2 = &other.declared_names;
            if jn1.intersection(kn1).next().is_some() {
                return true;
            }
            if jn1.intersection(kn2).next().is_some() {
                return true;
            }
            if jn2.intersection(kn1).next().is_some() {
                return true;
            }
            if jn2.intersection(kn2).next().is_some() {
                return true;
            }
        false

    }
}

fn zip_trees(a: &Tree<Goff>, b: &Tree<Goff>, map: &BucketMap<Goff, TypeUnit>, merges: &mut BTreeSet<(Goff, Goff)>) -> cu::Result<()> {
    match (a, b) {
        (Tree::Base(a), Tree::Base(b)) => {
            do_zip(*a, *b, map, merges)?;
            return Ok(());
        }
        (Tree::Array(a, len_a), Tree::Array(b, len_b)) => {
            if len_a != len_b {
                return Ok(());
            }
            // cu::ensure!(len_a == len_b, "arrays being zipped don't have the same length");
            cu::check!(zip_trees(a, b, map, merges), "failed to zip array element type")?;
            return Ok(())
        },
        (Tree::Ptr(a), Tree::Ptr(b)) => {
            cu::check!(zip_trees(a, b, map, merges), "failed to zip pointee type")?;
            return Ok(())
        }
        (Tree::Sub(a), Tree::Sub(b)) => {
            if a.len() != b.len() {
                return Ok(())
            }
            // cu::ensure!(a.len() == b.len(), "subroutines being zipped don't have the same type length");
            for (ty_a, ty_b) in std::iter::zip(a, b) {
                cu::check!(zip_trees(ty_a, ty_b, map, merges), "failed to zip subroutine parts")?;
            }
            return Ok(())
        }
        (Tree::Ptmd(this_a, a), Tree::Ptmd(this_b, b)) => {
            cu::check!(do_zip(*this_a, *this_b, map, merges), "failed to zip ptmd this type")?;
            cu::check!(zip_trees(a, b, map, merges), "failed to zip ptmd pointee type")?;
            return Ok(())
        }
        (Tree::Ptmf(this_a, a), Tree::Ptmf(this_b, b)) => {
            cu::check!(do_zip(*this_a, *this_b, map, merges), "failed to zip ptmf this type")?;
            cu::ensure!(a.len() == b.len(), "ptmf being zipped don't have the same type length");
            for (ty_a, ty_b) in std::iter::zip(a, b) {
                cu::check!(zip_trees(ty_a, ty_b, map, merges), "failed to zip ptmf subroutine parts")?;
            }
            return Ok(())
        }
        // trees could have different shape in different units, which is OK
        _ => return Ok(())
    }
}

fn do_zip(a: Goff, b: Goff, map: &BucketMap<Goff, TypeUnit>, merges: &mut BTreeSet<(Goff, Goff)>) -> cu::Result<()> {
    if merges.insert((a,b)) {
        // let a_unit = &map.get_unwrap(a)?.value;
        // let b_unit = &map.get_unwrap(b)?.value;
        // cu::check!(a_unit.zip(b_unit, map, merges), "failed to zip types {a} and {b}")?;
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TypeUnitData {
    Prim(Prim),
    Enum(TypeUnitEnum),
    EnumDecl,
    Union(TypeUnitUnion),
    UnionDecl,
    Struct(TypeUnitStruct),
    StructDecl,
    Tree(Tree<Goff>),
}

impl TypeUnitData {
    fn absorb(&mut self, other: Self) {
        match self {
            Self::Tree(Tree::Base(_)) => {
                *self = other;
            }
            Self::Prim(_) | Self::Tree(_) => {
                // primitive types will not be overriden
            }
            Self::Enum(_) | Self::Union(_) | Self::Struct(_) => {
                // structured type does not get overriden unless
                // it's premitive or tree
                match other {
                    Self::Prim(_) | Self::Tree(_) => {
                        *self = other;
                    }
                    _ => {}
                }
            }
            _ => match other {
                Self::Tree(Tree::Base(_)) => {}
                _ => {
                    *self = other;
                }
            },
        }
    }

    fn canonicalize(&mut self, canonical: &BTreeMap<Goff, Goff>) {
        match self {
            TypeUnitData::Prim(_) | TypeUnitData::EnumDecl | TypeUnitData::UnionDecl | TypeUnitData::StructDecl => {}
            TypeUnitData::Tree(tree) => tree.for_each_mut(|goff| *goff = *canonical.get(goff).unwrap()),
            TypeUnitData::Enum(_) => {}
            TypeUnitData::Union(data) => {
                for (_, member_type) in &mut data.members {
                    *member_type = *canonical.get(member_type).unwrap();
                }
            }
            TypeUnitData::Struct(data) => {
                for ventry in &mut data.vtable.entries {
                    for function_type in &mut ventry.function_types {
                        function_type.for_each_mut(|x| *x = *canonical.get(x).unwrap());
                    }
                }
                for member in &mut data.members {
                    member.ty = *canonical.get(&member.ty).unwrap();
                }
            }
        }
    }

    fn check_unlinked(&self, types: &BucketMap<Goff, TypeUnit>, out: &mut BTreeSet<Goff>) {
        match self {
            TypeUnitData::Prim(_) | TypeUnitData::EnumDecl | TypeUnitData::UnionDecl | TypeUnitData::StructDecl => {}
            TypeUnitData::Tree(tree) => {
                tree.for_each(|goff| {
                    let goff = *goff;
                    if types.get(goff).is_none() {
                        cu::error!("found unlinked key {goff} in tree");
                        out.insert(goff);
                    }
                });
            }
            TypeUnitData::Enum(_) => {}
            TypeUnitData::Union(data) => {
                for (_, member_type) in &data.members {
                    let goff = *member_type;
                    if types.get(goff).is_none() {
                        cu::error!("found unlinked key {goff} in union");
                        out.insert(goff);
                    }
                }
            }
            TypeUnitData::Struct(data) => {
                for ventry in &data.vtable.entries {
                    for function_type in &ventry.function_types {
                        function_type.for_each(|goff| {
                            let goff = *goff;
                            if types.get(goff).is_none() {
                                cu::error!("found unlinked key {goff} in struct vtable");
                                out.insert(goff);
                            }
                        });
                    }
                }
                for member in &data.members {
                    let goff = member.ty;
                    if types.get(goff).is_none() {
                        cu::error!("found unlinked key {goff} in struct member");
                        out.insert(goff);
                    }
                }
            }
        }
    }

    /// Check if this type contains a recursive reference to goff
    pub fn is_recursive_to(&self, goff: Goff) -> bool {
        match self {
            TypeUnitData::Prim(prim) => Goff::prim(*prim) == goff,
            TypeUnitData::EnumDecl
            | TypeUnitData::UnionDecl
            | TypeUnitData::StructDecl
            | TypeUnitData::Enum(_)
            | TypeUnitData::Union(_)
            | TypeUnitData::Struct(_) => false,
            TypeUnitData::Tree(tree) => {
                let mut found = false;
                tree.for_each(|x| {
                    if *x == goff {
                        found = true
                    }
                });
                found
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeUnitEnum {
    /// Byte size of the enum
    pub byte_size: u32,
    /// Enumerators of the enum, in the order they appear in DWARF
    pub enumerators: Vec<(Arc<str>, i64)>,
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeUnitUnion {
    /// Byte size of the union
    pub byte_size: u32,
    /// Enumerators of the enum, in the order they appear in DWARF
    pub members: Vec<(Option<Arc<str>>, Goff)>,
}
impl TypeUnitUnion {
    pub fn new(data: &TypePieceUnion) -> Self {
        let byte_size = data.byte_size;
        let members = data
            .members
        .clone();
        Self { byte_size, members }
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeUnitStruct {
    /// Byte size of the struct
    pub byte_size: u32,
    /// Virtual function table
    pub vtable: TypeUnitStructVtable,
    /// Memebers of the struct
    pub members: Vec<TypeUnitStructMember>,
}
impl TypeUnitStruct {
    pub fn new(data: &TypePieceStruct, vtable: TypeUnitStructVtable) -> Self {
        let byte_size = data.byte_size;
        let members = data
            .members
            .iter()
            .map(|member| TypeUnitStructMember {
                offset: member.offset,
                name: member.name.clone(),
                ty: member.ty,
                special: member.special.clone(),
            })
            .collect();
        Self {
            byte_size,
            vtable,
            members,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeUnitStructMember {
    /// Offset of the member within the struct
    pub offset: usize,
    /// Name of the member. Could be None for anonymous union as member
    pub name: Option<Arc<str>>,
    /// Type of the member
    pub ty: Goff,
    /// Special-case member
    pub special: Option<SpecialMember>,
}

impl TypeUnitStructMember {
    pub fn is_base(&self) -> bool {
        matches!(self.special, Some(SpecialMember::Base))
    }
}

#[derive(Debug, Clone, Eq)]
pub struct TypeUnitStructVtable {
    pub entries: Vec<TypeUnitStructVentry>,
}

impl std::hash::Hash for TypeUnitStructVtable {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.entries.len().hash(state);
        for entry in &self.entries {
            entry.name.hash(state);
            entry.is_from_base.hash(state);
        }
    }
}

impl PartialEq for TypeUnitStructVtable {
    fn eq(&self, other: &Self) -> bool {
        if self.entries.len() != other.entries.len() {
            return false;
        }
        for (a, b) in std::iter::zip(self.entries.iter(), other.entries.iter()) {
            if a.name != b.name {
                return false;
            }

            // note we don't need to compare the function pointer type,
            // since that could vary per-vtable

            // do we need this? (have it for now to avoid incorrect merges)
            if a.is_from_base != b.is_from_base {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeUnitStructVtableTemp {
    entries: Vec<Option<TypeUnitStructVentry>>,
    virtual_dtor: Option<TypeUnitStructVentry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TypeUnitStructVentry {
    /// Name of the virtual function
    name: Arc<str>,
    /// Types to make up the subroutine type
    function_types: Vec<Tree<Goff>>,
    /// If the vtable entry is inherited from base class
    is_from_base: bool,
}

impl TypeUnitStructVtableTemp {
    pub fn try_new(piece_vtable: &TypePieceVtable) -> cu::Result<Self> {
        let mut self_ = Self::default();
        self_.make_derived(piece_vtable)?;
        Ok(self_)
    }
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Make self a derived class vtable, by adding info from the derived class
    pub fn make_derived(&mut self, piece_vtable: &TypePieceVtable) -> cu::Result<()> {
        // mark existing entries as from base
        for e in &mut self.entries {
            if let Some(e) = e {
                e.is_from_base = true;
            }
        }
        if let Some(e) = &mut self.virtual_dtor {
            e.is_from_base = true;
        }
        for piece_entry in &piece_vtable.entries {
            let name = piece_entry.name.clone();
            let function_types = piece_entry
                .function_types
                .iter()
                .map(|tree| tree.clone().map(|x| x.unwrap_or(Goff::prim(Prim::Void))))
                .collect();
            let new_entry = TypeUnitStructVentry {
                name,
                function_types,
                is_from_base: false,
            };
            if piece_entry.name.starts_with('~') {
                if let Some(e) = &self.virtual_dtor
                    && !e.is_from_base
                {
                    cu::bail!("conflicting derived vtable entry at virtual dtor");
                }
                self.virtual_dtor = Some(new_entry);
            } else {
                let entry = self.ensure(piece_entry.index);
                if let Some(e) = entry
                    && !e.is_from_base
                {
                    cu::bail!("conflicting derived vtable entry at index {}", piece_entry.index);
                }
                *entry = Some(new_entry);
            }
        }
        Ok(())
    }

    fn ensure(&mut self, idx: usize) -> &mut Option<TypeUnitStructVentry> {
        if idx >= self.entries.len() {
            self.entries.resize(idx + 1, None);
        }
        &mut self.entries[idx]
    }

    /// Finalize the vtable
    pub fn finish(mut self) -> cu::Result<TypeUnitStructVtable> {
        self.place_dtor();
        if let Some(i) = self.entries.iter().position(|x| x.is_none()) {
            cu::bail!("unexpected hole in vtable at index {i}");
        }
        let entries = self.entries.into_iter().map(|x| x.unwrap()).collect();
        Ok(TypeUnitStructVtable { entries })
    }

    /// Place virtual destructors
    ///
    /// Looks for the first 2 adjacent entries that each is either empty or a destructor,
    /// replace them with the given destructor info, and rename the first
    /// one D1 and the second D0
    fn place_dtor(&mut self) {
        let Some(mut vdtor) = self.virtual_dtor.take() else {
            return;
        };
        let mut placement = self.entries.len();
        for i in 0..self.entries.len() {
            let a_can_place = self.entries[i]
                .as_ref()
                .map(|x| x.name.starts_with('~'))
                .unwrap_or(true);
            let b_can_place = if i < self.entries.len() - 1 {
                self.entries[i + 1]
                    .as_ref()
                    .map(|x| x.name.starts_with('~'))
                    .unwrap_or(true)
            } else {
                true
            };
            if a_can_place && b_can_place {
                placement = i;
                break;
            }
        }
        let i = placement;
        // if we know this entry in the vtable is a dtor, then we don't
        // really care what the name is. So, we change the vtable entry
        // name to ~dtorD0 and ~dtorD1. This allows us to merge types
        // that are equivalent, except for the name of the dtor
        // (which depends on the type name)
        let mut d0 = vdtor.clone();
        d0.name = Arc::from(format!("{}D0", vdtor.name).as_str());
        self.ensure(i + 1).replace(d0);
        vdtor.name = Arc::from(format!("{}D1", vdtor.name).as_str());
        self.ensure(i).replace(vdtor);
    }
}
