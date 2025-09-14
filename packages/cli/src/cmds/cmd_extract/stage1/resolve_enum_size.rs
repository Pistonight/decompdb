use cu::pre::*;
use tyyaml::{Prim, Tree};

use crate::config::Config;

use super::pre::*;

// using "0" as speical value for resolving - this is valid because sizeof must return non-zero
const RESOLVING: u32 = 0;

pub fn resolve_enum_size(stage0: &mut Stage0) -> cu::Result<()> {
    let mut resolver = cu::check!(SizeResolver::try_new(&stage0.config), "failed to create size resolver")?;
    for (goff, data) in &stage0.types {
        let Type0::Enum(_, data) = data else {
            continue
        };
        if data.byte_size_or_base.is_err() {
            resolver.get_size(*goff, stage0)?;
        }
    }
    for (goff, data) in &mut stage0.types {
        let Type0::Enum(_, data) = data else {
            continue
        };
        if data.byte_size_or_base.is_ok() {
            continue;
        }
        let size = resolver.sizes.get(goff).unwrap();
        data.byte_size_or_base = Ok(*size);
    }
    Ok(())
}

struct SizeResolver {
    sizes: GoffMap<u32>
}

impl SizeResolver {
    pub fn try_new(config: &Config) -> cu::Result<Self> {
        let mut sizes = GoffMap::new();
        sizes.insert(Goff::pointer(), config.extract.pointer_size()?);
        sizes.insert(Goff::ptmd(), config.extract.ptmd_size()?);
        sizes.insert(Goff::ptmf(), config.extract.ptmf_size()?);
        for p in Prim::iter() {
            sizes.insert(Goff::prim(p), p.byte_size().unwrap_or(UNSIZED));
        }
        Ok(Self {
            sizes
        })
    }
    /// Resolve the size of the given type Goff, adds the size to the sizes map.
    pub fn get_size(&mut self, goff: Goff, stage0: &Stage0) -> cu::Result<u32> {
        if let Some(x) = self.sizes.get(&goff) {
            if *x == RESOLVING {
                cu::bail!("failed to resolve size: infinite sized type {goff}");
            }
            return Ok(*x);
        }
        let data = cu::check!(stage0.types.get(&goff), "unexpected unlinked type {goff}")?;
        let size = match data {
            Type0::Prim(prim) => {
                let size = prim.byte_size().unwrap_or(UNSIZED);
                self.sizes.insert(goff, size);
                size
            }
            Type0::Typedef(_, inner) => {
                self.sizes.insert(goff, RESOLVING);
                let inner = *inner;
                let size = cu::check!(
                    self.get_size(inner, stage0),
                    "failed to resolve size for typedef {goff} -> {inner}"
                )?;
                size
            }
            Type0::Alias(inner) => {
                self.sizes.insert(goff, RESOLVING);
                let inner = *inner;
                let size = cu::check!(
                    self.get_size(inner, stage0),
                    "failed to resolve size for alias {goff} -> {inner}"
                )?;
                size
            }
        Type0::Enum(_, data) => {
            let size = match data.byte_size_or_base {
                Ok(size) => size,
                Err(inner) => {
                    self.sizes.insert(goff, RESOLVING);
                    let size = cu::check!(
                        self.get_size(inner, stage0),
                        "failed to resolve size for enum base type {goff} -> {inner}"
                    )?;
                    size
                }
            };
            cu::ensure!(size != 0, "unexpected zero-sized enum: {goff}");
            cu::ensure!(size != UNSIZED, "unexpected unsized enum: {goff}");
            size
        }
        Type0::EnumDecl(_, _) => {
            cu::bail!("encountered declaration while resolving size: enum decl {goff}");
        }
        Type0::Union(_, data) => {
            // verify size is the same as largest member
            self.sizes.insert(goff, RESOLVING);
            let size = data.byte_size;
            let mut max_size = 0;
            for member in &data.members {
                let size = cu::check!(
                    self.get_tree_size(&member.ty, stage0),
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
        Type0::UnionDecl(_, _) => {
            cu::bail!("encountered declaration while resolving size: union decl {goff}");
        }
        Type0::Struct(_, data) => {
            let size = data.byte_size;
            cu::ensure!(size != 0, "unexpected zero-sized struct: {goff}");
            cu::ensure!(size != UNSIZED, "unexpected unsized struct: {goff}");
            size
        }
        Type0::StructDecl(_, _) => {
            cu::bail!("encountered declaration while resolving size: struct decl {goff}");
        }
        Type0::Tree(ty_tree) => {
            self.sizes.insert(goff, RESOLVING);
            let size = cu::check!(
                self.get_tree_size(ty_tree, stage0),
                "failed to resolve size for type tree: {goff}"
            )?;
            size
        }
    };

    // insert the actual size
    cu::ensure!(size != RESOLVING, "unexpected invalid size for type {goff}");
    self.sizes.insert(goff, size);
    Ok(size)
}
    pub fn get_tree_size(&mut self, tree: &Tree<Goff>, stage0: &Stage0) -> cu::Result<u32> {
        match tree {
            Tree::Base(inner) => {
                let inner = *inner;
                cu::check!(
                    self.get_size(inner, stage0),
                    "failed to resolve size for type {inner}"
                )
            }
            Tree::Array(elemty, len) => {
                let len = *len;
                cu::ensure!(len != 0, "unexpected 0-length array");
                let elem_size = cu::check!(
                    self.get_tree_size(elemty, stage0),
                    "failed to resolve array element size"
                )?;
                cu::ensure!(elem_size != UNSIZED, "array element must be sized");
                Ok(elem_size * (len as u32))
            }
            Tree::Ptr(_) => Ok(*self.sizes.get(&Goff::pointer()).unwrap()),
            Tree::Sub(_) => Ok(UNSIZED),
            Tree::Ptmd(_, _) => Ok(*self.sizes.get(&Goff::ptmd()).unwrap()),
            Tree::Ptmf(_, _) => Ok(*self.sizes.get(&Goff::ptmf()).unwrap()),
        }
    }
}

