
use cu::pre::*;
use tyyaml::{Tree, Prim};

use super::type_structure::*;
use super::pre::*;
use super::bucket::GoffBuckets;

pub fn compile_stage0(mut stage0: TypeStage0) -> cu::Result<TypeStage1> {
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
        let Type0::Enum(_, data) = &type_data else {
            continue
        };
        resolve_size_stage0(*goff, &stage0, &mut sizes).context("unable to resolve enum sizes")?;
    }


    let mut buckets = GoffBuckets::default();
    let mut map = GoffMap::new();

    let mut merges = vec![];

    // convert type0 to type1
    for (goff, type_data) in stage0.types {
        let type1 = match type_data {
            Type0::Prim(p) => {
                if !goff.is_prim() {
                    merges.push((Goff::prim(p), goff));
                    None
                } else {
                    Some(Type1::Prim(p))
                }
            }
            Type0::Typedef(name, goff) => Some(Type1::Typedef(name, goff)),
            Type0::Enum(name, data) => {
                let size = *cu::check!(sizes.get(&goff), "unexpected: did not resolve enum size for enum {goff}")?;
                Some(Type1::Enum(name, Type1Enum { byte_size: size, enumerators: data.enumerators }))
            },
            Type0::EnumDecl(name) => Some(Type1::EnumDecl(name)),
            Type0::Union(name, data) => Some(Type1::Union(name, data)),
            Type0::UnionDecl(name) => Some(Type1::UnionDecl(name)),
            Type0::Struct(name, data) => Some(Type1::Struct(name, data)),
            Type0::StructDecl(name) => Some(Type1::StructDecl(name)),
            Type0::Tree(data) => Some(Type1::Tree(data)),
            Type0::Alias(target) => {
                merges.push((goff, target));
                    None
            }
        };
        map.insert(goff, type1);
    }
    for (k1, k2) in merges {
        buckets.merge
    }

    todo!()
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
            cu::ensure!(size != 0,"unexpected zero-sized enum: {goff}");
            cu::ensure!(size != UNSIZED, "unexpected unsized enum: {goff}");
            size
        },
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
                    "failed to resolve size for union member type {goff} -> {}", member.ty
                )?;
                max_size = size.max(max_size);
            }
            cu::ensure!(
                max_size == size,
                "unexpected union size mismatch: largest member size is 0x{max_size:x}, but self size is 0x{size:x}"
            );
            cu::ensure!(size != 0,"unexpected zero-sized union: {goff}");
            cu::ensure!(size != UNSIZED, "unexpected unsized union: {goff}");
            size
        }
        Type0::UnionDecl(_) => { 
            cu::bail!("encountered declaration while resolving size: union decl {goff}");
        }
        Type0::Struct(_, data) => {
            let size = data.byte_size;
            cu::ensure!(size != 0,"unexpected zero-sized struct: {goff}");
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
