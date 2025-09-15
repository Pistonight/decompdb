
use cu::pre::*;

use crate::config::CompileCommand;

use super::pre::*;
use super::super::bucket::GoffBuckets;

pub async fn run_stage1(mut stage: Stage0, command: &CompileCommand) -> cu::Result<Stage1> {
    cu::check!(super::resolve_enum_sizes(&mut stage), "stage1: resolve_enum_sizes failed")?;
    cu::check!(super::clean_typedefs(&mut stage), "stage1: clean_typedefs failed")?;
    cu::check!(super::flatten_trees(&mut stage), "stage1: flatten_trees failed")?;
    let mut names = cu::check!(super::parse_names(&stage, command).await, "stage1: parse_names failed")?;

    // build stage1 types
    let mut types = GoffMap::default();
    let mut dupes = vec![];
    for (k, t) in stage.types {
        match t {
            Type0::Prim(prim) => {
                types.insert(k, Type1::Prim(prim));
            }
            Type0::Typedef(_, goff) => {
                match names.remove(&k) {
                    Some(name) => {
                        types.insert(k, Type1::Typedef(name, goff));
                    }
                    None => {
                        // cannot resolve the name: this means this typedef
                        // might be a private one (`using` inside a class or function).
                        // So in this case, we delete the name
                        dupes.push((k, goff));
                    }
                }
            }
            Type0::EnumDecl(_, _) => {
                let name = cu::check!(names.remove(&k), "was not able to resolve enum decl name for {k}")?;
                types.insert(k, Type1::EnumDecl(name));
            }
            Type0::Enum(name, data) => {
                let Ok(byte_size) = data.byte_size_or_base else { 
                    cu::bail!("unexpected did not resolve enum byte size: {k}");
                };
                let enumerators = data.enumerators;
                types.insert(k, Type1::Enum(name, Type1Enum { byte_size, enumerators }));
            }
            Type0::UnionDecl(_, _) => {
                let name = cu::check!(
                    names.remove(&k),
                    "was not able to resolve union decl name for {k}"
                )?;
                types.insert(k, Type1::UnionDecl(name));
            }
            Type0::Union(name, data) => {
                types.insert(k, Type1::Union(name, data));
            }
            Type0::StructDecl(_, _) => {
                let name = cu::check!(
                    names.remove(&k),
                    "was not able to resolve struct decl name for {k}"
                )?;
                types.insert(k, Type1::StructDecl(name));
            }
            Type0::Struct(name, data) => {
                types.insert(k, Type1::Struct(name, data));
            }
            Type0::Tree(tree) => {
                cu::bail!("unexpected leftover tree after stage1: {k}: {tree:#?}")
            }
            Type0::Alias(goff) => {
                cu::bail!("unexpected leftover alias after stage1: {k} -> {goff}")
            }
        }
    }
    for (k, g) in dupes {
        types.insert(k, types.get(&g).unwrap().clone());
    }
    let deduped = super::super::deduper::dedupe(
        types,
        GoffBuckets::default(),
        &mut stage.symbols,
        |data, buckets| {
            data.map_goff(|k| Ok(buckets.primary_fallback(k)))
        }
    );
    let deduped = cu::check!(deduped, "clean_typedefs: deduped failed")?;
    if stage.name.contains("PauseMenuDataMgr") {
        cu::print!("{:#?}", deduped);
    }

    Ok(Stage1 { 
        offset: stage.offset, 
        name: stage.name, 
        types: deduped, 
        config: stage.config,
        symbols: stage.symbols
    })
}
