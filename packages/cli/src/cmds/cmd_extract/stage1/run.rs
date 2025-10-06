use cu::pre::*;

use crate::config::CompileCommand;

use super::super::bucket::GoffBuckets;
use super::pre::*;

pub async fn run_stage1(mut stage: Stage0, command: &CompileCommand) -> cu::Result<Stage1> {
    cu::check!(
        super::resolve_enum_sizes(&mut stage),
        "stage1: resolve_enum_sizes failed"
    )?;
    cu::check!(super::clean_typedefs(&mut stage), "stage1: clean_typedefs failed")?;
    cu::check!(super::flatten_trees(&mut stage), "stage1: flatten_trees failed")?;
    let mut names = cu::check!(super::parse_names(&stage, command).await, "stage1: parse_names failed")?;

    // GC types to ensure trees are all GC-ed
    // note GC must be after parsing names, since some types could be referenced
    // only in namespaces, and we only get it after parsing the string type name
    let mut marked = GoffSet::default();
    for symbol in stage.symbols.values() {
        symbol.mark(&mut marked);
    }
    // also mark the parsed name
    for name in names.values() {
        name.mark(&mut marked);
    }
    super::super::garbage_collector::mark_and_sweep(marked, &mut stage.types, |t, k, marked| {
        t.mark(k, marked);
    });
    if cfg!(debug_assertions) {
        for (k, t) in &stage.types {
            if let Type0::Tree(t) = t {
                cu::bail!("unexpected tree type not gc'ed: k={k}, type={t:#?}");
            }
        }
    }

    // build stage1 types
    let mut types = GoffMap::default();
    let mut typedef_names = GoffMap::<Vec<_>>::default();
    let mut dupes = vec![];
    for (k, t) in &stage.types {
        match t {
            Type0::Prim(prim) => {
                types.insert(*k, Type1::Prim(*prim));
            }
            Type0::Typedef(_, goff) => {
                let mut target_goff = *goff;
                loop {
                    match stage.types.get(&target_goff).unwrap() {
                        Type0::Typedef(_, x) => {
                            target_goff = *x;
                        }
                        _ => break,
                    }
                }
                dupes.push((*k, target_goff));
                match names.remove(&k) {
                    Some(name) => {
                        typedef_names.entry(target_goff).or_default().push(name);
                    }
                    None => {
                        // cannot resolve the name: this means this typedef
                        // might be a private one (`using` inside a class or function).
                        // So in this case, we ignore the name
                    }
                }
            }
            Type0::EnumDecl(_, _) => {
                let name = cu::check!(names.remove(k), "was not able to resolve enum decl name for {k}")?;
                types.insert(*k, Type1::EnumDecl(name, vec![]));
            }
            Type0::Enum(name, data) => {
                let Ok(byte_size) = data.byte_size_or_base else {
                    cu::bail!("unexpected did not resolve enum byte size: {k}");
                };
                let enumerators = data.enumerators.clone();
                types.insert(
                    *k,
                    Type1::Enum(name.clone(), Type1Enum { byte_size, enumerators }, vec![]),
                );
            }
            Type0::UnionDecl(_, _) => {
                let name = cu::check!(names.remove(&k), "was not able to resolve union decl name for {k}")?;
                types.insert(*k, Type1::UnionDecl(name, vec![]));
            }
            Type0::Union(name, data) => {
                types.insert(*k, Type1::Union(name.clone(), data.clone(), vec![]));
            }
            Type0::StructDecl(_, _) => {
                let name = cu::check!(names.remove(k), "was not able to resolve struct decl name for {k}")?;
                types.insert(*k, Type1::StructDecl(name, vec![]));
            }
            Type0::Struct(name, data) => {
                types.insert(*k, Type1::Struct(name.clone(), data.clone(), vec![]));
            }
            Type0::Tree(tree) => {
                cu::bail!("unexpected leftover tree after stage1: {k}: {tree:#?}")
            }
            Type0::Alias(goff) => {
                cu::bail!("unexpected leftover alias after stage1: {k} -> {goff}")
            }
        }
    }

    // fill in typedef names
    for (k, names) in typedef_names {
        match types.get_mut(&k).unwrap() {
            Type1::Prim(_) => {
                cu::bail!("unexpected leftover typedef to prim: {k}");
            }
            Type1::Enum(_, _, n)
            | Type1::EnumDecl(_, n)
            | Type1::Union(_, _, n)
            | Type1::UnionDecl(_, n)
            | Type1::Struct(_, _, n)
            | Type1::StructDecl(_, n) => *n = names,
        }
    }
    for (k, g) in dupes {
        types.insert(k, types.get(&g).unwrap().clone());
    }
    let deduped = super::super::deduper::dedupe(types, GoffBuckets::default(), &mut stage.symbols, None, |data, buckets| {
        data.map_goff(|k| Ok(buckets.primary_fallback(k)))
    });
    let deduped = cu::check!(deduped, "stage1: final deduped failed")?;

    Ok(Stage1 {
        offset: stage.offset,
        name: stage.name,
        types: deduped,
        config: stage.config,
        symbols: stage.symbols,
    })
}
