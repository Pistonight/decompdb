use cu::pre::*;
use tyyaml::Tree;

use super::super::bucket::GoffBuckets;
use super::super::deduper;
use super::pre::*;

/// Eliminate and merge:
/// - Typedef to a composite type
/// - Typedef to a primitive type
/// - Aliases
/// - Tree::Base
pub fn clean_typedefs(stage: &mut Stage0) -> cu::Result<()> {
    let mut buckets = GoffBuckets::default();
    let mut new_map = GoffMap::default();
    let mut is_tree_cache = GoffMap::default();
    for k1 in stage.types.keys().copied() {
        let (k2, data) = cu::check!(
            resolve_alias(k1, &stage.types, &mut is_tree_cache, 0),
            "resolve_alias failed for {k1}"
        )?;
        cu::check!(buckets.merge(k1, k2), "clean_typedefs: failed to merge {k1} and {k2}")?;
        new_map.insert(k2, data);
    }

    // it should no longer contain Tree::Base or Alias
    if cfg!(debug_assertions) {
        for (k, data) in &new_map {
            match data {
                Type0::Alias(goff) => {
                    cu::bail!("unexpected alias after cleaning: {k} -> {goff}");
                }
                Type0::Tree(Tree::Base(goff)) => {
                    cu::bail!("unexpected tree-base alias after cleaning: {k} -> {goff}");
                }
                _ => {}
            }
        }
    }

    let deduped = deduper::dedupe(new_map, buckets, &mut stage.symbols, 
        Some(&mut stage.ns),
        |data, buckets| {
        data.map_goff(|k| Ok(buckets.primary_fallback(k)))
    });
    let deduped = cu::check!(deduped, "clean_typedefs: dedupe failed")?;

    stage.types = deduped;

    Ok(())
}

// resolve the alias, if it is an alias, return the alias (goff, data) resolved to.
pub fn resolve_alias<'a>(
    goff: Goff,
    types: &'a GoffMap<Type0>,
    is_tree_cache: &mut GoffMap<bool>,
    depth: usize,
) -> cu::Result<(Goff, Type0)> {
    cu::ensure!(depth <= 1000, "depth limit exceeded in resolve_alias");

    let data = types.get(&goff).unwrap();
    match data {
        Type0::Alias(inner) => {
            cu::check!(
                resolve_alias(*inner, types, is_tree_cache, depth + 1),
                "failed to resolve alias {goff} -> {inner}"
            )
        }
        Type0::Tree(Tree::Base(inner)) => {
            cu::check!(
                resolve_alias(*inner, types, is_tree_cache, depth + 1),
                "failed to resolve tree-base alias {goff} -> {inner}"
            )
        }
        Type0::Typedef(name, inner) => {
            let resolved = cu::check!(
                resolve_alias(*inner, types, is_tree_cache, depth + 1),
                "failed to resolve typedef alias {goff} -> {inner}"
            )?;
            let should_remove_name = is_tree(*inner, types, is_tree_cache) || is_primitive(*inner, types);
            if should_remove_name {
                // change typedef to an alias (eliminate the name) if the inner type is a tree
                Ok(resolved)
            } else if *inner != resolved.0 {
                // if inner is another alias, make the typedef point to it directly
                Ok((goff, Type0::Typedef(name.clone(), resolved.0)))
            } else {
                // inner is non-alias, retain the typedef
                Ok((goff, data.clone()))
            }
        }
        other => Ok((goff, other.clone())),
    }
}

pub fn is_tree(goff: Goff, types: &GoffMap<Type0>, cache: &mut GoffMap<bool>) -> bool {
    if let Some(is_tree) = cache.get(&goff) {
        return *is_tree;
    }

    let data = types.get(&goff).unwrap();
    let is_tree = match data {
        Type0::Typedef(_, inner) => is_tree(*inner, types, cache),
        Type0::Alias(inner) => is_tree(*inner, types, cache),
        Type0::Tree(Tree::Base(inner)) => is_tree(*inner, types, cache),
        Type0::Tree(_) => true,
        _ => false,
    };
    cache.insert(goff, is_tree);
    is_tree
}

pub fn is_primitive(goff: Goff, types: &GoffMap<Type0>) -> bool {
    if goff.is_prim() {
        return true;
    }
    let data = types.get(&goff).unwrap();
    let is_prim = match data {
        Type0::Typedef(_, inner) => is_primitive(*inner, types),
        Type0::Alias(inner) => is_primitive(*inner, types),
        Type0::Tree(Tree::Base(inner)) => is_primitive(*inner, types),
        Type0::Prim(_) => true,
        _ => false,
    };
    is_prim
}
