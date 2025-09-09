use cu::pre::*;
use tyyaml::Tree;

use super::pre::*;

/// Eliminate typedef to composite (tree) types
pub fn clean_typedef(stage0: &mut Stage0) -> cu::Result<()> {
    let mut to_clean = vec![];
    let mut cache = GoffMap::default();
    for (goff, data) in &stage0.types {
        let Type0::Typedef(_, inner) = data else {
            continue;
        };
        let is_tree = cu::check!(is_tree(*inner, stage0, &mut cache), "failed to check is_tree for {goff}")?;
        if !is_tree {
            continue;
        }
        to_clean.push((*goff, *inner));
    }

    for (goff, inner) in to_clean {
        let data = stage0.types.get_mut(&goff).unwrap();
        *data = Type0::Alias(inner);
    }

    Ok(())
}

pub fn is_tree(goff: Goff, stage0: &Stage0, 
    cache: &mut GoffMap<bool>,
) -> cu::Result<bool> {
    if let Some(is_tree) = cache.get(&goff) {
        return Ok(*is_tree);
    }

    let data = cu::check!(stage0.types.get(&goff), "unexpected unlinked type {goff}")?;
    let is_tree = match data {
        Type0::Typedef(_, inner) => {
            is_tree(*inner, stage0, cache)?
        }
        Type0::Alias(inner) => {
            is_tree(*inner, stage0, cache)?
        }
        Type0::Tree(Tree::Base(inner)) => {
            is_tree(*inner, stage0, cache)?
        }
        Type0::Tree(_) => true,
        _ => false
    };
    cache.insert(goff, is_tree);
    Ok(is_tree)
}
