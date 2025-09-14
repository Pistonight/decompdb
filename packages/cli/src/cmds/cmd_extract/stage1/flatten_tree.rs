use cu::pre::*;
use tyyaml::{Prim, Tree};

use crate::config::Config;

use super::pre::*;

pub fn flatten_tree(stage0: &mut Stage0) -> cu::Result<()> {
}

fn flatten_by_goff(goff: Goff, types: &GoffMap<Type0>, depth: usize) -> cu::Result<Option<Tree<Goff>>> {
    match types.get(&goff).unwrap() {
        Type0::Tree(tree) => {
            flatten_by_tree(tree, types, depth)
        }
        Type0::Alias(_) => {
            cu::bail!("stage1 flatten_tree step should not contain alias.");
        },
        _ => Ok(None)
    }
}

// return Some if changed
fn flatten_by_tree(
    tree: &Tree<Goff>,
    types: &GoffMap<Type0>,
    depth: usize,
) -> cu::Result<Option<Tree<Goff>>> {
    if depth > 1000 {
        cu::bail!("max flatten depth limit reached");
    }
    match tree {
        Tree::Base(inner) => {
            let inner_tree = cu::check!(
                flatten_by_goff(*inner, types, depth + 1),
                "failed to flatten base tree inner: {inner}"
            )?;
            if let Some(t) = inner_tree {
                return Ok(Some(t));
            }
        }
        Tree::Ptr(inner) => {
            let inner_flatten = cu::check!(
                flatten_by_tree(inner, types, depth + 1),
                    "failed to flatten pointer-to- {inner:#?}"
                )?;
                if let Some(inner_flatten) = inner_flatten {
                    return Ok(Some(Tree::Ptr(Box::new(inner_flatten))));
                }
            }
            Tree::Array(inner, len) => {
                let len = *len;
                let inner_flatten = cu::check!(
                    get_flattened_tree_recur(inner, buckets, depth + 1),
                    "failed to flatten array-of- {inner:#?}"
                )?;
                if let Some(inner_flatten) = inner_flatten {
                    // single element optimization
                    if len == 1 {
                        return Ok(Some(inner_flatten));
                    }
                    return Ok(Some(Tree::Array(Box::new(inner_flatten), len)));
                }
                // single element optimization
                if len == 1 {
                    return Ok(Some(inner.as_ref().clone()));
                }
            }
            Tree::Sub(inners) => {
                let mut new_vec = Vec::with_capacity(inners.len());
                for (i, inner) in inners.iter().enumerate() {
                    let inner_flatten = cu::check!(
                        get_flattened_tree_recur(inner, buckets, depth + 1),
                        "failed to flatten subroutine part {i}: {inner:#?}"
                    )?;
                    if let Some(inner_flatten) = inner_flatten {
                        if new_vec.is_empty() {
                            for old in inners.iter().take(i) {
                                new_vec.push(old.clone());
                            }
                        }
                        new_vec.push(inner_flatten);
                    }
                }
                if !new_vec.is_empty() {
                    return Ok(Some(Tree::Sub(new_vec)));
                }
            }
            Tree::Ptmd(this_, inner) => {
                    let inner_flatten = cu::check!(
                        get_flattened_tree_recur(inner, buckets, depth + 1),
                        "failed to flatten ptmd: {inner:#?}"
                    )?;
                if let Some(inner_flatten) = inner_flatten {
                    return Ok(Some(Tree::Ptmd(this_.clone(), Box::new(inner_flatten))));
                }
            }
            Tree::Ptmf(this_, inners) => {
                let mut new_vec = Vec::with_capacity(inners.len());
                for (i, inner) in inners.iter().enumerate() {
                    let inner_flatten = cu::check!(
                        get_flattened_tree_recur(inner, buckets, depth + 1),
                        "failed to flatten ptmf part {i}: {inner:#?}"
                    )?;
                    if let Some(inner_flatten) = inner_flatten {
                        if new_vec.is_empty() {
                            for old in inners.iter().take(i) {
                                new_vec.push(old.clone());
                            }
                        }
                        new_vec.push(inner_flatten);
                    }
                }
                if !new_vec.is_empty() {
                    return Ok(Some(Tree::Ptmf(this_.clone(), new_vec)));
                }
            }
        }

        Ok(None)
    }

fn optimize_flatten_trees(compiler: &mut TypeCompilerUnit, _: bool) -> cu::Result<bool> {
    let mut changed = false;
    for bucket in compiler.compiled.buckets() {
        let unit = &bucket.value;
        let Some(TypeUnitData::Tree(tree)) = &unit.data else {
            continue;
        };
        let bucket_key = bucket.canonical_key();
        match tree {
            Tree::Base(inner) => {
                let inner_key = *inner;
                if inner_key == bucket_key {
                    cu::bail!("unexpected self-referencing tree: {bucket_key}");
                }
                compiler.merges.push((inner_key, bucket_key));
                changed = true;
            }
            _ => {
                let new_tree = cu::check!(
                    get_flattened_tree_recur(tree, &compiler.compiled, 0),
                    "failed to flatten tree at {bucket_key}"
                )?;
                if let Some(new_tree) = new_tree {
                    let new_data = TypeUnitData::Tree(new_tree);
                    compiler.changes.insert(bucket_key, new_data);
                    changed = true;
                }
            }
        }
    }
    Ok(changed)
}
