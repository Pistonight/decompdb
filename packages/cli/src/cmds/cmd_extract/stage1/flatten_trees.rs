use cu::pre::*;
use tyyaml::Tree;

use super::pre::*;
use super::super::bucket::GoffBuckets;
use super::super::deduper;

pub fn flatten_trees(stage: &mut Stage0) -> cu::Result<()> {
    let mut changes = GoffMap::default();
    for goff in stage.types.keys().copied() {
        let flattened = cu::check!(flatten_by_goff(goff, &stage.types, 0), "failed to flatten type {goff}")?;
        if let Some(flattened) = flattened {
            changes.insert(goff, Type0::Tree(flattened));
        }
    }
    stage.types.extend(changes);

    // flatten trees in type data
    let mut changes = vec![];
    for (goff, data) in &stage.types {
        match data {
            Type0::Union(name, data) => {
                let mut copy = data.clone();
                let mut changed = false;
                for targ in &mut copy.template_args {
                    let TemplateArg::Type(tree) = targ else  {
                        continue;
                    };
                    let flattened = cu::check!(
                        flatten_by_tree(tree, &stage.types, 0), "failed to flatten union template arg for {goff}"
                    )?;
                    if let Some(flattened) = flattened {
                        *tree = flattened;
                        changed = true;
                    }
                }
                for member in &mut copy.members {
                    let flattened = cu::check!(
                        flatten_by_tree(&member.ty, &stage.types, 0), "failed to flatten union member for {goff}"
                    )?;
                    if let Some(flattened) = flattened {
                        member.ty = flattened;
                        changed = true;
                    }
                }
                if changed {
                    changes.push((*goff, Type0::Union(name.clone(), copy)));
                }
            },
            Type0::Struct(name, data) => {
                let mut copy = data.clone();
                let mut changed = false;
                for targ in &mut copy.template_args {
                    let TemplateArg::Type(tree) = targ else  {
                        continue;
                    };
                    let flattened = cu::check!(
                        flatten_by_tree(tree, &stage.types, 0), "failed to flatten struct template arg for {goff}"
                    )?;
                    if let Some(flattened) = flattened {
                        *tree = flattened;
                        changed = true;
                    }
                }
                for (_, ventry) in &mut copy.vtable {
                    for tree in &mut ventry.function_types {
                        let flattened = cu::check!(
                            flatten_by_tree(tree, &stage.types, 0), "failed to flatten struct vtable function type for {goff}"
                        )?;
                        if let Some(flattened) = flattened {
                            *tree = flattened;
                            changed = true;
                        }
                    }
                }
                for member in &mut copy.members {
                    let flattened = cu::check!(
                        flatten_by_tree(&member.ty, &stage.types, 0), "failed to flatten struct member for {goff}"
                    )?;
                    if let Some(flattened) = flattened {
                        member.ty = flattened;
                        changed = true;
                    }
                }
                if changed {
                    changes.push((*goff, Type0::Struct(name.clone(), copy)));
                }
            }
            _ => {}
        }
    }
    stage.types.extend(changes);

    // flatten types in symbols
    let mut changes = vec![];
    for (name, symbol) in &stage.symbols {
        let mut copy = symbol.clone();
        let mut changed = false;
        let flattened = cu::check!(
            flatten_by_tree(&symbol.ty, &stage.types, 0),
            "failed to flatten type for symbol '{name}'"
        )?;
        if let Some(flattened) = flattened {
            copy.ty = flattened;
            changed = true;
        }
        for targ in &mut copy.template_args {
            let TemplateArg::Type(tree) = targ else  {
                continue;
            };
            let flattened = cu::check!(
                flatten_by_tree(tree, &stage.types, 0), "failed to flatten symbol template arg for symbol '{name}'"
            )?;
            if let Some(flattened) = flattened {
                *tree = flattened;
                changed = true;
            }
        }
        if changed {
            changes.push((name.clone(), copy));
        }
    }
    stage.symbols.extend(changes);

    // GC types to ensure trees are all GC-ed
    let mut marked = GoffSet::default();
    for symbol in stage.symbols.values() {
        symbol.mark(&mut marked);
    }
    super::super::garbage_collector::mark_and_sweep(marked, &mut stage.types, Type0::mark);
    if cfg!(debug_assertions) {
        for (k, t) in &stage.types {
            if let Type0::Tree(t) = t {
                cu::bail!("unexpected tree type not gc'ed: k={k}, type={t:#?}");
            }
        }
    }

    let deduped = deduper::dedupe(
        std::mem::take(&mut stage.types),
        GoffBuckets::default(),
        &mut stage.symbols,
        |data, buckets| {
            data.map_goff(|k| Ok(buckets.primary_fallback(k)))
        }
    );
    let deduped = cu::check!(deduped, "flatten_trees: deduped failed")?;

    stage.types = deduped;
    Ok(())
}

fn flatten_by_goff(goff: Goff, types: &GoffMap<Type0>, depth: usize) -> cu::Result<Option<Tree<Goff>>> {
    match cu::check!(types.get(&goff), "unexpected unlinked type {goff}")? {
        Type0::Tree(tree) => {
            // we always need to flatten goff to a tree,
            // so here we always return Some
            match flatten_by_tree(tree, types, depth)? {
                Some(x) => Ok(Some(x)),
                None => Ok(Some(tree.clone()))
            }
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
                flatten_by_tree(inner, types, depth + 1),
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
                    flatten_by_tree(inner, types, depth + 1),
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
                flatten_by_tree(inner, types, depth + 1),
                "failed to flatten ptmd: {inner:#?}"
            )?;
            if let Some(inner_flatten) = inner_flatten {
                match inner_flatten {
                    Tree::Sub(types) => {
                        return Ok(Some(Tree::Ptmf(this_.clone(), types)));
                    }
                    other => {
                        return Ok(Some(Tree::Ptmd(this_.clone(), Box::new(other))));
                    }
                }
            }
        }
        Tree::Ptmf(this_, inners) => {
            let mut new_vec = Vec::with_capacity(inners.len());
            for (i, inner) in inners.iter().enumerate() {
                let inner_flatten = cu::check!(
                    flatten_by_tree(inner, types, depth + 1),
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

