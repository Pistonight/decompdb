use cu::pre::*;
use tyyaml::Prim;

use super::type_compiler::TypeCompilerUnit;
use super::pre::*;

pub async fn link_types(types: Vec<TypeCompilerUnit>) -> cu::Result<TypeCompilerUnit> {
    cu::info!("linking {} type units", types.len());
    let mut next = Vec::with_capacity(types.len() / 2 + 1);
    let mut current = types;

    let bar = cu::progress_unbounded("linking types");

    let pool = cu::co::pool(0);
    loop {
        let mut handles = Vec::with_capacity(current.len() / 2);
        loop {
            let mut unit1 = match current.pop() {
                Some(x) => x,
                None => break,
            };
            let unit2 = match current.pop() {
                Some(x) => x,
                None => {
                    next.push(unit1);
                    break;
                }
            };
            let handle = pool.spawn(async move {
                let unit2_name = unit2.name.clone();
                cu::check!(unit1.link_with(unit2), "failed to link types from {} with {unit2_name}", unit1.name)?;
                cu::Ok(unit1)
            });
            handles.push(handle);
        }

        if !handles.is_empty() {
            let mut set = cu::co::set(handles);
            while let Some(result) = set.next().await {
                let unit = result.flatten()?;
                next.push(unit);
            }
        }

        if next.len() == 1 {
            return Ok(next.into_iter().next().unwrap().link_done());
        }
        std::mem::swap(&mut current, &mut next);
        next.clear();

    }
}

impl TypeCompilerUnit {
    /// Link the compiled types from 2 compiler units
    pub fn link_with(&mut self, mut other: Self) -> cu::Result<()> {
        self.types.extend(other.types);
        self.sizes.clear();
        // since both compilers have a copy of primitive types, we need to dedupe it
        for p in Prim::iter() {
            let goff = Goff::prim(p);
            if let Some(bucket) = other.compiled.remove(goff) {
                for k in bucket.other_keys {
                    self.compiled.add_key(goff, k)?;
                }
            }
        }
        self.compiled.extend(other.compiled).context("failed to merge 2 type compiler units")?;
        cu::ensure!(self.merges.is_empty(), "cannot link with type compiler unit with leftover merges!");
        cu::ensure!(other.merges.is_empty(), "cannot link with type compiler unit with leftover merges!");
        cu::ensure!(self.changes.is_empty(), "cannot link with type compiler unit with leftover changes!");
        cu::ensure!(other.changes.is_empty(), "cannot link with type compiler unit with leftover changes!");

        self.optimize()?;
        if self.name != "<linked>" {
            self.name = "<linked>".to_string();
        }
        Ok(())
    }
    pub fn link_done(mut self) -> Self {
        // remove names for pritimive types
        for p in Prim::iter() {
            let goff = Goff::prim(p);
            if let Some(data) = self.compiled.get_mut(goff) {
                data.typedef_names.clear();
                data.declared_names.clear();
            }
        }
        self
    }
}
