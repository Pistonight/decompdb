#![allow(non_upper_case_globals)]

use std::sync::Arc;

use cu::pre::*;

use crate::config::Config;

mod dwarf_parse;

mod bucket;
pub use bucket::*;
mod namespace;
// pub use namespace::*;
mod name_comparator;
use name_comparator::*;

mod type0_compiler;
mod type_loader;
// mod type_compiler;
// mod type_optimizer;
// mod type_linker;
mod type_structure;

mod pre {
    pub use super::dwarf_parse::*;
    pub use super::namespace::Namespace;
    pub use gimli::constants::*;
}
use pre::*;

// mod compilation_unit;
// pub use compilation_unit::*;
/// Extract database artifacts from DWARF info from an ELF file
#[derive(Debug, clap::Parser, AsRef)]
pub struct CmdExtract {
    #[clap(flatten)]
    #[as_ref]
    pub common: cu::cli::Flags,
}
pub fn run(config: Config) -> cu::Result<()> {
    cu::co::run(async move {
        // we will leak memory if panic, but otherwise this is safe
        // let bytes: Box<[u8]> = cu::fs::read(&config.paths.elf)?.into();
        // let bytes = Box::into_raw(bytes);
        // let dwarf = parse_dwarf(unsafe { &*bytes })?;
        // let dwarf = Box::into_raw(Box::new(dwarf));
        //
        // let result = run_internal(config, unsafe { &*dwarf }).await;
        //
        // let dwarf = unsafe { Box::from_raw(dwarf) };
        // drop(dwarf);
        // let bytes = unsafe { Box::from_raw(bytes) };
        // drop(bytes);
        //
        // result
        run_internal(config).await
    })
}

async fn run_internal(config: Config) -> cu::Result<()> {
    let config = Arc::new(config);

    let bytes: Arc<[u8]> = cu::fs::read(&config.paths.elf)?.into();
    let dwarf = Dwarf::try_parse(bytes)?;

    let mut units = Vec::new();
    let mut iter = Dwarf::iter_units(&dwarf);
    while let Some(unit) = iter.next_unit().context("error while collecting units from DWARF")? {
        units.push(unit);
    }

    cu::info!("found {} compilation units", units.len());

    let mut stage1s = {
        let bar = cu::progress_bar(units.len(), "loading types");
        let mut handles = Vec::with_capacity(units.len());
        let pool = cu::co::pool(-1);
        let mut stage1s = Vec::with_capacity(units.len());

        for unit in units {
            // let unit = Arc::clone(unit);
            let config = Arc::clone(&config);
            let handle = pool.spawn(async move {
                let ns = namespace::load_namespaces(&unit)?;
                let stage0 = type_loader::load_types(&unit, config, ns)?;
                let stage1 = cu::check!(
                    type0_compiler::compile_stage0(stage0),
                    "failed to compile stage0 for {unit}"
                )?;
                cu::Ok(stage1)
            });
            handles.push(handle);
        }

        let mut set = cu::co::set(handles);
        let mut count = 0;
        let mut type_count = 0;
        while let Some(result) = set.next().await {
            let stage1 = result??;
            count += 1;
            type_count += stage1.types.len();
            cu::progress!(&bar, count, "{}", stage1.name);
            stage1s.push(stage1);
        }
        drop(bar);
        cu::info!("stage1: loaded {type_count} types");
        stage1s
    };
    //
    // compilers.sort_by(|a, b| a.name.cmp(&b.name));

    // let linked_types = type_linker::link_types(compilers).await.context("type linking failed")?;
    //
    // let keys = linked_types.categorized_type_keys();
    // let enums = keys.enums.into_iter().map(|k| {
    //     (k, &linked_types.compiled.get_unwrap(k).unwrap().value)
    // }).collect::<Vec<_>>();
    // let unions = keys.unions.into_iter().map(|k| {
    //     (k, &linked_types.compiled.get_unwrap(k).unwrap().value)
    // }).collect::<Vec<_>>();

    // cu::info!("ENUMS{enums:#?}");
    // cu::info!("UNIONS{unions:#?}");

    // let unit = units.iter().find(|x| x.name.contains("PauseMenuDataMgr")).unwrap();

    // cu::trace!("types: {types:#?}");

    // cu::debug!("compiled type count: {}", compiled_types.buckets().count());

    Ok(())
}
