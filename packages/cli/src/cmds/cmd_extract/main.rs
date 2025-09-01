use std::sync::Arc;

use cu::pre::*;

use crate::config::Config;

use super::namespace;
use super::pre::*;
use super::stage0_clang_parse::CompileCommand;
use super::stage0_loader;
use super::type0_compiler;

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
    cu::fs::make_dir(&config.paths.extract_output)?;
    let compile_commands = cu::fs::read_string(&config.paths.compdb)?;
    let compile_commands = json::parse::<Vec<CompileCommand>>(&compile_commands)?;

    let bytes: Arc<[u8]> = cu::fs::read(&config.paths.elf)?.into();
    let dwarf = Dwarf::try_parse(bytes)?;

    let units = {
        let mut units = Vec::new();
        let mut iter = Dwarf::iter_units(&dwarf);
        while let Some(unit) = iter.next_unit().context("error while collecting units from DWARF")? {
            units.push(unit);
        }
        cu::info!("found {} compilation units", units.len());
        units
    };

    let stage0 = {
        let bar = cu::progress_bar(units.len(), "stage0: loading types");
        let mut handles = Vec::with_capacity(units.len());
        let pool = cu::co::pool(-1);
        let mut stage1s = Vec::with_capacity(units.len());

        for unit in units {
            // let unit = Arc::clone(unit);
            let config = Arc::clone(&config);
            let handle = pool.spawn(async move {
                let ns = namespace::load_namespaces(&unit)?;
                let stage0 = stage0_loader::load_types(&unit, config, ns)?;
                cu::Ok(stage0)
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
        cu::progress_done!(&bar, "stage0: loaded {type_count} types");
        drop(bar);
        stage1s.sort_unstable_by_key(|x| x.offset);
        stage1s
    };

    // let stage2 = {
    //     let len = stage1.len();
    //     let mut iter = stage1.into_iter();
    //     let stage2 = cu::check!(iter.next(), "no compilation units to link!!!")?;
    //     let mut stage2 = type0_compiler::into_stage2(stage2);
    //
    //     let bar = cu::progress_bar(len, "stage1 -> stage2: linking types across units");
    //     for (i, stage1) in iter.enumerate() {
    //         let name = stage1.name.clone();
    //         stage2 = cu::check!(
    //             type0_compiler::link_stage1(stage2, stage1),
    //             "failed to link types with {name}"
    //         )?;
    //         cu::progress!(&bar, i + 1, "{name}");
    //     }
    //     stage2
    // };

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
