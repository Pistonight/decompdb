use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

use cu::pre::*;

use crate::config::{CompileCommand, Config};
use crate::demangler::Demangler;
use crate::symlist::SymbolList;

use super::namespace;
use super::pre::*;
use super::stage0_loader;

/// Extract database artifacts from DWARF info from an ELF file
#[derive(Debug, clap::Parser, AsRef)]
pub struct CmdExtract {
    #[clap(flatten)]
    #[as_ref]
    pub common: cu::cli::Flags,
}
pub fn run(config: Config) -> cu::Result<()> {
    cu::co::run(async move { run_internal(config).await })
}

async fn run_internal(config: Config) -> cu::Result<()> {
    let config = Arc::new(config);

    let build_bin = cu::check!(
        config.extract.build_command.first(),
        "missing extract.build-command in config"
    )?;
    {
        let (child, _, _) = Path::new(build_bin)
            .command()
            .args(config.extract.build_command.iter().skip(1))
            .current_dir(&config.paths.build_dir)
            .stderr(cu::pio::spinner("build project"))
            .stdio_null()
            .co_spawn()
            .await?;
        child.co_wait_nz().await?;
    }

    cu::fs::make_dir(&config.paths.extract_output)?;
    let compile_commands = {
        let cc = cu::fs::read_string(&config.paths.compdb)?;
        let cc_vec = json::parse::<Vec<CompileCommand>>(&cc)?;
        let mut cc_map = BTreeMap::new();
        for c in cc_vec {
            cc_map.insert(c.file.clone(), c);
        }
        cc_map
    };

    let bytes: Arc<[u8]> = cu::fs::read(&config.paths.elf)?.into();
    let dwarf = Dwarf::try_parse(bytes)?;

    let demangler = Arc::new(Demangler::try_new(
        config.paths.extract_output.join("demangler_cache.json"),
    )?);
    let mut symbol_list = SymbolList::default();
    symbol_list.load_data(&config.paths.data_csv)?;
    symbol_list
        .load_func(&config.paths.functions_csv, Arc::clone(&demangler))
        .await?;
    let symbol_list = Arc::new(symbol_list);
    cu::info!("loaded {} symbols from listing", symbol_list.len());
    if let Err(e) = demangler.flush_cache() {
        cu::warn!("failed to flush demangler cache: {e:?}");
    }

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
        let mut output = Vec::with_capacity(units.len());

        for unit in units {
            let config = Arc::clone(&config);
            let symbol_list = Arc::clone(&symbol_list);
            let handle = pool.spawn(async move {
                let ns = namespace::load_namespaces(&unit)?;
                let stage0 = stage0_loader::load(&unit, config, ns, symbol_list)?;
                cu::Ok(stage0)
            });
            handles.push(handle);
        }

        let mut set = cu::co::set(handles);
        let mut count = 0;
        let mut type_count = 0;
        while let Some(result) = set.next().await {
            let stage = result??;
            count += 1;
            type_count += stage.types.len();
            cu::progress!(&bar, count, "{}", stage.name);
            output.push(stage);
        }
        cu::progress_done!(&bar, "stage0: loaded {type_count} types");
        drop(bar);
        output.sort_unstable_by_key(|x| x.offset);
        output
    };

    let stage1 = {
        let bar = cu::progress_bar(stage0.len(), "stage0 -> stage1: reducing types");
        let mut handles = Vec::with_capacity(stage0.len());
        let pool = cu::co::pool(-1);
        let mut output = Vec::with_capacity(stage0.len());

        for stage in stage0 {
            let name = &stage.name;
            let command = cu::check!(compile_commands.get(name), "cannot find compile command for {name}")?;
            let command = command.clone();
            let handle = pool.spawn(async move { super::stage1::run_stage1(stage, &command).await });
            handles.push(handle);
        }

        let mut set = cu::co::set(handles);
        let mut count = 0;
        let mut type_count = 0;
        while let Some(result) = set.next().await {
            let stage = result??;
            count += 1;
            type_count += stage.types.len();
            cu::progress!(&bar, count, "{}", stage.name);
            output.push(stage);
        }
        cu::progress_done!(&bar, "stage1: reduced into {type_count} types");
        drop(bar);
        output.sort_unstable_by_key(|x| x.offset);
        output
    };

    let stage2 = super::stage2::run_stage2_serial(stage1).await?;

    cu::hint!("done");

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
