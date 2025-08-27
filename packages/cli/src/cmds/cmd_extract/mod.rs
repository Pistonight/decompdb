#![allow(non_upper_case_globals)]

use std::sync::Arc;

use cu::pre::*;

use crate::config::Config;

mod bucket;
pub use bucket::*;
mod compilation_unit;
pub use compilation_unit::*;
mod dwarf_types;
use dwarf_types::*;
mod namespace;
pub use namespace::*;
mod name_comparator;
use name_comparator::*;

mod type_compiler;
mod type_loader;
mod type_optimizer;
mod type_linker;

mod pre {
    pub use super::dwarf_types::*;
    pub use super::{CompUnit, Namespace};
    pub use gimli::constants::*;
}

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
        let bytes: Box<[u8]> = cu::fs::read(&config.paths.elf)?.into();
        let bytes = Box::into_raw(bytes);
        let dwarf = parse_dwarf(unsafe { &*bytes })?;
        let dwarf = Box::into_raw(Box::new(dwarf));

        let result = run_internal(config, unsafe { &*dwarf }).await;

        let dwarf = unsafe { Box::from_raw(dwarf) };
        drop(dwarf);
        let bytes = unsafe { Box::from_raw(bytes) };
        drop(bytes);

        result
    })
}

async fn run_internal(config: Config, dwarf: &'static pre::Dwarf<'static>) -> cu::Result<()> {
    // let bytes = cu::fs::read(&config.paths.elf)?;
    // let bytes: &'static mut [u8] = bytes.leak();
    // let dwarf = parse_dwarf(bytes)?;
    // let dwarf = Box::leak(Box::new(dwarf));

    let extract_config = Arc::new(config.extract);
    let name_comparator = NameComparator::new(Arc::clone(&extract_config));

    let debug_info = dwarf.debug_info;

    let mut iter = debug_info.units();
    let mut units = Vec::new();

    while let Some(unit_header) = iter.next().context("error reading DWARF compilation unit header")? {
        let unit_ctx = CompUnit::new(unit_header, dwarf)?;
        units.push(Arc::new(unit_ctx));
    }

    cu::info!("found {} compilation units", units.len());

    let mut compilers = {
        let bar = cu::progress_bar(units.len(), "compiling types");
        let mut handles = Vec::with_capacity(units.len());
        let pool = cu::co::pool(-1);

        for unit in &units {
            let unit = Arc::clone(unit);
            let extract_config = Arc::clone(&extract_config);
            let handle = pool.spawn(async move {
                let ns = unit.load_namespaces()?;
                let types = unit.load_types(Arc::clone(&extract_config), ns)?;
                cu::debug!("type count: {}", types.len());

                let name = unit.name.to_string();
                let compiler = type_compiler::compile_types(
                    name.clone(),
                    types, 
                    extract_config
                )?;
                cu::Ok((compiler, name))
            });
            handles.push(handle);
        }

        let mut set = cu::co::set(handles);
        let mut compilers = Vec::with_capacity(units.len());
        let mut num_enums = 0;
        let mut num_unions = 0;
        let mut num_structs = 0;
        let mut num_enum_decls = 0;
        let mut num_union_decls = 0;
        let mut num_struct_decls = 0;
        let mut count = 0;
        while let Some(compiler) = set.next().await {
            let (compiler, name) = compiler??;
            count += 1;
            for bucket in compiler.compiled.buckets() {
                // type_count += 1;
                let Some(data) = &bucket.value.data else {
                    continue;
                };
                match data {
                    type_compiler::TypeUnitData::Prim(_) => {}
                    type_compiler::TypeUnitData::Enum(_) => num_enums += 1,
                    type_compiler::TypeUnitData::EnumDecl => num_enum_decls += 1,
                    type_compiler::TypeUnitData::Union(_) => num_unions += 1,
                    type_compiler::TypeUnitData::UnionDecl => num_union_decls += 1,
                    type_compiler::TypeUnitData::Struct(_) => num_structs += 1,
                    type_compiler::TypeUnitData::StructDecl => num_struct_decls += 1,
                    type_compiler::TypeUnitData::Tree(_) => {}
                }
            }
            // cu::progress!(&bar, count, "[{num_enums} enums ({num_enum_decls} decl), {num_unions} unions ({num_union_decls} decl), {num_structs} structs ({num_struct_decls} decl)] {name}");
            cu::progress!(&bar, count, "{name}");
            compilers.push(compiler);
        }









        cu::info!("found {num_enums} enums, {num_unions} unions, {num_structs} structs");
        cu::info!("found {num_enum_decls} enum decls, {num_union_decls} union decls, {num_struct_decls} struct decls");
        compilers
    };

    compilers.sort_by(|a, b| a.name.cmp(&b.name));

    let linked_types = type_linker::link_types(compilers).await.context("type linking failed")?;

    let keys = linked_types.categorized_type_keys();
    let enums = keys.enums.into_iter().map(|k| {
        (k, &linked_types.compiled.get_unwrap(k).unwrap().value)
    }).collect::<Vec<_>>();
    let unions = keys.unions.into_iter().map(|k| {
        (k, &linked_types.compiled.get_unwrap(k).unwrap().value)
    }).collect::<Vec<_>>();


    cu::info!("ENUMS{enums:#?}");
    cu::info!("UNIONS{unions:#?}");

    // let unit = units.iter().find(|x| x.name.contains("PauseMenuDataMgr")).unwrap();


    // cu::trace!("types: {types:#?}");

    // cu::debug!("compiled type count: {}", compiled_types.buckets().count());

    Ok(())
}

/// Parse the DWARF in the ELF bytes
pub fn parse_dwarf(bytes: &[u8]) -> cu::Result<Dwarf<'_>> {
    use cu::pre::*;
    use elf::ElfBytes;
    use elf::endian::LittleEndian as ElfLittleEndian;
    use gimli::{DwarfFileType, EndianSlice, LittleEndian as DwarfLittleEndian};

    let elf_data = ElfBytes::<ElfLittleEndian>::minimal_parse(bytes).context("failed to parse ELF")?;
    let mut dwarf = gimli::Dwarf::load(|section| {
        let section_name = section.name();
        cu::debug!("loading ELF section {section_name}");
        let header = cu::check!(
            elf_data.section_header_by_name(section_name),
            "cannot read ELF section header for section {section_name}"
        )?;
        let endian_slice = match header {
            Some(header) => {
                let start = header.sh_offset as usize;
                let end = start + header.sh_size as usize;
                cu::debug!("found ELF section {section_name} at byte start=0x{start:016x}, end=0x{end:016x}");
                EndianSlice::new(&bytes[start..end], DwarfLittleEndian)
            }
            None => {
                cu::debug!("did not found ELF section {section_name}");
                EndianSlice::new(&[], DwarfLittleEndian)
            }
        };
        cu::Ok(endian_slice)
    })
    .context("failed to load DWARF from ELF")?;
    dwarf.file_type = DwarfFileType::Main;

    Ok(dwarf)
}
