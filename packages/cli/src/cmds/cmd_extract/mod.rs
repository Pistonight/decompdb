#![allow(non_upper_case_globals)]

use cu::pre::*;

use crate::config::Config;

mod bucket;
pub use bucket::*;
mod compilation_unit;
mod type_compiler;
mod type_optimizer;
pub use compilation_unit::*;
mod dwarf_types;
use dwarf_types::*;
mod namespace;
pub use namespace::*;
mod type_defn;
use type_defn::*;
mod name_comparator;
use name_comparator::*;

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
    let name_comparator = NameComparator::new(config.extract.name_resolution.rules.clone());
    let bytes = cu::fs::read(&config.paths.elf)?;
    let dwarf = parse_dwarf(&bytes)?;

    let debug_info = dwarf.debug_info;

    let mut iter = debug_info.units();
    let mut units = Vec::new();

    while let Some(unit_header) = iter.next().context("error reading DWARF compilation unit header")? {
        let unit_ctx = CompUnit::new(unit_header, &dwarf)?;
        units.push(unit_ctx);
    }

    cu::info!("found {} compilation units", units.len());

    let unit = &units[1];

    cu::info!("unit: {unit}");

    let ns = unit.load_namespaces()?;

    let types = unit.load_types(&config.extract, ns)?;
    cu::debug!("type count: {}", types.len());
    // cu::trace!("types: {types:#?}");

    let compiled_types = type_compiler::compile_types(types, &config.extract)?;
    cu::debug!("compiled type count: {}", compiled_types.buckets().count());

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
