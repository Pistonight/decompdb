#![allow(non_upper_case_globals)]

mod main;
pub use main::*;

mod dwarf_parse;

mod bucket;
mod name_comparator;
mod namespace;
mod deduper;

// mod stage0_clang_parse;
mod stage0_loader;
// mod type0_compiler;
// mod type_compiler;
// mod type_optimizer;
// mod type_linker;
mod type_structure;

mod stage1;

mod pre {
    pub use super::dwarf_parse::*;
    pub use super::namespace::{NameSeg, Namespace, NamespaceLiteral, NamespaceMaps, NamespacedName};
    pub use gimli::constants::*;
}
