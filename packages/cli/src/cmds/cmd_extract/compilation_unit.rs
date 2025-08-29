use std::sync::Arc;

use cu::pre::*;
use gimli::{Abbreviations, AttributeValue, Operation, UnitSectionOffset};

use super::pre::*;

/// A Compilation Unit in DWARF
#[derive(Deref, Display)]
#[display("compilation unit at {} ({})", self.offset, self.name)]
pub struct CompUnit<'d, 'i> {
    #[deref]
    pub unit: Unit<'i>,
    /// name of the unit (typically file name)
    pub name: String,
    /// offset of the unit
    pub offset: Goff,
    /// header of the unit
    pub header: UnitHeader<'i>,
    pub abbrevs: Abbreviations,
    pub dwarf: Arc<Dwarf>,
}

impl<'d, 'i> CompUnit<'d, 'i> {


    // /// Get if the entry is a declaration
    // pub fn entry_is_decl(&self, entry: &Die<'i, '_, '_>) -> cu::Result<bool> {
    //     self.entry_attr_flag(entry, DW_AT_declaration)
    // }



    /// Execute f on each direct child node (does not include the input node)
    pub fn entry_for_each_child<F>(&self, entry: &Die<'i, '_, '_>, f: F) -> cu::Result<()>
    where
        F: for<'t> FnMut(Node<'i, '_, '_, 't>) -> cu::Result<()>,
    {
        let mut tree = self.tree_at(entry.offset().into())?;
        let node = tree.root()?;
        self.for_each_child(node, f)
    }
}
