use std::collections::BTreeMap;

use cu::pre::*;

use gimli::constants::*;

// convienience type aliases
pub type In<'i> = gimli::EndianSlice<'i, gimli::LittleEndian>;
pub type Unit<'i> = gimli::Unit<In<'i>>;
pub type UnitHeader<'i> = gimli::UnitHeader<In<'i>>;
pub type Tree<'i, 'a, 'u> = gimli::EntriesTree<'a, 'u, In<'i>>;
pub type Node<'i, 'a, 'u, 't> = gimli::EntriesTreeNode<'a, 'u, 't, In<'i>>;
pub type Dwarf<'i> = gimli::Dwarf<In<'i>>;
pub type Die<'i, 'a, 'u> = gimli::DebuggingInformationEntry<'a, 'u, In<'i>, usize>;
pub type Tag = gimli::DwTag;
pub type GoffMap<T> = BTreeMap<Goff, T>;

/// Local offset into a Compilation Unit in DWARF
#[rustfmt::skip]
#[derive(
    Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
    Into, Display
)]
#[display("local(0x{:08x})", self.0)]
pub struct Loff(usize);

impl From<gimli::UnitOffset<usize>> for Loff {
    fn from(value: gimli::UnitOffset<usize>) -> Self {
        Self(value.0)
    }
}

impl From<Loff> for gimli::UnitOffset<usize> {
    fn from(value: Loff) -> Self {
        Self(value.0)
    }
}

impl Loff {
    /// Convert unit-local offset to global offset by adding the offset of the unit
    #[inline(always)]
    pub fn to_global(self, unit_offset: impl Into<usize>) -> Goff {
        Goff(self.0 + unit_offset.into())
    }
}

/// Global offset into DWARF
#[rustfmt::skip]
#[derive(
    Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
    From, Into, Display
)]
#[display("0x{:08x}", self.0)]
pub struct Goff(usize);

pub fn is_type_tag(tag: Tag) -> bool {
    matches!(
        tag,
        DW_TAG_structure_type
        | DW_TAG_class_type
        | DW_TAG_union_type
        | DW_TAG_enumeration_type
        // typedefs
        | DW_TAG_unspecified_type
        | DW_TAG_typedef
        // pointer
        | DW_TAG_pointer_type
        | DW_TAG_reference_type
        | DW_TAG_array_type
        // qualifier
        | DW_TAG_const_type
        | DW_TAG_volatile_type
        | DW_TAG_restrict_type
        // function
        | DW_TAG_subroutine_type
        | DW_TAG_ptr_to_member_type
        // base
        | DW_TAG_base_type
    )
}
