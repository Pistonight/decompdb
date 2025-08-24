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
    pub name: &'i str,
    /// offset of the unit
    pub offset: Goff,
    /// header of the unit
    pub header: UnitHeader<'i>,
    pub abbrevs: Abbreviations,
    pub dwarf: &'d Dwarf<'i>,
}

impl<'d, 'i> CompUnit<'d, 'i> {
    /// Minimally parse a compilation unit debug info
    pub fn new(header: UnitHeader<'i>, dwarf: &'d Dwarf<'i>) -> cu::Result<Self> {
        let offset = match header.offset() {
            UnitSectionOffset::DebugInfoOffset(o) => o.0,
            UnitSectionOffset::DebugTypesOffset(o) => {
                cu::bail!("failed to get DWARF offset for compilation unit: expecting DebugInfoOffset, got {o:?}");
            }
        };
        let unit = Unit::new(dwarf, header).context("failed to create gimli::Unit")?;
        let abbrevs = header
            .abbreviations(&dwarf.debug_abbrev)
            .context("failed to create gimli::Abbreviations")?;
        let mut self_ = Self {
            offset: offset.into(),
            unit,
            header,
            abbrevs,
            dwarf,
            name: "<unknown>",
        };
        let mut tree = self_.tree()?;
        let root = tree
            .root()
            .context("failed to get root entry while creating compilation unit")?;
        self_.name = self_
            .entry_name(root.entry())
            .context("failed to get compilation unit name")?;
        Ok(self_)
    }

    /// Create an entry tree for the unit
    pub fn tree(&self) -> cu::Result<Tree<'i, '_, '_>> {
        let tree = self.header.entries_tree(&self.abbrevs, None);
        cu::check!(tree, "failed to create gimli::EntriesTree for {self}")
    }

    /// Create entry tree from entry at offset
    pub fn tree_at(&self, local_off: Loff) -> cu::Result<Tree<'i, '_, '_>> {
        let tree = self.header.entries_tree(&self.abbrevs, Some(local_off.into()));
        cu::check!(
            tree,
            "failed to create gimli::EntriesTree at {} for {self}",
            local_off.to_global(self.offset)
        )
    }

    /// Get a single entry at offset
    pub fn entry_at(&self, local_off: Loff) -> cu::Result<Die<'i, '_, '_>> {
        let entry = self.unit.entry(local_off.into());
        cu::check!(
            entry,
            "failed to read entry at {} for {self}",
            local_off.to_global(self.offset)
        )
    }

    /// Get the DW_AT_name of a DIE
    pub fn entry_name(&self, entry: &Die<'i, '_, '_>) -> cu::Result<&'i str> {
        let offset = self.entry_goff(entry);
        let value = cu::check!(
            entry.attr_value(DW_AT_name),
            "failed to read DW_AT_name at offset {offset} in {self}"
        )?;
        let value = cu::check!(value, "DW_AT_name is missing for entry at offset {offset} in {self}")?;
        self.attr_string(value)
    }

    /// Get the DW_AT_name of a DIE, if it exists
    pub fn entry_name_opt(&self, entry: &Die<'i, '_, '_>) -> cu::Result<Option<&'i str>> {
        let offset = self.entry_goff(entry);
        let value = cu::check!(
            entry.attr_value(DW_AT_name),
            "failed to read DW_AT_name at offset {offset} in {self}"
        )?;
        let Some(value) = value else {
            return Ok(None);
        };
        let value = self.attr_string(value)?;
        Ok(Some(value))
    }

    /// Get the global offset of a DIE
    pub fn entry_goff(&self, entry: &Die<'i, '_, '_>) -> Goff {
        (entry.offset().0 + usize::from(self.offset)).into()
    }

    /// Get an attribute value as string
    pub fn attr_string(&self, value: AttributeValue<In<'i>>) -> cu::Result<&'i str> {
        let value = cu::check!(
            self.dwarf.attr_string(&self.unit, value),
            "failed to get attribute value as string in {self}"
        )?;
        cu::check!(
            value.to_string(),
            "failed to decode attribute value as string in {self}"
        )
    }

    /// Get a signed integer attribute value
    pub fn entry_signed_attr(&self, entry: &Die<'i, '_, '_>, attr: DwAt) -> cu::Result<i64> {
        let offset = self.entry_goff(entry);
        let value = cu::check!(entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let value = cu::check!(value, "entry is missing {attr} at offset {offset}")?;
        let value = self.attr_signed(offset, attr, value)?;
        Ok(value)
    }

    /// Get an unsigned integer attribute value
    pub fn entry_unsigned_attr(&self, entry: &Die<'i, '_, '_>, attr: DwAt) -> cu::Result<u64> {
        let offset = self.entry_goff(entry);
        let value = cu::check!(entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let value = cu::check!(value, "entry is missing {attr} at offset {offset}")?;
        let value = self.attr_unsigned(offset, attr, value)?;
        Ok(value)
    }

    /// Get an unsigned integer attribute value, allowing it to be missing
    pub fn entry_unsigned_attr_opt(&self, entry: &Die<'i, '_, '_>, attr: DwAt) -> cu::Result<Option<u64>> {
        let offset = self.entry_goff(entry);
        let value = cu::check!(entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let Some(value) = value else {
            return Ok(None);
        };
        let value = self.attr_unsigned(offset, attr, value)?;
        Ok(Some(value))
    }

    /// Get an attribute value as signed integer
    pub fn attr_signed(&self, offset: Goff, at: DwAt, attr: AttributeValue<In<'i>>) -> cu::Result<i64> {
        match attr {
            AttributeValue::Data1(x) => Ok(x as i64),
            AttributeValue::Data2(x) => Ok(x as i64),
            AttributeValue::Data4(x) => Ok(x as i64),
            AttributeValue::Data8(x) => Ok(x as i64),
            AttributeValue::Udata(x) => Ok(x as i64),
            AttributeValue::Sdata(x) => Ok(x as i64),
            _ => cu::bail!("expecting signed data for entry {offset}, attr {at}"),
        }
    }
    /// Get an attribute value as unsigned integer
    pub fn attr_unsigned(&self, offset: Goff, at: DwAt, attr: AttributeValue<In<'i>>) -> cu::Result<u64> {
        match attr {
            AttributeValue::Data1(x) => Ok(x as u64),
            AttributeValue::Data2(x) => Ok(x as u64),
            AttributeValue::Data4(x) => Ok(x as u64),
            AttributeValue::Data8(x) => Ok(x),
            AttributeValue::Udata(x) => Ok(x),
            // this is used for vtable elem location
            AttributeValue::Exprloc(expr) => {
                let mut ops = expr.operations(self.unit.encoding());
                let op = cu::check!(ops.next(), "failed to read Exprloc ops for entry {offset}, attr {at}")?;
                let op = cu::check!(op, "expecting an Exprloc ops for entry {offset}, attr {at}")?;
                let Operation::UnsignedConstant { value } = op else {
                    cu::bail!("expecting UnsignedConstant for Exprloc ops for entry {offset}, attr {at}");
                };
                Ok(value)
            }
            _ => cu::bail!("expecting unsigned data for entry {offset}, attr {at}"),
        }
    }

    /// Get if the entry is a declaration
    pub fn entry_is_decl(&self, entry: &Die<'i, '_, '_>) -> cu::Result<bool> {
        let offset = self.entry_goff(entry);
        let value = cu::check!(
            entry.attr_value(DW_AT_declaration),
            "failed to read DW_AT_declaration at {offset}"
        )?;
        match value {
            None => Ok(false),
            Some(AttributeValue::Flag(x)) => Ok(x),
            _ => {
                cu::bail!("expecting DW_AT_declaration to be a Flag, at entry {offset}");
            }
        }
    }

    /// Get the DW_TAG_vtable_elem_location of a DIE (index of the entry in the vtable), return None if not virtual
    pub fn entry_vtable_index(&self, entry: &Die<'i, '_, '_>) -> cu::Result<Option<usize>> {
        let offset = self.entry_goff(entry);
        let virtuality = cu::check!(
            entry.attr_value(DW_AT_virtuality),
            "failed to read DW_AT_virtuality for entry at {offset}"
        )?;
        let velem = cu::check!(
            entry.attr_value(DW_AT_vtable_elem_location),
            "failed to read DW_AT_vtable_elem_location for entry at {offset}"
        )?;
        match virtuality {
            None | Some(AttributeValue::Virtuality(DW_VIRTUALITY_none)) => {
                // vtable_elem_localtion should not be there
                if velem.is_some() {
                    cu::bail!("DW_AT_vtable_elem_location should not exist for non virtual entry at {offset}");
                }
                Ok(None)
            }
            Some(AttributeValue::Virtuality(DW_VIRTUALITY_virtual))
            | Some(AttributeValue::Virtuality(DW_VIRTUALITY_pure_virtual)) => {
                let velem = cu::check!(
                    velem,
                    "missing DW_AT_vtable_elem_location for virtual entry at {offset}"
                )?;
                let vel = self.attr_unsigned(offset, DW_AT_vtable_elem_location, velem)?;
                Ok(Some(vel as usize))
            }
            _ => cu::bail!("expecting DW_AT_virtuality to be Virtuality, at entry {offset}"),
        }
    }

    /// Execute f on each direct child node (does not include the input node)
    pub fn for_each_child<F>(&self, node: Node<'i, '_, '_, '_>, mut f: F) -> cu::Result<()>
    where
        F: for<'t> FnMut(Node<'i, '_, '_, 't>) -> cu::Result<()>,
    {
        let offset = self.entry_goff(node.entry());
        let mut children = node.children();
        while let Some(child) = cu::check!(
            children.next(),
            "failed to read a child for entry at {offset} in {self}"
        )? {
            let child_offset = self.entry_goff(child.entry());
            cu::check!(f(child), "error while processing child entry at {child_offset}")?;
        }
        Ok(())
    }

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
