use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use cu::pre::*;
use elf::ElfBytes;
use elf::endian::LittleEndian as ElfLittleEndian;
use gimli::{
    Abbreviations, AttributeValue, DwarfFileType, EndianSlice, LittleEndian as DwarfLittleEndian, Operation,
    UnitSectionOffset,
};
use tyyaml::{Prim, TreeRepr};

use super::pre::*;

pub type In<'i> = gimli::EndianSlice<'i, gimli::LittleEndian>;
pub type Tag = gimli::DwTag;

/// Holder of Dwarf info, backed by a shared ELF buffer
pub struct Dwarf {
    dwarf: gimli::Dwarf<In<'static>>,
    buf: ArcBuf,
}

impl Dwarf {
    /// Parse the DWARF in the ELF bytes
    pub fn try_parse(buf: Arc<[u8]>) -> cu::Result<Arc<Self>> {
        let raw_buf = ArcBuf::new(buf);
        // safety: the lifetime of raw_buf_ref is managed
        // by the Arc.
        let raw_buf_ref: &'static [u8] = unsafe { &*raw_buf.0 };
        let elf_data = ElfBytes::<ElfLittleEndian>::minimal_parse(raw_buf_ref).context("failed to parse ELF")?;
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
                    EndianSlice::new(&raw_buf_ref[start..end], DwarfLittleEndian)
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

        Ok(Arc::new(Self { dwarf, buf: raw_buf }))
    }

    pub fn iter_units(self_: &Arc<Self>) -> UnitIter {
        let iter = self_.dwarf.debug_info.units();
        UnitIter {
            debug_info_iter: iter,
            dwarf: Arc::clone(&self_),
        }
    }
}

pub struct UnitIter {
    debug_info_iter: gimli::DebugInfoUnitHeadersIter<In<'static>>,
    dwarf: Arc<Dwarf>,
}

impl UnitIter {
    pub fn next_unit(&mut self) -> cu::Result<Option<Unit>> {
        let header = cu::check!(self.debug_info_iter.next(), "failed to read next unit header")?;
        let Some(header) = header else {
            return Ok(None);
        };
        let offset = match header.offset() {
            UnitSectionOffset::DebugInfoOffset(o) => o.0,
            UnitSectionOffset::DebugTypesOffset(o) => {
                cu::bail!("failed to get DWARF offset for compilation unit: expecting DebugInfoOffset, got {o:?}");
            }
        };
        let unit = cu::check!(
            gimli::Unit::new(&self.dwarf.dwarf, header),
            "failed to create debug info unit"
        )?;
        let abbrevs = cu::check!(
            header.abbreviations(&self.dwarf.dwarf.debug_abbrev),
            "failed to create debug info unit abbrevs"
        )?;
        let mut unit = Unit {
            unit,
            header,
            abbrevs,
            dwarf: Arc::clone(&self.dwarf),
            name: String::new(),
            offset: offset.into(),
        };

        let mut tree = cu::check!(unit.tree(), "failed to parse root node when creating debug info unit")?;
        let root = cu::check!(tree.root(), "failed to parse root node when creating debug info unit")?;
        let entry = root.entry();
        let name = cu::check!(entry.name(), "failed to get name of compilation unit")?;
        unit.name = name.to_string();
        Ok(Some(unit))
    }
}

/// Holder of a Unit in .debug_info
#[derive(Display)]
#[display("compilation unit at {} ({})", self.offset, self.name)]
pub struct Unit {
    unit: gimli::Unit<In<'static>>,
    header: gimli::UnitHeader<In<'static>>,
    abbrevs: Abbreviations,
    dwarf: Arc<Dwarf>,
    /// name of the unit (typically file name)
    pub name: String,
    /// offset of the unit
    pub offset: Goff,
}

impl Unit {
    pub fn tree(&self) -> cu::Result<EntriesTree<'_>> {
        self.entries_tree(None)
    }
    pub fn tree_at(&self, loff: Loff) -> cu::Result<EntriesTree<'_>> {
        self.entries_tree(Some(loff))
    }
    fn entries_tree(&self, loff: Option<Loff>) -> cu::Result<EntriesTree<'_>> {
        let tree = match loff {
            None => cu::check!(self.unit.entries_tree(None), "failed to parse root for {self}")?,
            Some(loff) => cu::check!(
                self.unit.entries_tree(Some(loff.into())),
                "failed to parse tree at {} for {self}",
                self.goff(loff)
            )?,
        };
        Ok(EntriesTree { unit: self, tree })
    }

    /// Get a single entry at offset
    pub fn entry_at<'x>(&'x self, loff: Loff) -> cu::Result<Die<'x, 'x>> {
        let entry = self.unit.entry(loff.into());
        let entry = cu::check!(entry, "failed to read entry at {} for {self}", self.goff(loff))?;
        Ok(Die {
            unit: self,
            entry: Cow::Owned(entry),
        })
    }

    /// Convert local offset in this compilation unit to global offset
    pub fn goff(&self, loff: Loff) -> Goff {
        loff.to_global(self.offset)
    }

    /// Get an attribute value as string
    fn attr_string<'x>(&'x self, value: AttributeValue<In<'static>>) -> cu::Result<&'x str> {
        let value = cu::check!(
            self.dwarf.dwarf.attr_string(&self.unit, value),
            "failed to get attribute value as string in {self}"
        )?;
        cu::check!(
            value.to_string(),
            "failed to decode attribute value as string in {self}"
        )
    }
    /// Get an attribute value as signed integer
    fn attr_signed(&self, offset: Goff, at: DwAt, attr: AttributeValue<In<'_>>) -> cu::Result<i64> {
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
    fn attr_unsigned(&self, offset: Goff, at: DwAt, attr: AttributeValue<In<'_>>) -> cu::Result<u64> {
        match attr {
            AttributeValue::Data1(x) => Ok(x as u64),
            AttributeValue::Data2(x) => Ok(x as u64),
            AttributeValue::Data4(x) => Ok(x as u64),
            AttributeValue::Data8(x) => Ok(x),
            AttributeValue::Udata(x) => Ok(x),
            AttributeValue::Addr(x) => Ok(x),
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
            other => cu::bail!("expecting unsigned data for entry {offset}, attr {at}, got: {other:?}"),
        }
    }
}

pub struct EntriesTree<'x> {
    unit: &'x Unit,
    tree: gimli::EntriesTree<'x, 'x, In<'static>>,
}

impl<'x> EntriesTree<'x> {
    pub fn root(&mut self) -> cu::Result<DieNode<'x, '_>> {
        let node = cu::check!(self.tree.root(), "failed to parse tree node in {}", self.unit)?;
        Ok(DieNode { unit: self.unit, node })
    }
}

pub struct DieNode<'x, 't> {
    node: gimli::EntriesTreeNode<'x, 'x, 't, In<'static>>,
    unit: &'x Unit,
}

impl<'x> DieNode<'x, '_> {
    pub fn unit(&self) -> &'x Unit {
        self.unit
    }
    pub fn entry(&self) -> Die<'x, '_> {
        let entry = self.node.entry();
        Die {
            unit: self.unit,
            entry: Cow::Borrowed(entry),
        }
    }
    pub fn goff(&self) -> Goff {
        self.unit.goff(self.node.entry().offset().into())
    }
    /// Execute f on each direct child node (does not include this node)
    pub fn for_each_child<F>(self, mut f: F) -> cu::Result<()>
    where
        F: for<'t> FnMut(DieNode<'x, 't>) -> cu::Result<()>,
    {
        let offset = self.goff();
        let mut children = self.node.children();
        while let Some(child) = cu::check!(
            children.next(),
            "failed to read a child for entry at {offset} in {}",
            self.unit
        )? {
            let node = DieNode {
                node: child,
                unit: self.unit,
            };
            let child_offset = node.goff();
            cu::check!(f(node), "error while processing child entry at {child_offset}")?;
        }
        Ok(())
    }
}

pub struct Die<'x, 'n> {
    unit: &'x Unit,
    entry: Cow<'n, gimli::DebuggingInformationEntry<'x, 'x, In<'static>, usize>>,
}

impl<'x> Die<'x, '_> {
    /// Get the global offset of this entry
    pub fn goff(&self) -> Goff {
        (self.entry.offset().0 + usize::from(self.unit.offset)).into()
    }
    pub fn to_global(&self, loff: Loff) -> Goff {
        self.unit.goff(loff)
    }
    /// Get the unit
    pub fn unit(&self) -> &'x Unit {
        self.unit
    }
    pub fn tag(&self) -> Tag {
        self.entry.tag()
    }

    /// Get the name of the entry
    pub fn name_owned(&self) -> cu::Result<String> {
        Ok(self.name()?.to_string())
    }

    /// Get the name of the entry
    pub fn name(&self) -> cu::Result<&str> {
        let value = self.name_opt()?;
        let offset = self.goff();
        let value = cu::check!(
            value,
            "DW_AT_name is missing for entry at offset {offset} in {}",
            self.unit
        )?;
        Ok(value)
    }
    /// Get the name of the entry before the first `<`. This can only be used
    /// for types, and not function names, because of `operator<=`
    pub fn untemplated_name_owned(&self) -> cu::Result<String> {
        Ok(self.untemplated_name()?.to_string())
    }

    /// Get the name of the entry before the first `<`. This can only be used
    /// for types, and not function names, because of `operator<=`
    pub fn untemplated_name(&self) -> cu::Result<&str> {
        let value = self.untemplated_name_opt()?;
        let offset = self.goff();
        let value = cu::check!(
            value,
            "DW_AT_name is missing for entry at offset {offset} in {}",
            self.unit
        )?;
        Ok(value)
    }

    /// Get the name of the entry before the first `<`. This can only be used
    /// for types, and not function names, because of `operator<=`
    pub fn untemplated_name_opt(&self) -> cu::Result<Option<&str>> {
        let value = self.name_opt()?;
        Ok(value.map(|x| match x.find('<') {
            Some(i) => &x[..i],
            None => x,
        }))
    }

    /// Get the DW_AT_name of a DIE, if it exists
    pub fn name_opt(&self) -> cu::Result<Option<&str>> {
        self.str_opt(DW_AT_name)
    }

    /// Get a string attribute value
    pub fn str_opt(&self, attr: DwAt) -> cu::Result<Option<&str>> {
        let offset = self.goff();
        let value = cu::check!(
            self.entry.attr_value(attr),
            "failed to read {attr} at {offset} in {}",
            self.unit
        )?;
        let Some(value) = value else {
            return Ok(None);
        };
        let value = cu::check!(
            self.unit.attr_string(value),
            "failed to read value for {attr} at {offset} in {}",
            self.unit
        )?;
        Ok(Some(value))
    }
    /// Get a signed integer attribute value
    pub fn int(&self, attr: DwAt) -> cu::Result<i64> {
        let value = self.int_opt(attr)?;
        let offset = self.goff();
        let value = cu::check!(value, "entry is missing {attr} at offset {offset}")?;
        Ok(value)
    }
    /// Get a signed integer attribute value, allowing it to be missing
    pub fn int_opt(&self, attr: DwAt) -> cu::Result<Option<i64>> {
        let offset = self.goff();
        let value = cu::check!(self.entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let Some(value) = value else {
            return Ok(None);
        };
        let value = self.unit.attr_signed(offset, attr, value)?;
        Ok(Some(value))
    }
    /// Get an unsigned integer attribute value
    pub fn uint(&self, attr: DwAt) -> cu::Result<u64> {
        let value = self.uint_opt(attr)?;
        let offset = self.goff();
        let value = cu::check!(value, "entry is missing {attr} at offset {offset}")?;
        Ok(value)
    }
    /// Get an unsigned integer attribute value, allowing it to be missing
    pub fn uint_opt(&self, attr: DwAt) -> cu::Result<Option<u64>> {
        let offset = self.goff();
        let value = cu::check!(self.entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let Some(value) = value else {
            return Ok(None);
        };
        let value = self.unit.attr_unsigned(offset, attr, value)?;
        Ok(Some(value))
    }
    /// Get an attr of an entry as flag
    pub fn flag(&self, attr: DwAt) -> cu::Result<bool> {
        let offset = self.goff();
        let value = cu::check!(self.entry.attr_value(attr), "failed to read {attr} at {offset}")?;
        match value {
            None => Ok(false),
            Some(AttributeValue::Flag(x)) => Ok(x),
            _ => {
                cu::bail!("expecting {attr} to be a Flag, at entry {offset}");
            }
        }
    }
    /// Get the DW_TAG_vtable_elem_location of a DIE (index of the entry in the vtable), return None if not virtual
    pub fn vtable_index(&self) -> cu::Result<Option<usize>> {
        let offset = self.goff();
        let virtuality = cu::check!(
            self.entry.attr_value(DW_AT_virtuality),
            "failed to read DW_AT_virtuality for entry at {offset}"
        )?;
        let velem = cu::check!(
            self.entry.attr_value(DW_AT_vtable_elem_location),
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
                let vel = self.unit.attr_unsigned(offset, DW_AT_vtable_elem_location, velem)?;
                Ok(Some(vel as usize))
            }
            _ => cu::bail!("expecting DW_AT_virtuality to be Virtuality, at entry {offset}"),
        }
    }

    /// Read an attribute of a DIE, expecting a unit reference (local offset)
    pub fn loff(&self, attr: DwAt) -> cu::Result<Loff> {
        let t = self.loff_opt(attr)?;
        cu::check!(t, "missing {attr} for entry at offset {}", self.goff())
    }

    /// Read an attribute of a DIE, expecting a local offset, allowing it to be missing
    pub fn loff_opt(&self, attr: DwAt) -> cu::Result<Option<Loff>> {
        let offset = self.goff();
        let type_value = cu::check!(self.entry.attr_value(attr), "failed to read {attr} at offset {offset}")?;
        let Some(type_value) = type_value else {
            return Ok(None);
        };
        let type_offset = match type_value {
            AttributeValue::UnitRef(offset) => offset,
            _ => cu::bail!("expecting {attr} to be a unit ref at offset {offset}"),
        };
        Ok(Some(type_offset.into()))
    }

    /// Read this entry as a primitive type node
    pub fn prim_type(&self) -> cu::Result<Prim> {
        let offset = self.goff();
        let encoding = cu::check!(
            self.entry.attr_value(DW_AT_encoding),
            "failed to read DW_AT_encoding for primitive type at offset {offset}"
        )?;
        let encoding = cu::check!(encoding, "missing DW_AT_encoding for primitive type at offset {offset}")?;
        let AttributeValue::Encoding(encoding) = encoding else {
            cu::bail!("expecting an Encoding attribute for DW_AT_encoding for primitive type at offset {offset}");
        };
        let byte_size = cu::check!(
            self.uint(DW_AT_byte_size),
            "failed to get byte size for primitive type at offset {offset}"
        )?;
        let prim = match (encoding, byte_size) {
            (DW_ATE_boolean, 0x1) => Prim::Bool,
            (DW_ATE_unsigned, 0x1) => Prim::U8,
            (DW_ATE_unsigned_char, 0x1) => Prim::U8,
            (DW_ATE_signed, 0x1) => Prim::I8,
            (DW_ATE_signed_char, 0x1) => Prim::I8,

            (DW_ATE_unsigned, 0x2) => Prim::U16,
            (DW_ATE_signed, 0x2) => Prim::I16,
            (DW_ATE_UTF, 0x2) => Prim::U16,

            (DW_ATE_unsigned, 0x4) => Prim::U32,
            (DW_ATE_signed, 0x4) => Prim::I32,
            (DW_ATE_float, 0x4) => Prim::F32,

            (DW_ATE_unsigned, 0x8) => Prim::U64,
            (DW_ATE_signed, 0x8) => Prim::I64,
            (DW_ATE_float, 0x8) => Prim::F64,

            (DW_ATE_unsigned, 0x10) => Prim::U128,
            (DW_ATE_signed, 0x10) => Prim::I128,
            (DW_ATE_float, 0x10) => Prim::F128,

            _ => cu::bail!("unknown primitive type. encoding: {encoding}, byte size: {byte_size}"),
        };

        Ok(prim)
    }

    pub fn is_inlined(&self) -> cu::Result<bool> {
        let offset = self.goff();
        let inline = cu::check!(self.entry.attr_value(DW_AT_inline), 
            "failed to read DW_AT_inline for entry at offset {offset}")?;
        match inline {
            None => Ok(false),
            Some(AttributeValue::Inline(x)) => {
                Ok(matches!(x, DW_INL_inlined | DW_INL_declared_inlined))
            }
            _ => {
            cu::bail!("expecting DW_AT_inline to be an inline attribute at offset {offset}")
            }
        }
    }

    /// Execute f on each direct child node (does not include the input node)
    pub fn for_each_child<F>(&self, f: F) -> cu::Result<()>
    where
        F: for<'t> FnMut(DieNode<'x, 't>) -> cu::Result<()>,
    {
        let mut tree = self.unit.tree_at(self.entry.offset().into())?;
        let node = tree.root()?;
        node.for_each_child(f)
    }
}

struct ArcBuf(*const [u8]);
impl ArcBuf {
    fn new(buf: Arc<[u8]>) -> Self {
        Self(Arc::into_raw(buf))
    }
}
impl Drop for ArcBuf {
    fn drop(&mut self) {
        unsafe {
            Arc::from_raw(self.0);
        }
    }
}
unsafe impl Send for ArcBuf {}
unsafe impl Sync for ArcBuf {}

pub fn is_type_tag(tag: Tag) -> bool {
    match tag {
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
        | DW_TAG_base_type => true,

        // this is to prevent constants above from being interpreted as variable ident
        _tag => false
    }
}

// if something has size this big it's probably wrong, so it's fine to use
// as special value
pub const UNSIZED: u32 = u32::MAX;

pub type GoffMap<T> = BTreeMap<Goff, T>;
pub type GoffSet = BTreeSet<Goff>;

/// Local offset into a Compilation Unit in DWARF
#[rustfmt::skip]
#[derive(
    DebugCustom, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
    Into, Display
)]
#[display("local(0x{:08x})", self.0)]
#[debug("local(0x{:08x})", self.0)]
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
    DebugCustom, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord,
    From, Into, Display
)]
#[display("0x{:08x}", self.0)]
#[debug("0x{:08x}", self.0)]
pub struct Goff(usize);

impl Goff {
    /// Get a fabricated global offset for primitive types
    pub const fn prim(p: Prim) -> Self {
        let s = match p {
            Prim::Void => 0x1FFFF0000,
            Prim::Bool => 0x1FFFF0001,
            Prim::U8 => 0x1FFFF0101,
            Prim::U16 => 0x1FFFF0102,
            Prim::U32 => 0x1FFFF0104,
            Prim::U64 => 0x1FFFF0108,
            Prim::U128 => 0x1FFFF0110,
            Prim::I8 => 0x1FFFF0201,
            Prim::I16 => 0x1FFFF0202,
            Prim::I32 => 0x1FFFF0204,
            Prim::I64 => 0x1FFFF0208,
            Prim::I128 => 0x1FFFF0210,
            Prim::F32 => 0x1FFFF0304,
            Prim::F64 => 0x1FFFF0308,
            Prim::F128 => 0x1FFFF0310,
        };
        Self(s)
    }

    pub const fn pointer() -> Self {
        Self(0x2FFFF0000)
    }

    pub const fn ptmd() -> Self {
        Self(0x2FFFF0001)
    }

    pub const fn ptmf() -> Self {
        Self(0x2FFFF0002)
    }

    pub const fn is_prim(self) -> bool {
        return self.0 >= 0x1FFFF0000;
    }
}

impl Serialize for Goff {
    fn serialize<S: serde::Serializer>(&self, ser: S) -> Result<S::Ok, S::Error> {
        ser.serialize_str(&self.to_string())
    }
}

impl<'de> Deserialize<'de> for Goff {
    fn deserialize<D: serde::Deserializer<'de>>(de: D) -> Result<Self, D::Error> {
        return de.deserialize_str(Visitor);
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Goff;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(f, "a hex integer literal")
            }
            fn visit_str<E: serde::de::Error>(self, v: &str) -> Result<Self::Value, E> {
                match cu::parse::<usize>(v) {
                    Ok(x) => Ok(Goff(x)),
                    Err(e) => Err(serde::de::Error::custom(format!("failed to parse Goff: {e}"))),
                }
            }
        }
    }
}

impl TreeRepr for Goff {
    fn serialize_spec(&self) -> cu::Result<String> {
        Ok(self.to_string())
    }

    fn deserialize_void() -> Self {
        Self::prim(Prim::Void)
    }

    fn deserialize_spec(spec: &str) -> cu::Result<Self> {
        Ok(Self(cu::parse::<usize>(spec)?))
    }
}
