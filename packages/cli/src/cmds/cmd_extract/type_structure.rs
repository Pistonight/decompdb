use std::sync::Arc;

use cu::pre::*;
use tyyaml::{Prim, Tree};

use crate::config::Config;

use super::pre::*;
use super::bucket::GoffBuckets;

pub struct TypeStage1 {
    pub buckets: GoffBuckets,
    pub types: GoffMap<Type1>
}

/// "pieced-together" type info from raw types within a compilation unit
pub enum Type1 {
    /// Pritimive type
    Prim(Prim),
    /// Typedef <other> name; Other is global offset in debug info
    Typedef(String, Goff),
    /// Enum, could be anonymous
    Enum(Option<String>, Type1Enum),
    /// Declaration of an enum
    EnumDecl(String),
    /// Union, could be anonymous
    Union(Option<String>, Type0Union),
    /// Declaration of union
    UnionDecl(String),
    /// Struct or Class, could be anonymous
    Struct(Option<String>, Type0Struct),
    /// Declaration of struct or class
    StructDecl(String),
    /// Composition of other types. This is guaranteed to be not
    /// a TyTree::Base on the first level in stage 1
    Tree(Tree<Goff>),
}
impl Type1 {
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        match self {
            Type1::Prim(_) => {},
            Type1::Typedef(_, goff) => {
                *goff = cu::check!(f(*goff), "failed to map typedef goff {goff}")?;
            },
            Type1::Enum(_, _) => {},
            Type1::EnumDecl(_) => {},
            Type1::Union(_, data) => {
                for m in &mut data.members {
                    cu::check!(m.map_goff(&f), "failed to map union members")?;
                }
            }
            Type1::UnionDecl(_) => {}
            Type1::Struct(_, data) => {
                for m in &mut data.members {
                    cu::check!(m.map_goff(&f), "failed to map struct members")?;
                }
                for (_, e) in &mut data.vtable {
                    cu::check!(e.map_goff(&f), "failed to map struct vtable entry")?;
                }
            }
            Type1::StructDecl(_) => {},
            Type1::Tree(tree) => {
                cu::check!(tree.for_each_mut(|r| {
                    *r = f(*r)?;
                    cu::Ok(())
                }), "failed to map type tree")?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type1Enum {
    /// Base type, used to determine the size
    pub byte_size: u32,
    /// Enumerators of the enum, in the order they appear in DWARF
    pub enumerators: Vec<Enumerator>,
}

pub struct TypeStage0 {
    pub types: GoffMap<Type0>,
    pub config: Arc<Config>,
}

/// Raw definition of types parsed directly from DWARF
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type0 {
    /// Pritimive type
    Prim(Prim),
    /// Typedef <other> name; Other is offset in debug info
    Typedef(String, Goff),
    /// Enum, could be anonymous
    Enum(Option<String>, Type0Enum),
    /// Declaration of an enum
    EnumDecl(String),
    /// Union, could be anonymous
    Union(Option<String>, Type0Union),
    /// Declaration of union
    UnionDecl(String),
    /// Struct or Class, could be anonymous
    Struct(Option<String>, Type0Struct),
    /// Declaration of struct or class
    StructDecl(String),
    /// Composition of other types
    Tree(Tree<Goff>),
    /// Alias to another type (basically typedef without a name)
    Alias(Goff),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Enum {
    /// Base type, used to determine the size
    pub byte_size_or_base: Result<u32, Goff>,
    /// Enumerators of the enum, in the order they appear in DWARF
    pub enumerators: Vec<Enumerator>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enumerator {
    /// Name of the enumerator
    pub name: Arc<str>,
    /// Value of the enumerator. If the enumerator is unsigned
    /// and the value is greater than `i64::MAX`, then it's stored
    /// as if it's a `u64`. Enum type of byte size greater than 8
    /// is not allowed right now
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Union {
    /// Byte size of the union (should be size of the largest member)
    pub byte_size: u32,
    /// Union members. The members must have offset of 0 and special of None
    pub members: Vec<Type0Member>
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Struct {
    /// Byte size of the struct
    pub byte_size: u32,
    /// Vtable of the struct. (index, entry).
    /// Dtors will have an index of 0
    pub vtable: Vec<(usize, VtableEntry)>,
    /// Members of the struct
    pub members: Vec<Type0Member>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type0Member {
    /// Offset of the member within the struct. 0 For union.
    pub offset: usize,
    /// Name of the member. Could be None for anonymous union as member
    pub name: Option<Arc<str>>,
    /// Type of the member
    pub ty: Goff,
    /// Special-case member
    pub special: Option<SpecialMember>,
}

impl Type0Member {
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        self.ty = cu::check!(f(self.ty), "failed to map member goff {}", self.ty)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VtableEntry {
    /// Name of the virtual function
    pub name: Arc<str>,
    /// Types to make up the subroutine type
    pub function_types: Vec<Tree<Goff>>,
}

impl VtableEntry {
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        for x in &mut self.function_types {
            cu::check!(x.for_each_mut(|r| {
                *r = f(*r)?;
                cu::Ok(())
            }), "failed to map vtable entry")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpecialMember {
    Base,
    Vfptr,
    Bitfield(u32), // byte_size
}
