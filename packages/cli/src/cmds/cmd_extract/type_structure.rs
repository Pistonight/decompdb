use std::sync::Arc;

use cu::pre::*;
use tyyaml::{Prim, Tree};

use crate::config::Config;

use super::bucket::{GoffBuckets, MergeQueue};
use super::pre::*;

pub struct TypeStage1 {
    pub offset: usize,
    pub name: String,
    pub buckets: GoffBuckets,
    pub types: GoffMap<Type1>,
}

/// "pieced-together" type info from raw types within a compilation unit
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            Type1::Prim(_) => {}
            Type1::Typedef(_, goff) => {
                *goff = cu::check!(f(*goff), "failed to map typedef goff {goff}")?;
            }
            Type1::Enum(_, _) => {}
            Type1::EnumDecl(_) => {}
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
            Type1::StructDecl(_) => {}
            Type1::Tree(tree) => {
                cu::check!(
                    tree.for_each_mut(|r| {
                        *r = f(*r)?;
                        cu::Ok(())
                    }),
                    "failed to map type tree"
                )?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type1Enum {
    /// Base type, used to determine the size
    pub byte_size: u32,
    /// Enumerators of the enum, in the order they appear in DWARF
    pub enumerators: Vec<Enumerator>,
}

pub struct TypeStage0 {
    pub offset: usize,
    pub name: String,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Enumerator {
    /// Name of the enumerator
    pub name: Arc<str>,
    /// Value of the enumerator. If the enumerator is unsigned
    /// and the value is greater than `i64::MAX`, then it's stored
    /// as if it's a `u64`. Enum type of byte size greater than 8
    /// is not allowed right now
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type0Union {
    /// Byte size of the union (should be size of the largest member)
    pub byte_size: u32,
    /// Union members. The members must have offset of 0 and special of None
    pub members: Vec<Type0Member>,
}

impl Type0Union {
    pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
        cu::ensure!(
            self.byte_size == other.byte_size,
            "union byte sizes are not equal: 0x{:x} != 0x{:x}",
            self.byte_size,
            other.byte_size
        );
        cu::ensure!(
            self.members.len() == other.members.len(),
            "union member counts are not equal: {} != {}",
            self.members.len(),
            other.members.len()
        );
        let mut mq = MergeQueue::default();
        for (a, b) in std::iter::zip(&self.members, &other.members) {
            cu::check!(a.merge_checked(b, &mut mq), "cannot merge union member")?;
        }
        merges.extend(mq);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type0Struct {
    /// Byte size of the struct
    pub byte_size: u32,
    /// Vtable of the struct. (index, entry).
    /// Dtors will have an index of 0
    pub vtable: Vec<(usize, VtableEntry)>,
    /// Members of the struct
    pub members: Vec<Type0Member>,
}

impl Type0Struct {
    pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
        cu::ensure!(
            self.byte_size == other.byte_size,
            "struct byte sizes are not equal: 0x{:x} != 0x{:x}",
            self.byte_size,
            other.byte_size
        );
        cu::ensure!(
            self.members.len() == other.members.len(),
            "struct member counts are not equal: {} != {}",
            self.members.len(),
            other.members.len()
        );
        let mut mq = MergeQueue::default();
        // for vtable, we might not have the full vtable at this point yet,
        // so we can only check if there are any existing conflicts
        for (i, entry) in &self.vtable {
            if entry.is_dtor() {
                if let Some((_, other_entry)) = other.vtable.iter().find(|(_, e)| e.is_dtor()) {
                    cu::check!(
                        entry.merge_checked(other_entry, &mut mq),
                        "cannot merge struct vtable dtor entry"
                    )?;
                }
                continue;
            }
            if let Some((_, other_entry)) = other.vtable.iter().find(|(x, _)| x == i) {
                cu::check!(
                    entry.merge_checked(other_entry, &mut mq),
                    "cannot merge struct vtable entry {i}"
                )?;
            }
        }

        for (a, b) in std::iter::zip(&self.members, &other.members) {
            cu::check!(a.merge_checked(b, &mut mq), "cannot merge struct member")?;
        }
        merges.extend(mq);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type0Member {
    /// Offset of the member within the struct. 0 For union.
    pub offset: u32,
    /// Name of the member. Could be None for anonymous union as member
    pub name: Option<Arc<str>>,
    /// Type of the member
    pub ty: Goff,
    /// Special-case member
    pub special: Option<SpecialMember>,
}

impl Type0Member {
    pub fn is_base(&self) -> bool {
        matches!(self.special, Some(SpecialMember::Base))
    }
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        self.ty = cu::check!(f(self.ty), "failed to map member goff {}", self.ty)?;
        Ok(())
    }
    pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
        cu::ensure!(
            self.offset == other.offset,
            "member offsets are not equal: {} != {}",
            self.offset,
            other.offset
        );
        cu::ensure!(
            self.name == other.name,
            "member names are not equal: {:?} != {:?}",
            self.name,
            other.name
        );
        cu::ensure!(
            self.special == other.special,
            "member special types are not equal: {:?} != {:?}",
            self.special,
            other.special
        );
        merges.push(self.ty, other.ty);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VtableEntry {
    /// Name of the virtual function
    pub name: Arc<str>,
    /// Types to make up the subroutine type
    pub function_types: Vec<Tree<Goff>>,
}

impl VtableEntry {
    pub fn is_dtor(&self) -> bool {
        self.name.starts_with('~')
    }
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        for x in &mut self.function_types {
            cu::check!(
                x.for_each_mut(|r| {
                    *r = f(*r)?;
                    cu::Ok(())
                }),
                "failed to map vtable entry"
            )?;
        }
        Ok(())
    }
    pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
        cu::ensure!(
            self.name == other.name,
            "vtable function names are not equal: {:?} != {:?}",
            self.name,
            other.name
        );
        cu::ensure!(
            self.function_types.len() == other.function_types.len(),
            "vtable entry subroutine types lengths are not equal"
        );
        let mut mq = MergeQueue::default();
        for (a, b) in std::iter::zip(&self.function_types, &other.function_types) {
            cu::check!(
                tree_merge_checked(a, b, &mut mq),
                "cannot merge vtable entry subroutine arg/ret type"
            )?;
        }
        merges.extend(mq);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpecialMember {
    Base,
    Vfptr,
    Bitfield(u32), // byte_size
}

pub fn tree_merge_checked(a: &Tree<Goff>, b: &Tree<Goff>, merges: &mut MergeQueue) -> cu::Result<()> {
    match (a, b) {
        (Tree::Base(a), Tree::Base(b)) => {
            merges.push(*a, *b);
            Ok(())
        }
        (Tree::Array(a, a_len), Tree::Array(b, b_len)) => {
            cu::ensure!(a_len == b_len, "array lengths are not equal: {a_len} != {b_len}");
            cu::check!(tree_merge_checked(a, b, merges), "cannot merge array element types")
        }
        (Tree::Ptr(a), Tree::Ptr(b)) => {
            cu::check!(tree_merge_checked(a, b, merges), "cannot merge pointer types")
        }
        (Tree::Sub(a_args), Tree::Sub(b_args)) => {
            cu::ensure!(a_args.len() == b_args.len(), "subroutine types lengths are not equal");
            let mut mq = MergeQueue::default();
            for (a, b) in std::iter::zip(a_args, b_args) {
                cu::check!(
                    tree_merge_checked(a, b, &mut mq),
                    "cannot merge subroutine arg/ret type"
                )?;
            }
            merges.extend(mq);
            Ok(())
        }
        (Tree::Ptmd(a, a_inner), Tree::Ptmd(b, b_inner)) => {
            cu::check!(
                tree_merge_checked(a_inner, b_inner, merges),
                "cannot merge ptmd pointee types"
            );
            merges.push(*a, *b);
            Ok(())
        }
        (Tree::Ptmf(a, a_args), Tree::Ptmf(b, b_args)) => {
            cu::ensure!(a_args.len() == b_args.len(), "ptmf types lengths are not equal");
            let mut mq = MergeQueue::default();
            for (a, b) in std::iter::zip(a_args, b_args) {
                cu::check!(
                    tree_merge_checked(a, b, &mut mq),
                    "cannot merge ptmf subroutine arg/ret type"
                )?;
            }
            merges.extend(mq);
            merges.push(*a, *b);
            Ok(())
        }
        (a, b) => {
            cu::bail!("cannot merge type trees of different shapes: {a:?}, and {b:?}");
        }
    }
}
