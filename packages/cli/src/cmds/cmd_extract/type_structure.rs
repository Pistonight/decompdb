use std::collections::BTreeMap;
use std::sync::Arc;

use cu::pre::*;
use tyyaml::{Prim, Tree, TreeRepr};

use crate::config::Config;

// use super::bucket::MergeQueue;
use super::pre::*;

/// Type definitons in Stage2
///
/// - Declarations are merged with the definitions
/// - Typedef names are merged with the definitions
/// - All compile units are linked together
/// - Type layouts are optimized (simplified)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type2 {
    /// Pritimive type
    Prim(Prim),
    /// Enum + typedef names. The name does not include template args. could be anonymous
    Enum(NamespacedName, Type1Enum, Vec<NamespacedTemplatedName>),
    /// Union + typedef names. The name does not include template args. could be anonymous
    Union(NamespacedName, Type0Union, Vec<NamespacedTemplatedName>),
    /// Struct + typedef names. The name does not include template args. could be anonymous
    Struct(NamespacedName, Type0Struct, Vec<NamespacedTemplatedName>),
}

pub struct Stage1 {
    pub offset: usize,
    pub name: String,
    pub types: GoffMap<Type1>,
    pub config: Arc<Config>,
    pub symbols: BTreeMap<String, SymbolInfo>
}

/// Type definitons in Stage1
///
/// - Aliases are merged & eliminated
/// - Trees are flattened: A Tree::Base definitely points to a 
///   primitive, enum, union or struct.
/// - Typedefs to composite types are eliminated
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type1 {
    /// Pritimive type
    Prim(Prim),
    /// Typedef <other> name; Other is offset in debug info.
    /// Name could have template args
    Typedef(NamespacedTemplatedName, Goff),
    /// Enum. The name does not include template args. could be anonymous
    Enum(Option<NamespacedName>, Type1Enum),
    /// Declaration of an enum.
    /// Name includes template args
    EnumDecl(NamespacedTemplatedName),
    /// Union. The name does not include template args. could be anonymous
    Union(Option<NamespacedName>, Type0Union),
    /// Declaration of union.
    /// Name includes template args
    UnionDecl(NamespacedTemplatedName),
    /// Struct or Class. The name does not include template args. could be anonymous
    Struct(Option<NamespacedName>, Type0Struct),
    /// Declaration of struct or class.
    /// Name includes template args
    StructDecl(NamespacedTemplatedName),
}
impl Type1 {
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> {
        let f: GoffMapFn = Box::new(f);
        match self {
            Type1::Prim(_) => {}
            Type1::Typedef(name, goff) => {
                cu::check!(name.map_goff(&f), "failed to map name for typedef")?;
                *goff = cu::check!(f(*goff), "failed to map typedef goff {goff}")?;
            }
            Type1::Enum(name, _) => {
                if let Some(name) = name {
                    cu::check!(name.map_goff(&f), "failed to map name for enum")?;
                }
            }
            Type1::EnumDecl(name) => {
                cu::check!(name.map_goff(&f), "failed to map name for enum decl")?;
            }
            Type1::Union(name, data) => {
                if let Some(name) = name {
                    cu::check!(name.map_goff(&f), "failed to map name for union")?;
                }
                cu::check!(data.map_goff(&f), "failed to map union")?;
            }
            Type1::UnionDecl(name) => {
                cu::check!(name.map_goff(&f), "failed to map name for union decl")?;
            }
            Type1::Struct(name, data) => {
                if let Some(name) = name {
                    cu::check!(name.map_goff(&f), "failed to map name for struct")?;
                }
                for m in &mut data.members {
                    cu::check!(m.map_goff(&f), "failed to map struct members")?;
                }
                for (_, e) in &mut data.vtable {
                    cu::check!(e.map_goff(&f), "failed to map struct vtable entry")?;
                }
            }
            Type1::StructDecl(name) => {
                cu::check!(name.map_goff(&f), "failed to map name for struct decl")?;
            }
        }
        Ok(())
    }
}

pub struct Stage0 {
    pub offset: usize,
    pub name: String,
    pub types: GoffMap<Type0>,
    pub config: Arc<Config>,
    pub ns: NamespaceMaps,
    pub symbols: BTreeMap<String, SymbolInfo>
}

/// Type definitions in Stage0
///
/// - Trees are not flattened: for example, A Tree::Base could be pointing to a Goff that is a pointer type.
/// - Declarations and typedefs could have templates embedded in the name
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type0 {
    /// Pritimive type
    Prim(Prim),
    /// Typedef <other> name; Other is offset in debug info.
    /// Name could have template args
    Typedef(NamespacedName, Goff),
    /// Enum. The name does not include template args. could be anonymous
    Enum(Option<NamespacedName>, Type0Enum),
    /// Declaration of an enum.
    /// Name includes template args
    EnumDecl(Namespace, NamespacedName),
    /// Union. The name does not include template args. could be anonymous
    Union(Option<NamespacedName>, Type0Union),
    /// Declaration of union.
    /// Name includes template args
    UnionDecl(Namespace, NamespacedName),
    /// Struct or Class. The name does not include template args. could be anonymous
    Struct(Option<NamespacedName>, Type0Struct),
    /// Declaration of struct or class.
    /// Name includes template args
    StructDecl(Namespace, NamespacedName),
    /// Composition of other types
    Tree(Tree<Goff>),
    /// Alias to another type for type layout purpose (basically typedef without a name)
    Alias(Goff),
}

impl Type0 {
    /// Run goff conversion on nested type data
    pub fn map_goff<F: Fn(Goff) -> cu::Result<Goff>>(&mut self, f: F) -> cu::Result<()> { 
        let f: GoffMapFn = Box::new(f);
        match self {
            Type0::Prim(_) => {}
            Type0::Typedef(name, inner) => {
                cu::check!(name.map_goff(&f), "failed to map name for typedef")?;
                *inner = cu::check!(f(*inner), "failed to map typedef -> {inner}")?;
            }
            Type0::Alias(inner) => {
                *inner = cu::check!(f(*inner), "failed to map alias -> {inner}")?;
            }
            Type0::Enum(name, _) => { 
                if let Some(name) = name {
                    cu::check!(name.map_goff(&f), "failed to map type in enum name")?;
                }
            }
            Type0::EnumDecl(ns, name) => {
                cu::check!(ns.map_goff(&f), "failed to map namespace for enum decl")?;
                cu::check!(name.map_goff(&f), "failed to map name for enum decl")?;
            }
            Type0::Union(name, data) => {
                if let Some(name) = name {
                    cu::check!(name.map_goff(&f), "failed to map type in union name")?;
                }
                cu::check!(data.map_goff(&f), "failed to map union")?;
            }
            Type0::UnionDecl(ns, name) => {
                cu::check!(ns.map_goff(&f), "failed to map namespace for union decl")?;
                cu::check!(name.map_goff(&f), "failed to map name for union decl")?;
            }
            Type0::Struct(name, data) => {
                if let Some(name) = name {
                    cu::check!(name.map_goff(&f), "failed to map type in struct name")?;
                }
                cu::check!(data.map_goff(&f), "failed to map struct")?;
            }
            Type0::StructDecl(ns, name) => {
                cu::check!(ns.map_goff(&f), "failed to map namespace for struct decl")?;
                cu::check!(name.map_goff(&f), "failed to map name for struct decl")?;
            }
            Type0::Tree(tree) => {
                cu::check!(
                    tree.for_each_mut(|r| {
                        *r = f(*r)?;
                        cu::Ok(())
                    }),
                    "failed to map tree"
                )?;
            }
        }
        Ok(())
    }

    pub fn mark(&self, self_goff: Goff, marked: &mut GoffSet) {
        match self {
            Type0::Prim(prim) => {
                marked.insert(Goff::prim(*prim));
            }
            Type0::Typedef(_, goff) => {
                marked.insert(*goff);
                marked.insert(self_goff);
            }
            Type0::Union(_, data) => {
                for targ in &data.template_args {
                    targ.mark(marked);
                }
                for member in &data.members {
                    let _: Result<_, _> = member.ty.for_each(|goff| {
                        marked.insert(*goff);
                        Ok(())
                    });
                }
                marked.insert(self_goff);
            }
            Type0::Struct(_, data) => {
                for targ in &data.template_args {
                    let TemplateArg::Type(tree) = targ else {
                        continue;
                    };
                    let _: Result<_, _> =  tree.for_each(|goff| {
                        marked.insert(*goff);
                        Ok(())
                    });
                }
                for (_, ventry) in &data.vtable {
                    for t in &ventry.function_types {
  let _: Result<_, _> =                        t.for_each(|goff| {
                            marked.insert(*goff);
                            Ok(())
                        });
                    }
                }
                for member in &data.members {
                    let _: Result<_, _> =  member.ty.for_each(|goff| {
                        marked.insert(*goff);
                        Ok(())
                    });
                }
                marked.insert(self_goff);
            }
            Type0::Tree(tree) => {
  let _: Result<_, _> =                tree.for_each(|goff| {
                    marked.insert(*goff);
                    Ok(())
                });
            }
            Type0::Alias(goff) => {
                marked.insert(*goff);
            }
            // decls, and enum defn
            _ => {
                marked.insert(self_goff);
            }
        }
    }

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type1Enum {
    /// Base type, used to determine the size
    pub byte_size: u32,
    /// Enumerators of the enum, in the order they appear in DWARF
    pub enumerators: Vec<Enumerator>,
}
impl Type1Enum {
    pub fn merge_from(&mut self, other: &Self) -> cu::Result<()> {
        cu::ensure!(
            self.byte_size == other.byte_size,
            "cannot merge 2 enums of different byte size: 0x{:x} != 0x{:x}",
            self.byte_size,
            other.byte_size
        );
        // we don't have any "partial definitions" for enums observed yet
        cu::ensure!(
            self.enumerators == other.enumerators,
            "cannot merge 2 enums of different enumerators: {:#?}, and {:#?}",
            self.enumerators,
            other.enumerators
        );
        Ok(())
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    /// Template arguments, if any
    pub template_args: Vec<TemplateArg<Goff>>,
    /// Byte size of the union (should be size of the largest member)
    pub byte_size: u32,
    /// Union members. The members must have offset of 0 and special of None
    pub members: Vec<Member>,
}

impl Type0Union {
    pub fn map_goff(&mut self, f: &GoffMapFn) -> cu::Result<()> {
        for targ in &mut self.template_args {
            cu::check!(targ.map_goff(f), "failed to map union template args")?;
        }
        for member in &mut self.members {
            cu::check!(member.map_goff(f), "failed to map union members")?;
        }
        Ok(())
    }

    // pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
    //     self.check_merge_precondition(other)?;
    //     let mut mq = MergeQueue::default();
    //     for (a, b) in std::iter::zip(&self.members, &other.members) {
    //         cu::check!(a.merge_checked(b, &mut mq), "cannot merge union member")?;
    //     }
    //     merges.extend(mq)?;
    //     Ok(())
    // }
    pub fn merge_from(&mut self, other: &mut Self) -> cu::Result<()> {
        self.check_merge_precondition(other)?;
        // we don't have any "partial definitions" for unions observed yet
        for (a, b) in std::iter::zip(&self.members, &other.members) {
            cu::ensure!(
                a.name == b.name,
                "cannot merge 2 unions of different member names: {:?} and {:?}",
                a.name,
                b.name
            );
        }
        Ok(())
    }

    fn check_merge_precondition(&self, other: &Self) -> cu::Result<()> {
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
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Type0Struct {
    /// Template specialization of the struct, if any
    pub template_args: Vec<TemplateArg<Goff>>,
    /// Byte size of the struct
    pub byte_size: u32,
    /// Vtable of the struct. (index, entry).
    /// Dtors will have an index of 0
    pub vtable: Vec<(usize, VtableEntry)>,
    /// Members of the struct
    pub members: Vec<Member>,
}

impl Type0Struct {
    /// Run goff conversion on nested type data
    pub fn map_goff(&mut self, f: &GoffMapFn) -> cu::Result<()> { 
        for targ in &mut self.template_args {
            cu::check!(targ.map_goff(f), "failed to map struct template args")?;
        }
        for (_, ventry) in &mut self.vtable {
            cu::check!(ventry.map_goff(f), "failed to map struct vtable entries")?;
        }
        for member in &mut self.members {
            cu::check!(member.map_goff(f), "failed to map struct members")?;
        }
        Ok(())
    }

    // pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
    //     self.check_merge_precondition(other)?;
    //     let mut mq = MergeQueue::default();
    //     // for vtable, we might not have the full vtable at this point yet,
    //     // so we can only check if there are any existing conflicts
    //     for (i, entry) in &self.vtable {
    //         if entry.is_dtor() {
    //             if let Some((_, other_entry)) = other.vtable.iter().find(|(_, e)| e.is_dtor()) {
    //                 cu::check!(
    //                     entry.merge_checked(other_entry, &mut mq),
    //                     "cannot merge struct vtable dtor entry"
    //                 )?;
    //             }
    //             continue;
    //         }
    //         if let Some((_, other_entry)) = other.vtable.iter().find(|(x, _)| x == i) {
    //             cu::check!(
    //                 entry.merge_checked(other_entry, &mut mq),
    //                 "cannot merge struct vtable entry {i}"
    //             )?;
    //         }
    //     }
    //
    //     for (a, b) in std::iter::zip(&self.members, &other.members) {
    //         cu::check!(a.merge_checked(b, &mut mq), "cannot merge struct member")?;
    //     }
    //     merges.extend(mq)?;
    //     Ok(())
    // }

    pub fn merge_from(&mut self, other: &mut Self) -> cu::Result<()> {
        self.check_merge_precondition(other)?;
        // we don't have any "partial definitions" for structs observed yet
        for (a, b) in std::iter::zip(&self.members, &other.members) {
            cu::ensure!(
                a.name == b.name,
                "cannot merge 2 structs of different member names: {:?} and {:?}",
                a.name,
                b.name
            );
            cu::ensure!(
                a.offset == b.offset,
                "cannot merge 2 structs of different member offsets: {:?} and {:?}",
                a.offset,
                b.offset
            );
            cu::ensure!(
                a.special == b.special,
                "cannot merge 2 structs of different member specials: {:?} and {:?}",
                a.special,
                b.special
            );
        }

        // merge vtables
        // clone so if error, we don't change anything
        let mut new_vtable = self.vtable.clone();
        for (i, other_entry) in &other.vtable {
            if other_entry.is_dtor() {
                if let Some((_, self_entry)) = self.vtable.iter().find(|(_, e)| e.is_dtor()) {
                    cu::ensure!(
                        other_entry.name == self_entry.name,
                        "cannot merge vtable dtor entries of different names: {:?} and {:?}",
                        other_entry.name,
                        self_entry.name
                    );
                } else {
                    new_vtable.push((*i, other_entry.clone()));
                }
                continue;
            }
            if let Some((_, self_entry)) = self.vtable.iter().find(|(j, _)| i == j) {
                cu::ensure!(
                    other_entry.name == self_entry.name,
                    "cannot merge vtable entries of different names, at index {i}: {:?} and {:?}",
                    other_entry.name,
                    self_entry.name
                );
            } else {
                new_vtable.push((*i, other_entry.clone()));
            }
        }
        new_vtable.sort_by_key(|x| x.0);
        self.vtable = new_vtable;

        Ok(())
    }

    fn check_merge_precondition(&self, other: &Self) -> cu::Result<()> {
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
        Ok(())
    }
}

/// A struct or union member
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Member {
    /// Offset of the member within the struct. 0 For union.
    pub offset: u32,
    /// Name of the member. Could be None for anonymous typed member
    pub name: Option<Arc<str>>,
    /// Type of the member. Might be unflattened, depending on the stage
    pub ty: Tree<Goff>,
    /// Special-case member, None for union
    pub special: Option<SpecialMember>,
}
impl Member {
    pub fn is_base(&self) -> bool {
        matches!(self.special, Some(SpecialMember::Base))
    }

    /// Run goff conversion on nested type data
    pub fn map_goff(&mut self, f: &GoffMapFn) -> cu::Result<()> { 
        cu::check!(
            self.ty.for_each_mut(|r| {
                *r = f(*r)?;
                cu::Ok(())
            }),
            "failed to map member type"
        )?;
        Ok(())
    }
}

// impl Member {
//     pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
//         cu::ensure!(
//             self.offset == other.offset,
//             "member offsets are not equal: {} != {}",
//             self.offset,
//             other.offset
//         );
//         cu::ensure!(
//             self.name == other.name,
//             "member names are not equal: {:?} != {:?}",
//             self.name,
//             other.name
//         );
//         cu::ensure!(
//             self.special == other.special,
//             "member special types are not equal: {:?} != {:?}",
//             self.special,
//             other.special
//         );
//         merges.push(self.ty, other.ty)?;
//         Ok(())
//     }
// }

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
    // pub fn merge_checked(&self, other: &Self, merges: &mut MergeQueue) -> cu::Result<()> {
    //     cu::ensure!(
    //         self.name == other.name,
    //         "vtable function names are not equal: {:?} != {:?}",
    //         self.name,
    //         other.name
    //     );
    //     cu::ensure!(
    //         self.function_types.len() == other.function_types.len(),
    //         "vtable entry subroutine types lengths are not equal"
    //     );
    //     let mut mq = MergeQueue::default();
    //     for (a, b) in std::iter::zip(&self.function_types, &other.function_types) {
    //         cu::check!(
    //             tree_merge_checked(a, b, &mut mq),
    //             "cannot merge vtable entry subroutine arg/ret type"
    //         )?;
    //     }
    //     merges.extend(mq)?;
    //     Ok(())
    // }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpecialMember {
    Base,
    Vfptr,
    Bitfield(u32), // byte_size
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NamespacedTemplatedName {
    /// The untemplated base name (with namespace)
    pub base: NamespacedName,
    /// The template types
    pub templates: Vec<TemplateArg<NamespacedTemplatedName>>,
}
impl NamespacedTemplatedName {
    pub fn new(base: NamespacedName) -> Self {
        Self {
            base,
            templates: vec![],
        }
    }
    pub fn with_templates(base: NamespacedName, templates: Vec<TemplateArg<Self>>) -> Self {
        Self { base, templates }
    }
    /// Run goff conversion on nested type data
    pub fn map_goff(&mut self, f: &GoffMapFn) -> cu::Result<()> { 
        self.base.map_goff(f)?;
        for targ in &mut self.templates {
            targ.map_goff(&f)?;
        }
        Ok(())
    }
}
impl TreeRepr for NamespacedTemplatedName {
    fn serialize_spec(&self) -> cu::Result<String> {
        Ok(json::stringify(self)?)
    }
    fn deserialize_void() -> Self {
        Self::new(NamespacedName::unnamespaced("void"))
    }
    fn deserialize_spec(spec: &str) -> cu::Result<Self> {
        Ok(json::parse(spec)?)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Display, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum TemplateArg<T: TreeRepr> {
    /// Constant value. Could also be boolean (0=false, 1=true)
    #[display("{}", _0)]
    Const(i64),
    /// Type value. Could be unflattened depending on the stage
    #[display("{}", _0)]
    Type(Tree<T>),

    /// A constant value assigned by compiler (like a function address)
    #[display("[static]")]
    StaticConst
}

impl TemplateArg<Goff> {
    /// Run goff conversion on nested type data
    pub fn map_goff(&mut self, f: &GoffMapFn) -> cu::Result<()> { 
        let Self::Type(tree) = self else {
            return Ok(());
        };
        cu::check!(
            tree.for_each_mut(|r| {
                *r = f(*r)?;
                cu::Ok(())
            }),
            "failed to map template arg type"
        )?;
        Ok(())
    }

    pub fn mark(&self, marked: &mut GoffSet) {
        let TemplateArg::Type(tree) = self else {
            return;
        };
        let _: Result<_, _> = tree.for_each(|goff| {
            marked.insert(*goff);
            Ok(())
        });
    }
}

impl TemplateArg<NamespacedTemplatedName> {
    /// Run goff conversion on nested type data
    pub fn map_goff(&mut self, f: &GoffMapFn) -> cu::Result<()> { 
        let Self::Type(tree) = self else {
            return Ok(());
        };
        cu::check!(
            tree.for_each_mut(|r| {
                r.map_goff(f)
            }),
            "failed to map template arg name"
        )?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolInfo {
    /// Address of the symbol (offset in the original binary)
    pub address: u32,
    /// Name for linking (linkage name)
    pub link_name: String,
    /// Type of the symbol. For functions, this is a Tree::Sub.
    /// Could be unflattened depending on the stage.
    pub ty: Tree<Goff>,
    /// Function parameter names, if the symbol is a function.
    /// Empty string could exists for unnamed parameters,
    /// depending on the stage.
    pub param_names: Vec<String>,
    /// Function template instantiation
    pub template_args: Vec<TemplateArg<Goff>>,
}

impl SymbolInfo {
    pub fn new_data(linkage_name: String, ty: Goff) -> Self {
        Self { 
            address: 0,
            link_name: linkage_name,
            ty: Tree::Base(ty),
            // is_func: false, 
            param_names: vec![],
            template_args: Default::default(),
        }
    }
    pub fn new_func(
        linkage_name: String, 
        types: Vec<Tree<Goff>>,
        mut param_names: Vec<String>,
        template_args: Vec<TemplateArg<Goff>>
    ) -> Self {
        // fill in empty param names
        let mut changes = vec![];
        for (i, name) in param_names.iter().enumerate() {
            if !name.is_empty() {
                continue;
            }
            let mut j = i;
            let mut new_name = format!("a{j}");
            while param_names.iter().any(|x| x == &new_name) {
                j+=1;
                new_name = format!("a{j}");
            }
            changes.push((i, new_name));
        }
        for (i, name) in changes {
            param_names[i] = name;
        }
        Self { 
            address: 0,
            link_name: linkage_name,
            ty: Tree::Sub(types),
            param_names,
            template_args,
        }
    }
    /// Run goff conversion on nested type data
    pub fn map_goff(&mut self, f: &GoffMapFn) -> cu::Result<()> { 
        cu::check!(
            self.ty.for_each_mut(|r| {
                *r = f(*r)?;
                cu::Ok(())
            }),
            "failed to map symbol type"
        )?;

        for targ in &mut self.template_args {
            cu::check!(targ.map_goff(&f), "failed to map symbol template args")?;
        }

        Ok(())
    }
    pub fn mark(&self, marked: &mut GoffSet) {
        let _: Result<_, _> = self.ty.for_each(|goff| {
            marked.insert(*goff);
            Ok(())
        });
        for targ in &self.template_args {
            targ.mark(marked);
        }
    }
    pub fn merge(&mut self, other: &Self) -> cu::Result<()> {
        cu::ensure!(self.link_name == other.link_name, "cannot merge symbol info with different linkage names: {} != {}", self.link_name, other.link_name);
        cu::ensure!(self.ty == other.ty, "cannot merge symbol info with different types");
        cu::ensure!(self.param_names == other.param_names, "cannot merge symbol info with different param_names");
        // some info does not have template args, in which case we fill it in
        match (self.template_args.is_empty(), other.template_args.is_empty()) {
            (_, true) => {},
            (true, false) => {
                self.template_args = other.template_args.clone();
            }
            (false, false) => {
                cu::ensure!(self.template_args == other.template_args, "cannot merge symbol info with different template_args");
            }
        }
        Ok(())
    }
}

// pub fn tree_merge_checked(a: &Tree<Goff>, b: &Tree<Goff>, merges: &mut MergeQueue) -> cu::Result<()> {
//     match (a, b) {
//         (Tree::Base(a), Tree::Base(b)) => {
//             merges.push(*a, *b)?;
//             Ok(())
//         }
//         (Tree::Array(a, a_len), Tree::Array(b, b_len)) => {
//             cu::ensure!(a_len == b_len, "array lengths are not equal: {a_len} != {b_len}");
//             cu::check!(tree_merge_checked(a, b, merges), "cannot merge array element types")
//         }
//         (Tree::Ptr(a), Tree::Ptr(b)) => {
//             cu::check!(tree_merge_checked(a, b, merges), "cannot merge pointer types")
//         }
//         (Tree::Sub(a_args), Tree::Sub(b_args)) => {
//             cu::ensure!(a_args.len() == b_args.len(), "subroutine types lengths are not equal");
//             let mut mq = MergeQueue::default();
//             for (a, b) in std::iter::zip(a_args, b_args) {
//                 cu::check!(
//                     tree_merge_checked(a, b, &mut mq),
//                     "cannot merge subroutine arg/ret type"
//                 )?;
//             }
//             merges.extend(mq)?;
//             Ok(())
//         }
//         (Tree::Ptmd(a, a_inner), Tree::Ptmd(b, b_inner)) => {
//             cu::check!(
//                 tree_merge_checked(a_inner, b_inner, merges),
//                 "cannot merge ptmd pointee types"
//             )?;
//             merges.push(*a, *b)?;
//             Ok(())
//         }
//         (Tree::Ptmf(a, a_args), Tree::Ptmf(b, b_args)) => {
//             cu::ensure!(a_args.len() == b_args.len(), "ptmf types lengths are not equal");
//             let mut mq = MergeQueue::default();
//             for (a, b) in std::iter::zip(a_args, b_args) {
//                 cu::check!(
//                     tree_merge_checked(a, b, &mut mq),
//                     "cannot merge ptmf subroutine arg/ret type"
//                 )?;
//             }
//             merges.extend(mq)?;
//             merges.push(*a, *b)?;
//             Ok(())
//         }
//         (a, b) => {
//             cu::bail!("cannot merge type trees of different shapes: {a:#?}, and {b:#?}");
//         }
//     }
// }
