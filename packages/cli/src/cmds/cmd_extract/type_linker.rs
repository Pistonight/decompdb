use std::collections::BTreeMap;
use std::collections::BTreeSet;

use cu::pre::*;
use fxhash::FxHashMap;

use super::pre::*;
use super::type_structure::*;
use super::BucketMap;

pub fn link_raw_types(raw_types: GoffMap<Type0>) {
}

pub struct RawLinker {
    pub linked: GoffBuckets,
    pub name_map: FxHashMap<String, BTreeSet<Goff>>
}

pub enum RawLinkRule {
    /// Link the 2 types
    DoLink(Goff, Goff)
}

