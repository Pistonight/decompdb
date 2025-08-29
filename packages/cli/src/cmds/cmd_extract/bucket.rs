use cu::pre::*;
use std::collections::{BTreeMap, BTreeSet};

use super::pre::*;

pub struct BucketMap<K, V> {
    key_to_index: BTreeMap<K, usize>,
    buckets: Vec<Option<Bucket<K, V>>>,
}

pub struct Bucket<K, V> {
    canonical: K,
    pub other_keys: BTreeSet<K>,
    pub value: V,
}

pub trait BucketValue {
    /// Absort value from another bucket
    fn absorb(&mut self, other: Self) -> cu::Result<()>;
}

impl BucketValue for () {
    fn absorb(&mut self, _: Self) -> cu::Result<()> {
        Ok(())
    }
}

impl<K: Copy + PartialEq + Ord + std::fmt::Display, V: BucketValue> BucketMap<K, V> {
    /// Get a bucket by key. The bucket contains the value and all keys that lead to that bucket
    pub fn get(&self, key: K) -> Option<&Bucket<K, V>> {
        let index = *self.key_to_index.get(&key)?;
        self.buckets.get(index)?.as_ref()
    }

    pub fn get_unwrap(&self, key: K) -> cu::Result<&Bucket<K, V>> {
        cu::check!(self.get(key), "key {key} not found in bucket map")
    }

    pub fn get_mut(&mut self, key: K) -> Option<&mut V> {
        self.bucket_mut(key).map(|(_, x)| &mut x.value)
    }

    fn bucket_mut(&mut self, key: K) -> Option<(usize, &mut Bucket<K, V>)> {
        let index = *self.key_to_index.get(&key)?;
        self.buckets.get_mut(index)?.as_mut().map(|x| (index, x))
    }

    pub fn buckets(&self) -> impl Iterator<Item = &Bucket<K, V>> {
        self.buckets.iter().filter_map(|x| x.as_ref())
    }

    pub fn buckets_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.buckets.iter_mut().filter_map(|x| x.as_mut().map(|x| &mut x.value))
    }

    pub fn add_key(&mut self, key: K, new_key: K) -> cu::Result<()> {
        let (i, bucket) = cu::check!(self.bucket_mut(key), "error in add_key: cannot find bucket with key {key}")?;
        bucket.other_keys.insert(new_key);
        self.key_to_index.insert(new_key, i);
        cu::check!(self.check(), "error in add_key: bucket map is invalid after adding new key {new_key} to key {key}")?;
        Ok(())
    }

    pub fn remove(&mut self, key: K) -> Option<Bucket<K, V>> {
        let index = *self.key_to_index.get(&key)?;
        let bucket = self.buckets.get_mut(index)?;
        let bucket = bucket.take();
        if let Some(bucket) = &bucket {
            self.key_to_index.remove(&bucket.canonical_key());
            for k in &bucket.other_keys {
                self.key_to_index.remove(k);
            }
        }
        bucket
    }

    pub fn canonical_key_map(&self) -> BTreeMap<K, K> {
        self.key_to_index.iter().map(|(k, i)| {
            let ck = self.buckets.get(*i).unwrap().as_ref().unwrap().canonical_key();
            (*k, ck)
        }).collect()
    }

    pub fn canonical_key(&self, key: K) -> K {
        let Some(bucket) = self.get(key) else {
            return key;
        };
        bucket.canonical_key()
    }

    /// Insert a new bucket with key. error if the key is already pointing to a bucket
    pub fn insert_new(&mut self, key: K, value: V) -> cu::Result<()> {
        self.insert_new_internal(key, value)?;
        Ok(())
    }
    fn insert_new_internal(&mut self, key: K, value: V) -> cu::Result<usize> {
        use std::collections::btree_map::Entry;
        match self.key_to_index.entry(key) {
            Entry::Vacant(e) => {
                let new_bucket = Bucket::new(key, value);
                // find a place to put it
                let new_index = self.buckets.iter().position(|x| x.is_none());
                let new_index = match new_index {
                    Some(i) => {
                        self.buckets[i] = Some(new_bucket);
                        i
                    }
                    None => {
                        let len = self.buckets.len();
                        self.buckets.push(Some(new_bucket));
                        len
                    }
                };
                e.insert(new_index);
                Ok(new_index)
            }
            Entry::Occupied(_) => {
                cu::bail!("key {key} is already in a bucket!");
            }
        }
    }

    /// Merge the from key into the to key. No-op if they are already in the same bucket.
    /// Error if any of the key doesn't exist.
    /// The values are also merged by calling absorb
    pub fn merge(&mut self, from: K, to: K) -> cu::Result<()> {
        let to_index = cu::check!(
            self.key_to_index.get(&to),
            "error in merge: key {to} is not in the bucket"
        )?;
        let to_index = *to_index;
        let from_index = cu::check!(
            self.key_to_index.get(&from),
            "error in merge: key {from} is not in the bucket"
        )?;
        let from_index = *from_index;
        if to_index == from_index {
            return Ok(());
        }
        let from_bucket = cu::check!(
            self.buckets.get_mut(from_index),
            "error in merge: key {from} points to invalid index"
        )?;
        let from_bucket = from_bucket.take();
        let from_bucket = cu::check!(from_bucket, "error in merge: key {from} points to null bucket")?;
        let to_bucket = cu::check!(
            self.buckets.get_mut(to_index),
            "error in merge: key {to} points to invalid index"
        )?;
        let to_bucket = cu::check!(to_bucket.as_mut(), "error in merge: key {to} points to null bucket")?;
        to_bucket.value.absorb(from_bucket.value)?;

        // update index
        self.key_to_index.insert(from_bucket.canonical, to_index);
        for k in &from_bucket.other_keys {
            self.key_to_index.insert(*k, to_index);
        }

        // update keys
        to_bucket.other_keys.insert(from_bucket.canonical);
        to_bucket.other_keys.extend(from_bucket.other_keys);
        Ok(())
    }

    pub fn merge_to(&mut self, from: K, value: V, to: K) -> cu::Result<()> {
        if self.key_to_index.contains_key(&from) {
            cu::bail!("error in merge_to: key {from} is already in the map");
        }
        let index = cu::check!(
            self.key_to_index.get(&to),
            "error in merge_to: key {to} is not in the bucket"
        )?;
        let index = *index;
        let bucket = cu::check!(
            self.buckets.get_mut(index),
            "error in merge_to: key {to} points to invalid index"
        )?;
        let bucket = cu::check!(bucket.as_mut(), "error in merge_to: key {to} points to null bucket")?;
        bucket.other_keys.insert(from);
        bucket.value.absorb(value)?;
        self.key_to_index.insert(from, index);

        Ok(())
    }

    /// Extend this map with another. The 2 maps must have disjoint key sets
    pub fn extend(&mut self, other: Self) -> cu::Result<()> {
        for bucket in other.buckets.into_iter().flatten() {
            let i = self.insert_new_internal(bucket.canonical_key(), bucket.value).context("error in extend: the 2 maps have overlapping key sets")?;
            let new_bucket = self.buckets.get_mut(i).unwrap().as_mut().unwrap();
            new_bucket.other_keys = bucket.other_keys;
            for key in &new_bucket.other_keys {
                self.key_to_index.insert(*key, i);
            }
        }
        cu::check!(self.check(), "error in extend: bucket map is invalid after extend")?;
        Ok(())
    }
    #[cfg(not(debug_assertions))]
    pub fn check(&self) -> cu::Result<()> {
        Ok(())
    }

    #[cfg(debug_assertions)]
    pub fn check(&self) -> cu::Result<()> {
        let mut keys_in_buckets = BTreeSet::new();
        for (i, bucket) in self.buckets.iter().enumerate() {
            match bucket {
                None => {
                    for (k, ki) in &self.key_to_index {
                        if i == *ki {
                            cu::bail!("bucket map check failed: key {k} references unlinked bucket {i}");
                        }
                    }
                }
                Some(bucket) => {
                    let bucket_key = bucket.canonical_key();
                    if !keys_in_buckets.insert(bucket_key) {
                        cu::bail!("key {bucket_key} is in multiple buckets");
                    }
                    if bucket.other_keys.contains(&bucket_key) {
                        cu::bail!("bucket map check failed: bucket {i} has its canonical key {bucket_key} inside other_keys");
                    }
                    for k in &bucket.other_keys {
                        if !keys_in_buckets.insert(*k) {
                            cu::bail!("key {bucket_key} is in multiple buckets");
                        }
                        if self.key_to_index.get(k).is_none() {
                            cu::bail!("bucket map check failed: bucket {i} with key {bucket_key} has key {k}, but the key {k} is unlinked");
                        }
                    }
                    for (k, ki) in &self.key_to_index {
                        let is_this_bucket = i == *ki;
                        if bucket_key == *k || bucket.other_keys.contains(k) {
                            if !is_this_bucket {
                                cu::bail!("bucket map check failed: bucket {i} with key {bucket_key} has key {k}, but {k} links to bucket {ki}");
                            }
                            continue;
                        }
                        if is_this_bucket {
                            cu::bail!("bucket map check failed: key {k} links to bucket {i}, but the key {k} is not in that bucket");
                        }
                    }
                }
            }
        }
        let keys_in_map: BTreeSet<_> = self.key_to_index.keys().copied().collect();
        for k in keys_in_map.difference(&keys_in_buckets) {
            cu::bail!("key {k} is tracked in the map but not in any bucket");
        }
        for k in keys_in_buckets.difference(&keys_in_map) {
            cu::bail!("key {k} is tracked in a bucket but not in the map");
        }
        Ok(())
    }
}

impl<K: Copy + Ord, V> Bucket<K, V> {
    fn new(key: K, value: V) -> Self {
        Self {
            canonical: key,
            other_keys: Default::default(),
            value,
        }
    }
    pub fn canonical_key(&self) -> K {
        self.canonical
    }
}

impl<K, V> Default for BucketMap<K, V> {
    fn default() -> Self {
        Self {
            key_to_index: Default::default(),
            buckets: Default::default(),
        }
    }
}

impl<K: std::fmt::Debug + Copy, V: std::fmt::Debug> std::fmt::Debug for BucketMap<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(self.buckets.iter().filter_map(|x| {
                let bucket = x.as_ref()?;
                let mut keys = Vec::with_capacity(bucket.other_keys.len() + 1);
                keys.push(bucket.canonical);
                keys.extend(bucket.other_keys.iter().copied());
                Some((keys, &bucket.value))
            }))
            .finish()
    }
}

#[derive(Default)]
pub struct GoffBuckets {
    index_map: BTreeMap<Goff, usize>,
    buckets: Vec<BTreeSet<Goff>>,
    free_list: Vec<usize>
}

impl GoffBuckets {
    /// Check if `k` is in any bucket
    pub fn contains(&self, k: Goff) -> bool {
        self.primary(k).is_some()
    }

    /// Get the smallest goff in the bucket `k` is in.
    /// Returns the input if it's not in any bucket
    pub fn primary_fallback(&self, k: Goff) -> Goff {
        self.primary(k).unwrap_or(k)
    }

    /// Get the primary goff in the bucket `k` is in, if it is in any bucket.
    /// If the goff corresponds to a primitive, then the canonical value
    /// for the primitive is returned. Otherwise, the smallest goff is returned.
    pub fn primary(&self, k: Goff) -> Option<Goff> {
        let i = *self.index_map.get(&k)?;
        self.primary_from_index(i)
    }

    pub fn primaries(&self) -> impl Iterator<Item=Goff> {
        self.buckets.iter().filter(|x| !x.is_empty())
        .filter_map(Self::primary_from_bucket)
    }

    /// Insert a new goff to the buckets.
    ///
    /// Returns `None` if the goff is inserted as a new bucket.
    /// Returns `Some(k)` if the goff is already in a bucket,
    /// and `k` is the smallest `Goff` in that bucket.
    #[must_use]
    pub fn insert(&mut self, k: Goff) -> Option<Goff> {
        match self.index_map.get(&k).copied() {
            None => {
                let i = self.make_new_bucket();
                self.buckets[i].insert(k);
                self.index_map.insert(k, i);
                None
            },
            Some(i) => {
                self.primary_from_index(i)
            }
        }
    }

    /// Returns Some(k) where k is the primary of the bucket at index i
    #[inline(always)]
    fn primary_from_index(&self, i: usize) -> Option<Goff> {
        if cfg!(debug_assertions) {
            let bucket = &self.buckets[i];
            Self::primary_from_bucket(bucket)
        } else {
            let bucket = self.buckets.get(i)?;
            Self::primary_from_bucket(bucket)
        }
    }

    #[inline(always)]
    fn primary_from_bucket(bucket: &BTreeSet<Goff>) -> Option<Goff> {
        if cfg!(debug_assertions) {
            let largest = *bucket.last().expect("unexpected empty bucket");
            if largest.is_prim() {
                return Some(largest);
            }
            Some(bucket.first().copied().expect("unexpected empty bucket"))
        } else {
            let largest = *bucket.last()?;
            if largest.is_prim() {
                return Some(largest);
            }
            bucket.first().copied()
        }
    }

    /// Merge the 2 buckets `k1` and `k2`, insert the bucket as new if `k1`
    /// or `k2` are not in any buckets
    pub fn merge(&mut self, k1: Goff, k2: Goff) -> cu::Result<()> {
        let k1_primary = self.insert(k1).unwrap_or(k1);
        let k2_primary = self.insert(k2).unwrap_or(k2);
        if k1_primary == k2_primary {
            // already in the same bucket
            return Ok(());
        }
        let (k_to, k_from) = cu::check!(pick_bucket_primary_key(k1_primary, k2_primary), "merge: failed to pick primary key when merging {k1} and {k2}")?;
        let i_from = *cu::check!(self.index_map.get(&k_from), "merge: unexpected k_from not found: {k_from}")?;
        let i_to = *cu::check!(self.index_map.get(&k_to), "merge: unexpected k_to not found: {k_to}")?;
        let keys_from = self.remove_bucket(i_from);
        for k in &keys_from {
            self.index_map.insert(*k, i_to);
        }
        self.buckets[i_to].extend(keys_from);
        Ok(())
    }

    fn make_new_bucket(&mut self) -> usize {
        match self.free_list.pop() {
            None => {
                let i = self.buckets.len();
                self.buckets.push(Default::default());
                i
            }
            Some(i) => i,
        }
    }

    fn remove_bucket(&mut self, i: usize) -> BTreeSet<Goff> {
        self.free_list.push(i);
        std::mem::take(&mut self.buckets[i])
    }

}

fn pick_bucket_primary_key(k1: Goff, k2: Goff) -> cu::Result<(Goff, Goff)> {
    match (k1.is_prim(), k2.is_prim()) {
        (true, true) => cu::bail!("cannot have 2 different primitive goffs in the same bucket: {k1} and {k2}"),
        (true, false) => Ok((k1, k2)),
        (false, true) => Ok((k2, k1)),
        (false, false) => Ok(if k1 < k2 { (k1, k2) } else { (k2, k1) })
    }
}
