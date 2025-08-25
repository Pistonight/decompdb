use cu::pre::*;
use std::collections::{BTreeMap, BTreeSet};

pub struct BucketMap<K, V> {
    key_to_index: BTreeMap<K, usize>,
    buckets: Vec<Option<Bucket<K, V>>>,
}

pub struct Bucket<K, V> {
    canonical: K,
    other_keys: BTreeSet<K>,
    pub value: V,
}

pub trait BucketValue {
    /// Absort value from another bucket
    fn absorb(&mut self, other: Self) -> cu::Result<()>;
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
        let index = *self.key_to_index.get(&key)?;
        Some(&mut self.buckets.get_mut(index)?.as_mut()?.value)
    }

    pub fn buckets(&self) -> impl Iterator<Item = &Bucket<K, V>> {
        self.buckets.iter().filter_map(|x| x.as_ref())
    }

    pub fn canonical_key(&self, key: K) -> K {
        let Some(bucket) = self.get(key) else {
            return key;
        };
        bucket.canonical_key()
    }

    /// Insert a new bucket with key. error if the key is already pointing to a bucket
    pub fn insert_new(&mut self, key: K, value: V) -> cu::Result<()> {
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
                Ok(())
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
