use cu::pre::*;
use std::collections::{BTreeMap, BTreeSet};

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

    pub fn add_key(&mut self, key: K, new_key: K) -> cu::Result<()> {
        let (i, bucket) = cu::check!(self.bucket_mut(key), "error in add_key: cannot find bucket with key {key}")?;
        bucket.other_keys.insert(new_key);
        self.key_to_index.insert(new_key, i);
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
            for key in bucket.other_keys {
                self.key_to_index.insert(key, i);
            }
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
