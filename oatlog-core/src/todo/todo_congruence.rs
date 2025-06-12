use crate::runtime::*;
use std::array::from_fn;
use std::collections::{HashMap, HashSet};

eclass_wrapper_ty!(M);

fn cc(index: &mut [HashMap<(M, M), M>; 4], uf: &mut UnionFind<M>) {
    let mut uproot_index: [HashMap<M, (M, M)>; 4] = from_fn(|_| HashMap::new());
}

fn mk_index(data: &mut [(u32, u32, u32)]) -> HashMap<u32, HashMap<u32, HashSet<u32>>> {
    data.sort();
    let mut map: HashMap<u32, HashMap<u32, HashSet<u32>>> = HashMap::new();
    let mut i = 0;
    while i < data.len() {
        let mut j = i + 1;
        while j < data.len() && data[i].0 == data[j].0 {
            j += 1;
        }
        map.insert(data[i].0, {
            let mut map = HashMap::new();
            let mut i = 0;
            while i < data.len() {
                let mut j = i + 1;
                while j < data.len() && data[i].1 == data[j].1 {
                    j += 1;
                }
                map.insert(data[i].1, data[i..j].iter().map(|x| x.2).collect());
                i = j;
            }
            map
        });
        i = j;
    }
    map
}

struct FdHashIndex<Key, Value> {
    inner: HashMap<Key, Value>,
}

struct HashVecIndex<Key, Value> {
    inner: HashMap<Key, Vec<Value>>,
}

struct IndexedSortedListIndexNonCompact<Key, Row> {
    map: HashMap<Key, (u32, u32)>,
    list: Vec<Row>,
}

impl<Key: Ord + Hash, Row: KeyExtract<Key = Key> + Copy>
    IndexedSortedListIndexNonCompact<Key, Row>
{
    fn append(&mut self, new_rows: &[Row]) {
        self.list.extend(new_rows.iter().copied());
        self.list.sort_by_key(|row| Row::extract_key(*row));
        //                          ^^^^^^^^^^^^^^^^^^^^^^
        //                         what if we sort by hash?
        self.map.clear();
        let n = self.list.len();
        let mut i = 0;
        while i < n {
            let key_i = Row::extract_key(self.list[i]);
            let mut j = i + 1;
            while j < n && Row::extract_key(self.list[j]) == key_i {
                j += 1;
            }
            self.map.insert(key_i, (i as u32, j as u32));
            i = j;
        }
    }
}

// typically:
// Key = (Math,)
// Value = (Math, Math,)

trait KeyExtract {
    type Key;
    type Value;
    fn extract_key(self) -> Self::Key;
    fn extract_value(self) -> Self::Value;
}

struct IndexedSortedListIndex<Key, Value> {
    map: HashMap<Key, (u32, u32)>,
    list: Vec<Value>,
}

impl<Key: Ord + Hash, Value> IndexedSortedListIndex<Key, Value> {
    fn reconstruct<Row: KeyExtract<Key = Key, Value = Value> + Copy>(&mut self, rows: &mut [Row]) {
        rows.sort_by_key(|row| Row::extract_key(*row));
        //                     ^^^^^^^^^^^^^^^^^^^^^^
        //                      we could sort by hash
        self.map.clear();
        let n = rows.len();
        let mut i = 0;
        while i < n {
            let key_i = Row::extract_key(rows[i]);
            let mut j = i + 1;
            while j < n && Row::extract_key(rows[j]) == key_i {
                j += 1;
            }
            self.map.insert(key_i, (i as u32, j as u32));
            i = j;
        }
        self.list.clear();
        self.list
            .extend(rows.iter().copied().map(Row::extract_value));
    }
}

mod update {
    use hashbrown_14::HashMap;
    use hashbrown_14::hash_map::Entry;
    fn update_begin_basic(
        map: &mut HashMap<(u32, u32), u32>,
        find: impl Fn(u32) -> u32,
        union: impl Fn(u32, u32) -> u32,
        insertions: &[((u32, u32), u32)],
    ) {
        for &(key, value) in insertions {
            let key = (find(key.0), find(key.1));
            match map.entry(key) {
                Entry::Occupied(mut entry) => {
                    let entry = entry.get_mut();
                    *entry = union(value, *entry);
                }
                Entry::Vacant(entry) => {
                    entry.insert(find(value));
                }
            }
        }
    }

    fn prefetch<T>(addr: *const T) {
        use core::arch::x86_64::{_MM_HINT_T0, _mm_prefetch};
        unsafe { _mm_prefetch::<_MM_HINT_T0>(addr as _) }
    }

    fn find(repr: &[u32], mut i: u32) -> u32 {
        loop {
            let i_old = i;
            i = repr[i as usize];
            if i != i_old {
                break i;
            }
        }
    }
    fn union(repr: &mut [u32], a: u32, b: u32) -> u32 {
        let (a, b) = (find(repr, a), find(repr, b));

        if a == b {
            return a;
        }
        let old = u32::min(a, b);
        let new = u32::max(a, b);
        repr[new as usize] = old;
        old
    }

    fn update_begin_no_iterator(
        map: &mut HashMap<(u32, u32), u32>,
        repr: &mut [u32],
        insertions: &[((u32, u32), u32)],
    ) {
        let n = insertions.len();
        let mut i = 0;
        while i < n {
            let (key, value) = insertions[i];
            let key = (find(repr, key.0), find(repr, key.1));
            match map.entry(key) {
                Entry::Occupied(mut entry) => {
                    let entry = entry.get_mut();
                    *entry = union(repr, value, *entry);
                }
                Entry::Vacant(entry) => {
                    entry.insert(find(repr, value));
                }
            }
            i += 1;
        }
    }

    fn update_begin_unroll(
        map: &mut HashMap<(u32, u32), u32>,
        find: impl Fn(u32) -> u32,
        union: impl Fn(u32, u32) -> u32,
        insertions: &[((u32, u32), u32)],
    ) {
        const PREFETCH: usize = 10;
        let n = insertions.len();
        let mut i = 0;
        while i < n {
            let (key, value) = insertions[i];
            let key = (find(key.0), find(key.1));
            match map.entry(key) {
                Entry::Occupied(mut entry) => {
                    let entry = entry.get_mut();
                    *entry = union(value, *entry);
                }
                Entry::Vacant(entry) => {
                    entry.insert(find(value));
                }
            }
        }

        // 1. key = find(key)
        // 2. hash = hash(key)
        // 3. hashmap[hash] (load)
        // 4. find(value)
        // 5. either
        //     * union
        //     * insert

        // 1. (key, value) = insertions[i]
        // 2. prefetch(&repr[key])
        //    prefetch(&repr[value])
        // 3. key = repr[key]
        //    value = repr[value]
        // 4. prefetch(&repr[key])
        //    prefetch(&repr[value])
        // 5. key = repr[key]
        //    value = repr[value]
        // 6. key = find(key)
        // 7. hash = hash(key)
        // 8. prefetch(&map_u8[hash]), prefetch(&map_data[hash])
        // 9. entry = &mut map[hash]
        //    if let Some(entry) = entry {
        //        *entry = Some(union(find(entry), find(value)))
        //    } else {
        //        *entry = Some(find(value))
        //    }

        macro_rules! auto_pipeline {
            ($($tt:tt)*) => {};
        }

        auto_pipeline! {
            {
                let (key, value) = insertions[i];
                let (mut key2, mut value2) = (0, 0);
                if_in_pipeline! {
                    (key2, value2) = insertions[i + FOO];
                }
            }
            save(key, value, key2, value2, s0),
            {
                prefetch(&repr[key]);
                prefetch(&repr[value]);
            },
            save(key, value, s1),
            {
                let key = repr[key];
                let value = repr[value];
            },
            save(key, value, s2),
            {
                let key = find(key);
            },
            save(key, value, s3),
            {
                load(key, value, s3);
                let hash = hash(key);
                save(key, value, hash, s4);
            },
            {
                load(key, value, hash, s4);
                prefetch(&map_u8[hash]);
                prefetch(&map_data[hash]);
                save(key, value, hash, s5);
            },
            {
                ...
            },
        }
    }

    use std::{
        hash::{BuildHasher, Hash},
        mem::replace,
    };

    fn prefetch_key<K: Hash + Copy, V: Copy>(map: &HashMap<K, V>, key: K) {
        if map.is_empty() {
            // required by safety invariant of `RawTable::bucket`.
            // it's probably still safe on x86 to prefetch out of bounds.
            return;
        }

        let hasher = map.hasher();
        let hash = hasher.hash_one(key);
        let raw_table = map.raw_table();
        let bucket_mask = (raw_table.buckets() - 1) as u64;
        let idx = (hash & bucket_mask) as usize;

        // from hashbrown 0.14 docs
        // If mem::size_of::<T>() != 0 then return a pointer to the `element` in the `data part` of the table
        // (we start counting from "0", so that in the expression T[n], the "n" index actually one less than
        // the "buckets" number of our `RawTable`, i.e. "n = RawTable::buckets() - 1"):
        //
        //           `table.bucket(3).as_ptr()` returns a pointer that points here in the `data`
        //           part of the `RawTable`, i.e. to the start of T3 (see `Bucket::as_ptr`)
        //                  |
        //                  |               `base = self.data_end()` points here
        //                  |               (to the start of CT0 or to the end of T0)
        //                  v                 v
        // [Pad], T_n, ..., |T3|, T2, T1, T0, |CT0, CT1, CT2, CT3, ..., CT_n, CTa_0, CTa_1, ..., CTa_m
        //                     ^                                              \__________  __________/
        //        `table.bucket(3)` returns a pointer that points                        \/
        //         here in the `data` part of the `RawTable` (to              additional control bytes
        //         the end of T3)                                              `m = Group::WIDTH - 1`
        //
        // where: T0...T_n  - our stored data;
        //        CT0...CT_n - control bytes or metadata for `data`;
        //        CTa_0...CTa_m - additional control bytes (so that the search with loading `Group` bytes from
        //                        the heap works properly, even if the result of `h1(hash) & self.table.bucket_mask`
        //                        is equal to `self.table.bucket_mask`). See also `RawTableInner::set_ctrl` function.
        //
        // P.S. `h1(hash) & self.table.bucket_mask` is the same as `hash as usize % self.buckets()` because the number
        // of buckets is a power of two, and `self.table.bucket_mask = self.buckets() - 1`.

        // prefetch data
        prefetch(unsafe { raw_table.bucket(idx) }.as_ptr());

        let metadata_start = raw_table.data_end().as_ptr() as *const i8;

        // prefetch metadata
        prefetch(metadata_start.wrapping_add(idx));
    }

    fn update_begin_prefetch(
        map: &mut HashMap<(u32, u32), u32>,
        repr: &mut [u32],
        insertions: &[((u32, u32), u32)],
    ) {
        let n = insertions.len();
        let mut i = 0;
        while i < n {
            if i + 20 < n {
                // prefetch repr
                let (key_prefetch, value_prefetch) = insertions[i + 20];
                prefetch(repr.as_ptr().wrapping_add(key_prefetch.0 as usize));
                prefetch(repr.as_ptr().wrapping_add(key_prefetch.1 as usize));
                prefetch(repr.as_ptr().wrapping_add(value_prefetch as usize));

                // prefetch data
                let (key_prefetch, _) = insertions[i + 10];
                let key = (find(repr, key_prefetch.0), find(repr, key_prefetch.1));
                prefetch_key(map, key);
            }
            let (key, value) = insertions[i];
            let key = (find(repr, key.0), find(repr, key.1));
            match map.entry(key) {
                Entry::Occupied(mut entry) => {
                    let entry = entry.get_mut();
                    *entry = union(repr, value, *entry);
                }
                Entry::Vacant(entry) => {
                    entry.insert(find(repr, value));
                }
            }
            i += 1;
        }
    }

    fn update_basic(
        map: &mut HashMap<(u32, u32), u32>,
        repr: &[u32],
        insertions: &mut Vec<((u32, u32), u32)>,
    ) {
        map.retain(|&(x0, x1), &mut x2| {
            if (repr[x0 as usize] == x0) & (repr[x1 as usize] == x1) & (repr[x2 as usize] == x2) {
                true
            } else {
                insertions.push(((repr[x0 as usize], repr[x1 as usize]), repr[x2 as usize]));
                false
            }
        })
    }

    // TODO: Insertions array can be cleared.
    // TODO: We can do useful work *during* rehash.
    // TODO: Do we *really* need to completely finish canonicalization?
    // TODO: Regular entry actually hashes twice, once for check and once for insert, but we want
    // to insert unconditionally.
    //
    // TODO: On the FD index, it's fine to store (min(a, b), max(a, b), c) instead of (a, b, c)
    // in general, for any index, it is fine to apply a permutation as long as it only touches the
    // *key* part.

    fn update_begin_manual_rehash(
        map: &mut HashMap<(u32, u32), u32>,
        repr: &mut [u32],
        insertions: &mut Vec<((u32, u32), u32)>,
    ) {
        // fill table until just before rehash, and then do rehash and find at the same time.
        while map.capacity() - map.len() > 0 {
            let Some((key, value)) = insertions.pop() else {
                break;
            };
            let key = (find(repr, key.0), find(repr, key.1));

            match map.entry(key) {
                Entry::Occupied(mut entry) => {
                    let entry = entry.get_mut();
                    *entry = union(repr, value, *entry);
                }
                Entry::Vacant(entry) => {
                    entry.insert(find(repr, value));
                }
            }
        }

        // if there are still more insertions, we need to rehash to fit them.
        if !insertions.is_empty() {
            rehash_grow(map, repr, insertions);
        }
    }

    fn update_manual_rehash(
        map: &mut HashMap<(u32, u32), u32>,
        repr: &mut [u32],
        insertions: &mut Vec<((u32, u32), u32)>,
    ) {
        // we *could* treat this as just a regular rehash instead.
        map.retain(|&(x0, x1), &mut x2| {
            if (repr[x0 as usize] == x0) & (repr[x1 as usize] == x1) & (repr[x2 as usize] == x2) {
                true
            } else {
                insertions.push(((repr[x0 as usize], repr[x1 as usize]), repr[x2 as usize]));
                false
            }
        });
        update_begin_manual_rehash(map, repr, insertions);
    }

    fn rehash_grow(
        map: &mut HashMap<(u32, u32), u32>,
        repr: &mut [u32],
        insertions: &mut Vec<((u32, u32), u32)>,
    ) {
        // Internally, hashbrown creates a new allocation when rehashing, so this is
        // essentially equivalent in terms of performance, (I think).
        let needed_capacity = insertions.len() + map.len();
        let old_map = replace(map, HashMap::with_capacity(needed_capacity));

        for (key, value) in old_map.into_iter() {
            let key = (find(repr, key.0), find(repr, key.1));

            match map.entry(key) {
                Entry::Occupied(mut entry) => {
                    let entry = entry.get_mut();
                    *entry = union(repr, value, *entry);
                }
                Entry::Vacant(entry) => {
                    entry.insert(find(repr, value));
                }
            }
        }
        for (key, value) in insertions.drain(..) {
            let key = (find(repr, key.0), find(repr, key.1));

            match map.entry(key) {
                Entry::Occupied(mut entry) => {
                    let entry = entry.get_mut();
                    *entry = union(repr, value, *entry);
                }
                Entry::Vacant(entry) => {
                    entry.insert(find(repr, value));
                }
            }
        }
    }

    fn update_raw(
        map: &mut HashMap<(u32, u32), u32>,
        repr: &mut [u32],
        insertions: &mut Vec<((u32, u32), u32)>,
    ) {
        rehash_grow(map, repr, insertions);
        let raw_table = map.raw_table_mut();

        // 1. You must not free the hash table while iterating (including via growing/shrinking).
        // 2. It is fine to erase a bucket that has been yielded by the iterator.
        // 3. Erasing a bucket that has not yet been yielded by the iterator may still result in
        //    the iterator yielding that bucket (unless reflect_remove is called).
        // 4. It is unspecified whether an element inserted after the iterator was created will be
        //    yielded by that iterator (unless reflect_insert is called).
        // 5. The order in which the iterator yields bucket is unspecified and may change in the
        //    future.

        // TODO: safety: avoid rehash here

        // map.retain(|&(x0, x1), &mut x2| {
        //     if (repr[x0 as usize] == x0) & (repr[x1 as usize] == x1) & (repr[x2 as usize] == x2) {
        //         true
        //     } else {
        //         insertions.push(((repr[x0 as usize], repr[x1 as usize]), repr[x2 as usize]));
        //         false
        //     }
        // });

        for bucket in unsafe { raw_table.iter() } {
            let (key, value) = *unsafe { bucket.as_ref() };

            if !(repr[key.0 as usize] == key.0 && repr[key.1 as usize] == key.1) {
                unsafe { raw_table.erase(bucket) };
            } else {
            }
        }
    }
}

/// A non-fd index, behaves like a `HashMap<Key, Vec<Value>>`
///
/// Only the *query* parts of the index, reconstruct is implementation dependent.
pub trait NonFdIndex {
    type Key;
    type Value;

    fn iter_key_value(&self) -> impl Iterator<Item = (Self::Key, Self::Value)>;
    fn iter(&self, key: Self::Key) -> impl Iterator<Item = Self::Value>;
    fn contains_key(&self, key: &Self::Key) -> bool;
    fn len(&self) -> usize;
}

// 2^16 counts.

#[allow(unused)]
struct SimpleFdIndex<Key, Value> {
    map: HashMap<Key, SmallVec<[Value; 1]>>,
}
impl<Key, Value> SimpleFdIndex<Key, Value>
where
    Key: Copy + Ord + Hash,
    Value: Copy + Ord,
{
    #[allow(unused)]
    fn reconstruct<Row: Copy>(
        &mut self,
        all_rows: &mut [Row],
        extract_key: impl Fn(Row) -> Key,
        extract_value: impl Fn(Row) -> Value,
    ) {
        let mut buckets: [Vec<Row>; 256] = std::array::from_fn(|_| Vec::new());
        for &row in &*all_rows {
            use std::hash::{BuildHasher, Hash};
            let key = extract_key(row);

            let h = self.map.hasher();
            let hash = h.hash_one(key);
            let bucket = (hash & 0xFF) as u8;

            buckets[bucket as usize].push(row);
        }

        for bucket in &buckets {
            for &row in bucket {
                self.map
                    .entry(extract_key(row))
                    .or_insert_with(|| SmallVec::new())
                    .push(extract_value(row));
            }
        }
    }
}

fn radix_sort<
    Row,
    Key,
    Value,
    Continuation,
    Output,
    ExtractRadix,
    ExtractKey,
    MergeKey,
    const COMPUTE_MASK: bool,
>(
    data: &mut [Row],
    scratch: &mut [Row],
    output_list: &mut [Value],
    output_obj: &mut Output,
    mut mask: Key,
    extract_radix: ExtractRadix,
    extract_key: ExtractKey,
    merge_key: MergeKey,
    continuation: Continuation,
) -> Key
where
    Continuation: Fn(&mut [Row], &mut [Row], Key, &mut [Value], &mut Output) -> Key,
    Row: Copy,
    Key: Copy,
    Value: Copy,
    ExtractRadix: Fn(Key) -> u8 + Copy,
    ExtractKey: Fn(Row) -> Key + Copy,
    MergeKey: Fn(Key, Key) -> Key + Copy,
{
    // IF all paths agree on `mask`, then we won't have parity issues.
    if !COMPUTE_MASK && extract_radix(mask) == 0 {
        return continuation(data, scratch, mask, output_list, output_obj);
    }

    let mut counts: [usize; 256] = [0; 256];
    for &x in &*data {
        counts[extract_radix(extract_key(x)) as usize] += 1;
        if COMPUTE_MASK {
            mask = merge_key(mask, extract_key(x));
        }
    }

    if COMPUTE_MASK && extract_radix(mask) == 0 {
        return continuation(data, scratch, mask, output_list, output_obj);
    }

    let mut curr = 0;
    for i in 0..256 {
        let tmp = counts[i];
        counts[i] = curr;
        curr += tmp;
    }
    for &x in &*data {
        let c = &mut counts[extract_radix(extract_key(x)) as usize];
        scratch[*c] = x;
        *c += 1;
    }

    let mut start = 0;
    for i in 0..256 {
        let end = counts[i];
        if start != end {
            continuation(
                &mut scratch[start..end],
                &mut data[start..end],
                mask,
                output_list,
                output_obj,
            );
        }
        start = end;
    }
    mask
}

fn radix_sort_usage(
    data: &mut [(u32, u32)],
    scratch: &mut [(u32, u32)],
    map: &mut HashMap<u32, u32>,
    list: &mut [u32],
) {
    let extract_key = |(a, b): (u32, u32)| a;
    let extract_value = |(a, b): (u32, u32)| b;

    let radix0 = |x: u32| (x & 0xFF) as u8;
    let radix1 = |x: u32| ((x >> 8) & 0xFF) as u8;
    let radix2 = |x: u32| ((x >> 16) & 0xFF) as u8;
    let radix3 = |x: u32| ((x >> 24) & 0xFF) as u8;

    let merge_key = |a: u32, b: u32| a | b;
    let mask = radix_sort::<(u32, u32), u32, u32, _, _, _, _, _, true>(
        data,
        scratch,
        list,
        map,
        u32::default(),
        radix0,
        extract_key,
        merge_key,
        |data, scratch, mask, list, map| {
            radix_sort::<(u32, u32), u32, u32, _, _, _, _, _, false>(
                data,
                scratch,
                list,
                map,
                mask,
                radix1,
                extract_key,
                merge_key,
                |data, scratch, mask, list, map| {
                    radix_sort::<(u32, u32), u32, u32, _, _, _, _, _, false>(
                        data,
                        scratch,
                        list,
                        map,
                        mask,
                        radix2,
                        extract_key,
                        merge_key,
                        |data, scratch, mask, list, map| {
                            radix_sort::<(u32, u32), u32, u32, _, _, _, _, _, false>(
                                data,
                                scratch,
                                list,
                                map,
                                mask,
                                radix3,
                                extract_key,
                                merge_key,
                                |data, _, _, list, map| {
                                    let key = data[0].1;
                                    todo!()
                                },
                            )
                        },
                    )
                },
            )
        },
    );
}



















