//! "runtime" functions and types to be used by generated code.
#![allow(unsafe_code)]

mod generic;
mod global_vars;
mod index;
mod row;
mod uf;

pub use crate::{
    decl_row, eclass_wrapper_ty, log_duration, relation_element_wrapper_ty,
    runtime::{
        generic::{EclassProvider, EclassRepr, RelationElement, ReprU32},
        global_vars::GlobalVars,
        index::{
            EclassCtx, GeneralCtx, Index, IndexStatic, RowCtx, SortedVec, SortedVec as IndexImpl,
            dedup_suffix,
        },
        row::{
            IndexRow, RadixSortable, SimdRow,
            mk_rowsort::{
                RowSort001, RowSort01, RowSort1, RowSort010, RowSort10, RowSort011, RowSort11,
                RowSort100, RowSort101, RowSort110, RowSort111,
            },
        },
        uf::UnionFind,
    },
};
pub use hashbrown_14::{HashMap, hash_map::Entry as HashMapEntry};
pub use smallvec::SmallVec;
pub use std::{
    convert::Infallible,
    hash::Hash,
    mem::{swap, take},
};
use std::{
    fmt::Debug,
    hash::{BuildHasherDefault, Hasher},
    mem::replace,
};
pub use voracious_radix_sort::{RadixSort, Radixable};

pub use std::cmp::Ordering;

pub use std::iter::once;

#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub struct OrdF64(pub f64);
impl Eq for OrdF64 {}
impl PartialOrd for OrdF64 {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}
impl Ord for OrdF64 {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.total_cmp(&other.0)
    }
}
impl Hash for OrdF64 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state);
    }
}
impl std::fmt::Display for OrdF64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <f64 as std::fmt::Display>::fmt(&self.0, f)
    }
}

impl RelationElement for OrdF64 {
    const MIN_ID: Self = OrdF64(f64::MIN);

    const MAX_ID: Self = OrdF64(f64::MAX);
}

#[inline(always)]
pub fn prefetch_ptr<T>(ptr: *const T) {
    use core::arch::x86_64::{_MM_HINT_T0, _mm_prefetch};

    #[cfg(not(target_arch = "x86_64"))]
    compile_error!("prefetch code assumes a x86 system.");

    // SAFETY: we are on an x86_64 system.
    // Prefetching out of bounds is safe.
    unsafe { _mm_prefetch(ptr as *const i8, _MM_HINT_T0) };
}

#[inline(always)]
pub fn retain_hashmap<Key: Hash + Copy, Val: Copy, Row: Copy>(
    map: &mut hashbrown_14::HashMap<Key, Val>,
    out: &mut Vec<Row>,
    pack_key_val: impl Fn(Key, Val) -> Row,
    mut row_is_root: impl FnMut(Row) -> bool,
) {
    map.retain(|&key, &mut val| {
        let row = pack_key_val(key, val);
        if row_is_root(row) {
            true
        } else {
            out.push(row);
            false
        }
    });
}

/*
fn foo(map, insertions) {


    if insertions > map.leftover_capacity {

        // we get "free" margin because round to power of 2 AND most insertions probably
        // don' t do anything.
        let mut old_map = HashMap::with_capacity(map + insertions);
        swap(&mut old_map, map);

        // this is *mostly* using memory linearly
        for x in old_map {
            map.insert(x);
        }
    }

    for x in insertions {
        map.insert(x);
    }
}
*/

pub fn reinsert_hashmap<Key: Hash + Copy + Eq, Val: Copy, Row: Copy + Default>(
    map: &mut hashbrown_14::HashMap<Key, Val>,
    insertions: &[Row],
    mut map_insert: impl FnMut(&mut hashbrown_14::HashMap<Key, Val>, Row, TimeStamp),
    pack_key_val: impl Fn(Key, Val) -> (Row, TimeStamp),
    extract_key: impl Fn(Row) -> Key,
    latest_timestamp: TimeStamp,
) {
    /*
    let needed_capacity = map.len() + insertions.len();
    let current_capacity = map.capacity();

    // eagerly rehash
    if needed_capacity > current_capacity {
        let hasher: BuildHasherDefault<_> = map.hasher().clone();

        let old_map = replace(
            map,
            HashMap::with_capacity_and_hasher(needed_capacity, hasher),
        );

        for (&key, &value) in old_map.iter() {
            let (row, timestamp) = pack_key_val(key, value);
            map_insert(map, row, timestamp);
        }
    }
    */

    /*
    let should_sort = insertions.len() >= map.len() && map.len() > 8192;

    if should_sort {
        // let hasher = map.hasher();
        // let mut bin_count = [0; 256];

        // for row in insertions {
        //     use std::hash::BuildHasher;
        //     let key = extract_key(*row);
        //     let hash: u64 = hasher.hash_one(key);
        //     let bin = ((hash >> 6) & 0xFF) as usize;
        //     bin_count[bin] += 1;
        // }

        // // dbg!(bin_count);

        // let mut cc = 0;
        // for i in 0..256 {
        //     let tmp = bin_count[i];
        //     bin_count[i] = cc;
        //     cc += tmp;
        // }

        // let mut out: Vec<std::mem::MaybeUninit<Row>> = Vec::with_capacity(insertions.len());
        // unsafe { out.set_len(insertions.len()) };

        // for row in insertions {
        //     use std::hash::BuildHasher;
        //     let key = extract_key(*row);
        //     let hash: u64 = hasher.hash_one(key);
        //     let bin = ((hash >> 6) & 0xFF) as usize;

        //     let index = bin_count[bin];
        //     bin_count[bin] += 1;
        //     out[index].write(*row);
        // }

        // for row in out {
        //     let row = unsafe { row.assume_init() };
        //     map_insert(map, row, latest_timestamp);
        // }

        // for batch in bins {
        //     for row in batch {
        //         map_insert(map, row, latest_timestamp);
        //     }
        // }
        // for &row in insertions.iter() {
        //     map_insert(map, row, latest_timestamp);
        // }
    }
    */

    for &row in insertions.iter() {
        map_insert(map, row, latest_timestamp);
    }
}

// insert rhs into lhs
/*
fn foo(lhs, rhs, lhs_time, rhs_time, latest) {

    // rhs changed => latest_timestamp
    // merge => rhs changed


}
*/

#[inline(always)]
pub fn prefetch_map<K: Hash, V>(map: &hashbrown_14::HashMap<K, V>, key: K) {
    use std::hash::BuildHasher;

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
    prefetch_ptr(unsafe { raw_table.bucket(idx) }.as_ptr());

    let metadata_start = raw_table.data_end().as_ptr() as *const i8;

    // prefetch metadata
    prefetch_ptr(metadata_start.wrapping_add(idx));
}

/// semantically a HashMap<Key, Vec<Value>>
#[derive(Debug, Default)]
pub struct IndexedSortedList<Key, Value> {
    // (start, end), exclusive range
    // TODO erik: we would want to replace this with a hashmap that is "insert only", which in
    // practice means that we don't check for tombstones anymore.
    map: HashMap<Key, (u32, u32)>,
    list: Vec<Value>,
}
impl<Key: Copy + Ord + Hash, Value: Copy + Ord> IndexedSortedList<Key, Value> {
    /// SAFETY:
    /// * all_rows is sorted based on some total order on extract_key(row)
    /// * meaning we don't care about the blocks of rows that are equal based on extract_key.
    pub unsafe fn reconstruct<Row: Copy>(
        &mut self,
        all_rows: &mut [Row],
        extract_key: impl Fn(Row) -> Key,
        extract_value: impl Fn(Row) -> Value,
        // dedup_done: bool
    ) {
        let n = all_rows.len();
        log_duration!("reconstruct alloc: {}", {
            // about 1 ms
            self.map.clear();
            self.map.reserve(n);
            self.list.clear();
            self.list.reserve(n);
        });

        // TODO erik: can we radix sort by hash?
        // let h = self.map.hasher();
        // let m = ((n * 8) / 7).next_power_of_two() as u64 - 1;

        // log_duration!("reconstruct sort: {}", {
        //     all_rows.sort_unstable_by_key(|row| {
        //         let key = extract_key(*row);
        //         let value = extract_value(*row);
        //         (key, value)
        //     });
        // });

        log_duration!("reconstruct copy list: {}", {
            // about 5 ms
            self.list
                .extend(all_rows.iter().copied().map(extract_value));
        });

        log_duration!("reconstruct insert: {}", {
            // about 10 ms
            let mut i = 0;
            while i < n {
                let key_i = extract_key(all_rows[i]);
                let mut j = i + 1;
                while j < n && extract_key(all_rows[j]) == key_i {
                    j += 1;
                }
                debug_assert!(!self.map.contains_key(&key_i));
                // self.map.insert(key_i, (i as u32, j as u32));
                unsafe {
                    self.map
                        .insert_unique_unchecked(key_i, (i as u32, j as u32));
                }
                i = j;
            }
        });
    }
    pub fn iter_key_value(&self) -> impl Iterator<Item = (Key, Value)> {
        self.map.iter().flat_map(|(key, &(start, end))| {
            debug_assert!(start <= end);
            debug_assert!((start as usize) < self.list.len());
            debug_assert!((end as usize) <= self.list.len());
            // self.list[start as usize..end as usize]
            unsafe { self.list.get_unchecked(start as usize..end as usize) }
                .iter()
                .copied()
                .map(|value| (*key, value))
        })
    }
    pub fn iter(&self, key: Key) -> impl Iterator<Item = Value> {
        self.map
            .get(&key)
            .copied()
            .into_iter()
            .flat_map(|(start, end)| {
                debug_assert!(start <= end);
                debug_assert!((start as usize) < self.list.len());
                debug_assert!((end as usize) <= self.list.len());
                // self.list[start as usize..end as usize].iter().copied()
                unsafe { self.list.get_unchecked(start as usize..end as usize) }
                    .iter()
                    .copied()
            })
    }
    pub fn contains_key(&self, key: &Key) -> bool {
        self.map.contains_key(key)
    }
    pub fn len(&self) -> usize {
        self.list.len()
    }
}

pub trait Relation {
    type Row;
    type Unification;
    type Enode;
    type Eclass;

    const COST: u32;

    fn new() -> Self;
    fn has_new(&self) -> bool;
    fn clear_new(&mut self);
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row>;
    fn len(&self) -> usize;
    fn emit_graphviz(&self, buf: &mut String);
    fn update_begin(
        &mut self,
        insertions: &[Self::Row],
        uf: &mut Self::Unification,
        latest_timestamp: TimeStamp,
    );
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        uf: &mut Self::Unification,
        latest_timestamp: TimeStamp,
    ) -> bool;
    fn update_finalize(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        uf: &mut Self::Unification,
        latest_timestamp: TimeStamp,
    );

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
    fn deferred_update(&mut self) {}

    fn serialize(&self, out: &mut Vec<(Self::Enode, Self::Eclass)>) {}
}

relation_element_wrapper_ty!(IString);
relation_element_wrapper_ty!(TimeStamp);

/// Only to be used for initial inserts, so performance does not really matter.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct StringIntern {
    to_id: std::collections::BTreeMap<String, IString>,
    to_string: std::collections::BTreeMap<IString, String>,
}
impl StringIntern {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
    #[inline]
    pub fn intern(&mut self, s: String) -> IString {
        let next_id = IString(u32::try_from(self.to_id.len()).unwrap());
        *self.to_id.entry(s.clone()).or_insert_with(|| {
            self.to_string.insert(next_id, s);
            next_id
        })
    }
    #[must_use]
    pub fn lookup(&self, i: IString) -> &str {
        &self.to_string[&i]
    }
}

pub trait EnodeInputs<Eclass> {
    fn inputs(self) -> Vec<Eclass>;
}

pub trait EclassMapExtract<Enode, ExtractExpr>: Sized {
    fn map_extract(self, extract: impl Fn(Self) -> Option<Enode> + Copy) -> Option<ExtractExpr>;
}

/// Perform DAG extraction
pub fn extract<
    Enode: EnodeInputs<Eclass> + Copy + Clone + Hash + Eq + Debug + Ord,
    Eclass: EclassMapExtract<Enode, ExtractExpr> + Copy + Clone + Hash + Eq + Debug,
    ExtractExpr,
>(
    enode_to_eclass: impl Iterator<Item = (Enode, Eclass)>,
    root: Eclass,
) -> Option<ExtractExpr> {
    use std::collections::HashMap;
    type Map<K, V> = HashMap<K, V>;

    let enode_to_eclass: Map<Enode, Eclass> = enode_to_eclass.collect();

    // how many unprocessed enodes?
    let mut eclass_in_deg: Map<Eclass, usize> = Map::new();

    // how many unprocessed eclasses?
    let mut enode_in_deg: Map<Enode, usize> = Map::new();

    let mut eclass_to_enode: Map<Eclass, Vec<Enode>> = Map::new();

    let mut eclass_readers: Map<Eclass, Vec<Enode>> = Map::new();
    for (&enode, &eclass) in enode_to_eclass.iter() {
        eclass_to_enode.entry(eclass).or_default().push(enode);
        *eclass_in_deg.entry(eclass).or_default() += 1;

        let inputs = enode.inputs();
        for eclass in inputs.iter().copied() {
            eclass_readers.entry(eclass).or_default().push(enode);
        }

        *enode_in_deg.entry(enode).or_default() = inputs.len();
    }

    let mut eclass_queue: Vec<Eclass> = Vec::new();
    let mut enode_queue: Vec<Enode> = Vec::new();

    for (&enode, &eclass) in enode_to_eclass.iter() {
        if enode_in_deg[&enode] == 0 {
            enode_queue.push(enode);
        }
    }

    let mut enode_cost: Map<Enode, i64> = Map::new();
    let mut eclass_cost: Map<Eclass, (i64, Enode)> = Map::new();

    while !(eclass_queue.is_empty() && enode_queue.is_empty()) {
        while let Some(eclass) = eclass_queue.pop() {
            let (cost, enode) = eclass_to_enode[&eclass]
                .iter()
                .copied()
                .map(|enode| (enode_cost[&enode], enode))
                .min()
                .expect("attempted to extract empty eclass?");

            eclass_cost.insert(eclass, (cost, enode));
            if let Some(eclass_readers) = eclass_readers.get(&eclass) {
                for reader_enode in eclass_readers.iter().copied() {
                    let count = enode_in_deg.entry(reader_enode).or_default();
                    *count -= 1;

                    if *count == 0 {
                        enode_queue.push(reader_enode);
                    }
                }
            }
        }
        while let Some(enode) = enode_queue.pop() {
            // cost of the enode itself.
            let mut cost = 1;

            for input in enode.inputs() {
                let (eclass_cost, _) = eclass_cost[&input];
                cost += eclass_cost;
            }
            enode_cost.insert(enode, cost);

            let eclass = enode_to_eclass[&enode];
            let count = eclass_in_deg.entry(eclass).or_default();
            *count -= 1;
            if *count == 0 {
                eclass_queue.push(eclass);
            }
        }
    }

    root.map_extract(|eclass| eclass_cost.get(&eclass).map(|x| x.1))
}
