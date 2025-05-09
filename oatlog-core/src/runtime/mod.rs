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
        generic::{Eclass, EclassProvider, RelationElement, ReprU32},
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
pub use hashbrown::{HashMap, hash_map::Entry as HashMapEntry};
pub use smallvec::SmallVec;
pub use std::{
    convert::Infallible,
    hash::Hash,
    mem::{swap, take},
};
pub use voracious_radix_sort::{RadixSort, Radixable};

use std::hash::BuildHasher;

pub trait Clear: Sized {
    fn clear(&mut self);
    /// set self to other and clear other without allocations
    fn take_scratch(&mut self, other: &mut Self) {
        self.clear();
        swap(self, other);
    }
}
impl<T> Clear for Vec<T> {
    #[inline]
    fn clear(&mut self) {
        Vec::clear(self);
    }
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
    /*
    pub unsafe fn iter_unchecked(&self, key: Key) -> impl Iterator<Item = Value> {
        self.map
            .get(&key)
            .copied()
            .into_iter()
            .flat_map(|(start, end)| {
                unsafe { self.list.get_unchecked(start as usize..end as usize) }
                    .iter()
                    .copied()
            })
    }
    */
    pub fn len(&self) -> usize {
        self.list.len()
    }
}

pub trait Relation {
    type Row;
    type Unification;

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

// TODO erik: delete, this is dead code.
pub trait RangeQuery<T, V> {
    fn query(&self, t: T) -> impl Iterator<Item = V>; // + use<'a, T, V, Self>;
    fn check(&self, t: T) -> bool {
        self.query(t).next().is_some()
    }
}

// TODO erik: delete, this is dead code.
mod range_query_impl {
    use super::RangeQuery;
    use super::RelationElement;
    use std::collections::BTreeSet;
    macro_rules! oh_no {
        ( , $($name0:ident $digit0:tt)*) => {};
        ( $($name0:ident $digit0:tt)*, ) => {};
        ($($name0:ident $digit0:tt)* , $($name1:ident $digit1:tt)*) => {
            #[allow(unused_parens)]
            impl<$($name0,)* $($name1),*> RangeQuery<($($name0,)*), ($($name1),*)> for BTreeSet<($($name0,)* $($name1),*)>
            where
                $($name0 : RelationElement,)*
                $($name1 : RelationElement,)*
            {
                #[inline]
                fn query(&self, t: ($($name0,)*)) -> impl Iterator<Item = ($($name1),*)> {
                    self.range(($(t . $digit0,)* $($name1::MIN_ID,)*)..($(t . $digit0,)* $($name1::MAX_ID,)*))
                        .copied()
                        .map(|x| ($(x . $digit1),*))
                }
            }

        };
    }
    macro_rules! what {
        ($($name0:ident $digit0:tt)* , ) => {
            oh_no!($($name0 $digit0)*,);
        };
        ($($name0:ident $digit0:tt)* , $name1:ident $digit1:tt $($name2:ident $digit2:tt)*) => {
            oh_no!($($name0 $digit0)*, $name1 $digit1 $($name2 $digit2)*);
            what!($($name0 $digit0)* $name1 $digit1, $($name2 $digit2)*);
        }
    }
    macro_rules! why {
        ($($name0:ident $digit0:tt)* , ) => {
            what!(,$($name0 $digit0)*);
        };
        ($($name0:ident $digit0:tt)* , $name1:ident $digit1:tt $($name2:ident $digit2:tt)*) => {
            what!(,$($name0 $digit0)*);
            why!($($name0 $digit0)* $name1 $digit1, $($name2 $digit2)*);
        };
    }
    why!(, A 0 B 1 C 2 D 3 E 4 F 5 G 6 H 7 I 8 J 9 K 10 L 11);
}
