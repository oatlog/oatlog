//! "runtime" functions and types to be used by generated code.

mod generic;
mod global_vars;
mod index;
mod row;
mod uf;

pub use crate::{
    decl_row, eclass_wrapper_ty, log_duration, relation_element_wrapper_ty,
    runtime::{
        generic::{Eclass, EclassProvider, RelationElement},
        global_vars::GlobalVars,
        index::{
            EclassCtx, GeneralCtx, Index, IndexStatic, RowCtx, SortedVec, SortedVec as IndexImpl,
            dedup_suffix,
        },
        row::{IndexRow, RadixSortable, SimdRow},
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
    fn update_begin(&mut self, insertions: &[Self::Row], uf: &mut Self::Unification);
    fn update(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Self::Unification) -> bool;
    fn update_finalize(&mut self, insertions: &mut Vec<Self::Row>, uf: &mut Self::Unification);

    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

relation_element_wrapper_ty!(IString);

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

pub trait RangeQuery<T, V> {
    fn query(&self, t: T) -> impl Iterator<Item = V>; // + use<'a, T, V, Self>;
    fn check(&self, t: T) -> bool {
        self.query(t).next().is_some()
    }
}
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
