//! "runtime" functions and types to be used by generated code.

#![allow(unused_parens)]

pub use std::hash::Hash;
pub use std::{collections::BTreeSet, mem::swap, mem::take};

use std::{collections::BTreeMap, marker::PhantomData};

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

/// Must be produced from a [`UnionFind`]
/// Trait to support wrapper types in [`UnionFind`]
/// Essentially an object that "the engine" controls.
pub trait Eclass: RelationElement {
    fn new(value: u32) -> Self;
    fn inner(self) -> u32;
}

impl RelationElement for u32 {
    const MAX_ID: Self = 0;
    const MIN_ID: Self = 0;
}
impl Eclass for u32 {
    fn new(x: u32) -> u32 {
        x
    }
    fn inner(self) -> u32 {
        self
    }
}

/// Some type that can be put in a relation.
pub trait RelationElement:
    Copy + Clone + Eq + PartialEq + Ord + PartialOrd + Default + Hash
{
    /// Minimum id value according to Ord
    /// Used to implement range queries.
    const MIN_ID: Self;
    /// Maximum id value according to Ord
    /// Used to implement range queries.
    const MAX_ID: Self;
}

// f64 not strictly required
impl RelationElement for i64 {
    const MIN_ID: Self = i64::MIN;
    const MAX_ID: Self = i64::MAX;
}

impl RelationElement for bool {
    const MIN_ID: Self = false;
    const MAX_ID: Self = true;
}

#[macro_export]
macro_rules! relation_element_wrapper_ty {
    ($($name:ident),*) => {
        $(
            #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Hash, Debug)]
            pub struct $name(pub u32);

            impl RelationElement for $name {
                const MIN_ID: Self = Self(0);
                const MAX_ID: Self = Self(u32::MAX);
            }

            impl std::fmt::Display for $name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                    write!(f, "{self:?}")
                }
            }
        )*
    };
}

/// Wrapper type for a u32 to represent a typed e-class.
/// Emit this instead to make codegen a bit cleaner.
#[macro_export]
macro_rules! eclass_wrapper_ty {
    ($($name:ident),*) => {
        relation_element_wrapper_ty!($($name),*);
        $(
            impl Eclass for $name {
                fn new(value: u32) -> Self {
                    Self(value)
                }
                fn inner(self) -> u32 {
                    self.0
                }
            }
        )*
    };
}

pub use eclass_wrapper_ty;
pub use relation_element_wrapper_ty;

relation_element_wrapper_ty!(IString);

/// Only to be used for initial inserts, so performance does not really matter.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Default)]
pub struct StringIntern {
    to_id: BTreeMap<String, IString>,
    to_string: BTreeMap<IString, String>,
}
impl StringIntern {
    #[inline]
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

pub trait Relation {
    type Row;
}

/// To avoid many functions with a type suffix: `make_math`, `make_data`, ...
pub trait EclassProvider<T: Eclass> {
    fn make(&mut self) -> T;
    fn find(&mut self, t: T) -> T;
    fn union(&mut self, a: T, b: T);
}

/// The main union-find.
/// Per eclass state.
#[derive(Default)]
pub struct UnionFind<T> {
    // TODO: maybe merge repr and size.
    repr: Vec<u32>,
    /// reprs that should be uprooted.
    dirty: Vec<T>,
    /// if canonicalized, about how many memory locations will need to be modified?
    size: Vec<u32>,
    _marker: PhantomData<T>,
    // forall semi-naive:
    // old: BTreeSet<T>,
    // new: Vec<T>,
    // delta: Vec<T>,
}
impl<T> std::fmt::Debug for UnionFind<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        for i in 0..self.repr.len() {
            let repr = self.repr[i];
            let size = self.size[i];
            writeln!(f, "{i}: {repr}({size})")?;
        }
        Ok(())
    }
}
impl<T: Eclass> UnionFind<T> {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            repr: Vec::new(),
            dirty: Vec::new(),
            size: Vec::new(),
            _marker: PhantomData,
        }
    }
    #[inline]
    pub fn find(&mut self, t: T) -> T {
        T::new(self.find_inner(t.inner()))
    }
    #[inline]
    pub fn find_inner(&mut self, i: u32) -> u32 {
        if self.repr[i as usize] == i {
            i
        } else {
            let root = self.find_inner(self.repr[i as usize]);
            self.repr[i as usize] = root;
            root
        }
    }
    #[inline]
    pub fn union(&mut self, a: T, b: T) {
        let a = self.find_inner(a.inner());
        let b = self.find_inner(b.inner());
        if a == b {
            return;
        }
        let (root, uprooted) = if self.size[a as usize] > self.size[b as usize] {
            (a, b)
        } else {
            (b, a)
        };
        self.repr[uprooted as usize] = self.repr[root as usize];
        self.dirty.push(T::new(uprooted));
    }
    #[inline]
    pub fn dirty(&mut self) -> &mut Vec<T> {
        &mut self.dirty
    }
    #[inline]
    pub fn add_eclass(&mut self) -> T {
        let id = u32::try_from(self.repr.len()).expect("out of u32 ids");
        self.repr.push(id);
        self.size.push(1);
        T::new(id)
    }
    #[inline]
    /// Increment cost of uprooting this e-class. No-op if already uprooted.
    pub fn inc_eclass(&mut self, t: T, delta: u32) {
        self.size[t.inner() as usize] += delta;
    }
    #[inline]
    /// Decrement cost of uprooting this e-class. No-op if already uprooted.
    pub fn dec_eclass(&mut self, t: T, delta: u32) {
        self.size[t.inner() as usize] -= delta;
    }
}
