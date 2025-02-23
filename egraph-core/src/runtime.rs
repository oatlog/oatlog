//! "runtime" functions and types to be used by generated code.

#![allow(unused_parens)]

use std::{marker::PhantomData, mem::swap};

pub trait Clear: Sized {
    fn clear(&mut self);
    /// set self to other and clear other without allocations
    fn take_scratch(&mut self, other: &mut Self) {
        self.clear();
        swap(self, other);
    }
}
impl<T> Clear for Vec<T> {
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

/// Some type that can be put in a relation.
pub trait RelationElement: Copy + Clone + Eq + PartialEq + Ord + PartialOrd {
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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Math(u32);
impl RelationElement for Math {
    const MIN_ID: Self = Math(0);
    const MAX_ID: Self = Math(u32::MAX);
}

// impl RangeQuery<(Math, Math), (Math)> for BTreeSet<(Math, Math, Math)> {
//     fn query(&self, t: (Math, Math)) -> impl Iterator<Item = (Math)> {
//         self.range((t.0, t.1, Math::MIN_ID)..(t.0, t.1, Math::MAX_ID)).copied().map(|x| (x.2))
//     }
// }

// impl<A, B, C> RangeQuery<(A, B), (C)> for BTreeSet<(A, B, C)>
// where
//     A: RelationElement,
//     B: RelationElement,
//     C: RelationElement,
// {
//     fn query(&self, t: (A, B)) -> impl Iterator<Item = (C)> {
//         self.range((t.0, t.1, C::MIN_ID)..(t.0, t.1, C::MAX_ID))
//             .copied()
//             .map(|x| (x.2))
//     }
// }

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
                fn query(&self, t: ($($name0,)*)) -> impl Iterator<Item = ($($name1),*)> {
                    self.range(($(t . $digit0,)* $($name1::MIN_ID,)*)..($(t . $digit0,)* $($name1::MIN_ID,)*))
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

/// Wrapper type for a u32 to represent a typed e-class.
/// Emit this instead to make codegen a bit cleaner.
#[macro_export]
macro_rules! eclass_wrapper_ty {
    ($name:ident) => {
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
        pub struct $name(u32);
        impl Eclass for $name {
            fn new(value: u32) -> Self {
                Self(value)
            }
            fn inner(self) -> u32 {
                self.0
            }
        }
        impl RelationElement for $name {
            const MIN_ID = Self(0);
            const MAX_ID = Self(u32::MAX);
        }
    };
}

/// The main union-find.
/// Per eclass state.
#[derive(Debug, Default)]
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
impl<T: Eclass> UnionFind<T> {
    pub fn new() -> Self {
        Self {
            repr: Vec::new(),
            dirty: Vec::new(),
            size: Vec::new(),
            _marker: PhantomData,
        }
    }
    pub fn find(&mut self, t: T) -> T {
        T::new(self.find_inner(t.inner()))
    }
    pub fn find_inner(&mut self, i: u32) -> u32 {
        if self.repr[i as usize] == i {
            i
        } else {
            let root = self.find_inner(self.repr[i as usize]);
            self.repr[i as usize] = root;
            root
        }
    }
    // returns uprooted
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
        println!("uf-union: {uprooted:?} -> {root:?}");
        self.dirty.push(T::new(uprooted));
    }
    pub fn dirty(&mut self) -> &mut Vec<T> {
        &mut self.dirty
    }
    /// INVARIANT: also add to delta
    pub fn add_eclass(&mut self) -> T {
        let id = u32::try_from(self.repr.len()).expect("out of u32 ids");
        self.repr.push(id);
        self.size.push(1);
        T::new(id)
    }
    // inc/dec this possibly non-canonicalized e-class
    // can be no-op if not repr.
    pub fn inc_eclass(&mut self, t: T, delta: u32) {
        self.size[t.inner() as usize] += delta;
    }
    pub fn dec_eclass(&mut self, t: T, delta: u32) {
        self.size[t.inner() as usize] -= delta;
    }
}
