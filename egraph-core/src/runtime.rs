//! "runtime" functions and types to be used by generated code.
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

/// Handle to some type (eg sets)
pub trait RelationElement: Copy + Clone + Eq + PartialEq + Ord + PartialOrd {
    /// Minimum id value according to Ord
    /// Used to implement range queries.
    const MIN_ID: Self;
    /// Maximum id value according to Ord
    /// Used to implement range queries.
    const MAX_ID: Self;
}

pub trait Relation { type Row; }

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
        let (root, uprooted) = if self.size[a as usize] > self.size[b as usize] {
            (a, b)
        } else {
            (b, a)
        };
        self.repr[uprooted as usize] = self.repr[root as usize];
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
