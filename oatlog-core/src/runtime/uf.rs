use crate::runtime::Eclass;
use std::marker::PhantomData;

/// The main union-find.
/// Per eclass state.
#[derive(Default)]
pub struct UnionFind<T> {
    // TODO: maybe merge repr and size.
    repr: Vec<u32>,
    /// reprs that should be uprooted.
    dirty: Vec<T>,
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
            if repr == i as u32 {
                writeln!(f, "{i}")?;
            } else {
                writeln!(f, "{i}: {repr}")?;
            }
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
    pub fn are_equal(&mut self, a: T, b: T) -> bool {
        self.find(a) == self.find(b)
    }
    #[inline]
    pub fn union_mut(&mut self, a: &mut T, b: &mut T) {
        let ret = self.union(*a, *b);
        *a = ret;
        *b = ret;
    }
    #[inline]
    pub fn union(&mut self, a: T, b: T) -> T {
        let a = self.find_inner(a.inner());
        let b = self.find_inner(b.inner());
        if a == b {
            return T::new(a);
        }
        let (root, uprooted) = (u32::min(a, b), u32::max(a, b));
        self.repr[uprooted as usize] = self.repr[root as usize];
        self.dirty.push(T::new(uprooted));
        T::new(root)
    }
    #[inline]
    pub fn dirty(&mut self) -> &mut Vec<T> {
        &mut self.dirty
    }
    #[inline]
    pub fn add_eclass(&mut self) -> T {
        let id = u32::try_from(self.repr.len()).expect("out of u32 ids");
        self.repr.push(id);
        T::new(id)
    }
}
