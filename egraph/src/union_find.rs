use std::cell::Cell;
use std::ops::{Index, IndexMut};

use crate::ids::Id;

enum Uninhabited {}

/// alias for a valid merge function.
pub(crate) trait Merge<D, E>: FnMut(D, D) -> Result<D, E> {}
impl<D, E, T: FnMut(D, D) -> Result<D, E>> Merge<D, E> for T {}

/// Type alias for union-find without data.
type UF<T> = UFData<T, ()>;

/// Union Find with optional attached data.
///
/// Indexing canonicalizes the index and returns the associated data for that index.
#[derive(Clone, Debug)]
pub(crate) struct UFData<T: Id, D> {
    repr: Vec<Cell<T>>,
    data: Vec<D>,
}
impl<T: Id, D: Default + Clone> UFData<T, D> {
    pub(crate) fn new_with_size(n: usize) -> Self {
        Self {
            repr: (0..n).map(T::from).map(Cell::new).collect(),
            data: vec![D::default(); n],
        }
    }
}
impl<T: Id, D: PartialEq> Eq for UFData<T, D> {}
impl<T: Id, D: PartialEq> PartialEq for UFData<T, D> {
    fn eq(&self, other: &Self) -> bool {
        self.canonicalize();
        other.canonicalize();

        let Self {
            repr: srepr,
            data: sdata,
        } = self;
        let Self {
            repr: orepr,
            data: odata,
        } = other;

        srepr == orepr && odata == sdata
    }
}

impl<T: Id, D: Clone> UFData<T, D> {
    /// Iterate the root representatives and their data
    pub(crate) fn iter_sets(&mut self) -> impl Iterator<Item = (T, D)> + use<'_, T, D> {
        (0..self.repr.len()).filter_map(|i0| {
            let i0 = i0.into();
            let i = self.find(i0);
            if i != i0 {
                return None;
            }
            Some((i, self.data[i.into()].clone()))
        })
    }
}
impl<T: Id, D> UFData<T, D> {
    /// Make repr only contain canonical data.
    fn canonicalize(&self) {
        for i in 0..self.repr.len() {
            let _: T = self.find(i.into());
        }
    }
    pub(crate) fn new() -> Self {
        Self {
            repr: vec![],
            data: vec![],
        }
    }
    pub(crate) fn find(&self, i: T) -> T {
        if i == self.repr[i.into()].get() {
            i
        } else {
            let new_repr = self.find(self.repr[i.into()].get());
            self.repr[i.into()].set(new_repr);
            new_repr
        }
    }
}
impl<T: Id, D> Index<T> for UFData<T, D> {
    type Output = D;

    fn index(&self, i: T) -> &Self::Output {
        &self.data[self.find(i).into()]
    }
}
impl<T: Id, D> IndexMut<T> for UFData<T, D> {
    fn index_mut(&mut self, i: T) -> &mut Self::Output {
        let idx = self.find(i).into();
        &mut self.data[idx]
    }
}
impl<T: Id, D: Clone> UFData<T, D> {
    /// Add a new entry
    pub(crate) fn add(&mut self, data: D) -> T {
        let id: T = self.repr.len().into();
        self.repr.push(Cell::new(id));
        self.data.push(data);
        id
    }

    /// Union a and b, calls `merge` if a and b are different
    ///
    /// Merge returns a result, if Err, it means it is not possible to merge
    /// the two data values and the union is canceled
    ///
    pub(crate) fn union_merge<E, F: Merge<D, E>>(
        &mut self,
        i: T,
        j: T,
        mut merge: F,
    ) -> Result<Option<(T, T)>, E> {
        let (i, j) = (self.find(i), self.find(j));
        if i == j {
            return Ok(None);
        }
        let a = self.data[i.into()].clone();
        let b = self.data[j.into()].clone();

        // merge canceled if err
        let res = (merge)(a, b)?;

        self.data[j.into()] = res;
        self.repr[j.into()].set(i);
        Ok(Some((i, j)))
    }
}

impl<T: Id, D: Clone + Eq> UFData<T, D> {
    pub(crate) fn union_eq(&mut self, i: T, j: T) -> Result<Option<(T, T)>, ()> {
        self.union_merge(i, j, |a, b| if a == b { Ok(a) } else { Err(()) })
    }
}

impl<T: Id> UF<T> {
    pub(crate) fn union(&mut self, i: T, j: T) -> Option<(T, T)> {
        let res: Result<_, Uninhabited> = self.union_merge(i, j, |(), ()| Ok(()));
        let Ok(res) = res;
        res
    }
}
