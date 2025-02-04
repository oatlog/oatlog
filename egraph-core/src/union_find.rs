use std::cell::Cell;
use std::ops::{Index, IndexMut};

use crate::ids::Id;

#[derive(Debug)]
pub(crate) enum Uninhabited {}

/// alias for a valid merge function.
pub(crate) trait Merge<D, E>: FnMut(D, D) -> Result<D, E> {}
impl<D, E, T: FnMut(D, D) -> Result<D, E>> Merge<D, E> for T {}

/// Type alias for union-find without data.
pub(crate) type UF<T> = UFData<T, ()>;

/// Union Find with optional attached data.
///
/// Indexing canonicalizes the index and returns the associated data for that index.
#[derive(Clone, Debug)]
pub(crate) struct UFData<K: Id, V> {
    repr: Vec<Cell<K>>,
    data: Vec<V>,
}
impl<K: Id, V: Clone> UFData<K, V> {
    pub(crate) fn new_with_size(n: usize, default: V) -> Self {
        Self {
            repr: (0..n).map(K::from).map(Cell::new).collect(),
            data: vec![default; n],
        }
    }
}
impl<K: Id, V: PartialEq> PartialEq for UFData<K, V> {
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

impl<K: Id, V> UFData<K, V> {
    /// Make repr only contain canonical data.
    fn canonicalize(&self) {
        for i in 0..self.repr.len() {
            let _: K = self.find(i.into());
        }
    }
    pub(crate) fn new() -> Self {
        Self {
            repr: vec![],
            data: vec![],
        }
    }
    pub(crate) fn find(&self, i: K) -> K {
        if i == self.repr[i.into()].get() {
            i
        } else {
            let new_repr = self.find(self.repr[i.into()].get());
            self.repr[i.into()].set(new_repr);
            new_repr
        }
    }
}
impl<K: Id, V: Clone> UFData<K, V> {
    /// Iterate the root representatives and their data
    pub(crate) fn iter_roots(&self) -> impl Iterator<Item = (K, V)> + use<'_, K, V> {
        (0..self.repr.len()).filter_map(|i0| {
            let i0 = i0.into();
            let i = self.find(i0);
            if i != i0 {
                return None;
            }
            Some((i, self.data[i.into()].clone()))
        })
    }

    /// Iterate all entries, including non-root.
    ///
    /// Iterator element is (id, find(id), find(id).value)
    pub(crate) fn iter_all(&self) -> impl Iterator<Item = (K, K, V)> + use<'_, K, V> {
        (0..self.repr.len()).map(|i0| {
            let i0 = i0.into();
            let i = self.find(i0);
            (i, i0, self.data[i.into()].clone())
        })
    }

    /// Add a new entry
    pub(crate) fn add(&mut self, data: V) -> K {
        let id: K = self.repr.len().into();
        self.repr.push(Cell::new(id));
        self.data.push(data);
        id
    }

    /// Union a and b, calls `merge` if a and b are different
    ///
    /// Merge returns a result, if Err, it means it is not possible to merge
    /// the two data values and the union is canceled
    ///
    pub(crate) fn union_merge<E, F: Merge<V, E>>(
        &mut self,
        i: K,
        j: K,
        mut merge: F,
    ) -> Result<Option<(K, K)>, E> {
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

impl<K: Id, V: Clone> FromIterator<V> for UFData<K, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut uf = UFData::new();
        for x in iter {
            uf.add(x);
        }
        uf
    }
}
impl<K: Id, V> Index<K> for UFData<K, V> {
    type Output = V;

    fn index(&self, i: K) -> &Self::Output {
        &self.data[self.find(i).into()]
    }
}
impl<K: Id, V> IndexMut<K> for UFData<K, V> {
    fn index_mut(&mut self, i: K) -> &mut Self::Output {
        let idx = self.find(i).into();
        &mut self.data[idx]
    }
}
