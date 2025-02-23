use std::{
    cell::Cell,
    hash::Hash,
    ops::{Index, IndexMut},
};

use itertools::Itertools as _;

use crate::ids::Id;

#[derive(Debug)]
pub(crate) enum Uninhabited {}

/// Type alias for union-find without data.
pub(crate) type UF<T> = UFData<T, ()>;

/// Union Find with optional attached data.
///
/// Indexing canonicalizes the index and returns the associated data for that index.
// TODO lgustafsson for emagnusson: Removed Eq, Hash etc because they weren't used anyway
#[derive(Clone, Debug, Default)]
pub(crate) struct UFData<K: Id, V> {
    repr: Vec<Cell<K>>,
    data: Vec<V>,
    sets: Vec<Vec<K>>,
}
impl<K: Id, V: Clone> UFData<K, V> {
    pub(crate) fn new_with_size(n: usize, default: V) -> Self {
        Self {
            repr: (0..n).map(K::from).map(Cell::new).collect(),
            data: vec![default; n],
            sets: (0..n).map(K::from).map(|i| vec![i]).collect(),
        }
    }
}

impl<K: Id, V> UFData<K, V> {
    pub(crate) fn new() -> Self {
        Self {
            repr: vec![],
            data: vec![],
            sets: vec![],
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
    /// The set that this element belongs to
    pub(crate) fn set(&self, i: K) -> &[K] {
        let i = self.find(i);
        &self.sets[i.into()]
    }

    /// Iterate the root representatives and their data
    pub(crate) fn iter_roots(&self) -> impl Iterator<Item = (K, &V)> + use<'_, K, V> {
        (0..self.repr.len()).filter_map(|i0| {
            let i0 = i0.into();
            let i = self.find(i0);
            if i != i0 {
                return None;
            }
            Some((i, &self.data[i.into()]))
        })
    }
    pub(crate) fn iter_sets(&self) -> impl Iterator<Item = &[K]> {
        (0..self.repr.len())
            .map(|i| self.sets[self.find(i.into()).into()].as_slice())
            .unique()
    }
    /// Iterate the sets that contain > 1 element.
    pub(crate) fn iter_merged_sets(&self) -> impl Iterator<Item = &[K]> {
        (0..self.repr.len()).filter_map(|i0| {
            let i0 = i0.into();
            let i = self.find(i0);
            if i != i0 && self.sets[i.into()].len() > 1 {
                None
            } else {
                Some(self.sets[i.into()].as_slice())
            }
        })
    }
    /// Iterate all entries, including non-root.
    ///
    /// Iterator element is (id, find(id), find(id).value)
    pub(crate) fn iter_all(&self) -> impl Iterator<Item = (K, K, &V)> + use<'_, K, V> {
        (0..self.repr.len()).map(|i0| {
            let i0 = i0.into();
            let i = self.find(i0);
            (i, i0, &self.data[i.into()])
        })
    }

    /// Add a new entry
    pub(crate) fn push(&mut self, data: V) -> K {
        let id: K = self.repr.len().into();
        self.repr.push(Cell::new(id));
        self.data.push(data);
        self.sets.push(vec![id]);
        id
    }
}
impl<K: Id, V: Clone> UFData<K, V> {
    /// Union a and b, calls `merge` if a and b are different
    ///
    /// Merge returns a result, if Err, it means it is not possible to merge
    /// the two data values and the union is canceled
    pub(crate) fn try_union_merge<E, F: FnMut(V, V) -> Result<V, E>>(
        &mut self,
        i: K,
        j: K,
        mut merge: F,
    ) -> Result<Option<(K, K)>, E> {
        let (src, target) = (self.find(i), self.find(j));
        if src == target {
            return Ok(None);
        }
        let a = self.data[src.into()].clone();
        let b = self.data[target.into()].clone();

        // merge canceled if err
        let res = (merge)(a, b)?;

        let mut new_set = Vec::new();
        new_set.extend(self.sets[src.into()].clone());
        new_set.extend(self.sets[target.into()].clone());

        self.sets[target.into()] = new_set;
        self.data[target.into()] = res;
        self.repr[src.into()].set(target);
        Ok(Some((src, target)))
    }
    pub(crate) fn union_merge<F: FnMut(V, V) -> V>(&mut self, i: K, j: K, mut merge: F) {
        let Ok(_) = self.try_union_merge::<Uninhabited, _>(i, j, |a, b| Ok(merge(a, b)));
    }
}

impl<T: Id, D: Clone + Eq> UFData<T, D> {
    pub(crate) fn union_eq(&mut self, i: T, j: T) -> Result<Option<(T, T)>, ()> {
        self.try_union_merge(i, j, |a, b| if a == b { Ok(a) } else { Err(()) })
    }
}

impl<T: Id> UF<T> {
    pub(crate) fn union(&mut self, i: T, j: T) -> Option<(T, T)> {
        let res: Result<_, Uninhabited> = self.try_union_merge(i, j, |(), ()| Ok(()));
        let Ok(res) = res;
        res
    }
    pub(crate) fn union_groups(&mut self, iter: impl Iterator<Item = Vec<T>>) {
        for x in iter {
            for w in x.windows(2) {
                self.union(w[0], w[1]);
            }
        }
    }
}

impl<K: Id, V: Clone> FromIterator<V> for UFData<K, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut uf = UFData::new();
        for x in iter {
            uf.push(x);
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
