//! TVec: Vec with typed indexes

use crate::ids::Id;
use itertools::Itertools as _;
use std::{cmp::Eq, fmt::Debug, marker::PhantomData};

/// Vec with typed indexes.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct TVec<K, V> {
    x: Vec<V>,
    _marker: PhantomData<K>,
}

impl<K: Id, V> Default for TVec<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! tvec {
    [$($tt:tt)*] => { TVec::from(vec![$($tt)*]) };
}
pub(crate) use tvec;

impl<K: Id, V: Clone> TVec<K, V> {
    /// analogous to `vec![default; n]`;
    pub(crate) fn new_with_size(n: usize, default: V) -> Self {
        Self {
            x: vec![default; n],
            _marker: PhantomData,
        }
    }
}
impl<K: Id, V> TVec<K, V> {
    pub(crate) fn new() -> Self {
        Self {
            x: Vec::new(),
            _marker: PhantomData,
        }
    }
    pub(crate) fn len(&self) -> usize {
        self.x.len()
    }
    pub(crate) fn iter(&self) -> impl Iterator<Item = &V> {
        self.x.iter()
    }
    #[allow(unused)]
    pub(crate) fn iter_mut(&mut self) -> impl Iterator<Item = &mut V> {
        self.x.iter_mut()
    }
    pub(crate) fn enumerate(&self) -> impl Iterator<Item = K> + use<K, V> {
        (0..self.len()).map(K::from)
    }
    /// `.iter().enumerate()` with typed indexes
    pub(crate) fn into_iter_enumerate(self) -> impl Iterator<Item = (K, V)> {
        (0..).map(K::from).zip(self.x)
    }
    pub(crate) fn iter_enumerate(&self) -> impl Iterator<Item = (K, &V)> {
        (0..).map(K::from).zip(self.x.iter())
    }
    pub(crate) fn iter_enumerate_mut(&mut self) -> impl Iterator<Item = (K, &mut V)> {
        (0..).map(K::from).zip(self.x.iter_mut())
    }
    pub(crate) fn push_expected(&mut self, expected_id: K, v: V) {
        assert_eq!(self.x.len(), expected_id.into());
        self.x.push(v);
    }
    pub(crate) fn push(&mut self, v: V) -> K {
        let id = self.x.len().into();
        self.x.push(v);
        id
    }
    pub(crate) fn new_same_size<V2: Default + Clone>(&self) -> TVec<K, V2> {
        TVec::new_with_size(self.len(), Default::default())
    }
    pub(crate) fn inner(&self) -> &Vec<V> {
        &self.x
    }
    pub(crate) fn inner_mut(&mut self) -> &mut Vec<V> {
        &mut self.x
    }
    pub(crate) fn map<'a, V2>(&'a self, f: impl FnMut(&'a V) -> V2) -> TVec<K, V2> {
        self.iter().map(f).collect()
    }
    pub(crate) fn from_iter_ordered(iter: impl Iterator<Item = (K, V)>) -> Self {
        iter.enumerate()
            .map(|(expected_id, (id, value))| {
                assert_eq!(id, K::from(expected_id));
                value
            })
            .collect()
    }
    /// Collect values that arrive out-of-order
    /// asserts that the ids are contiguous.
    pub(crate) fn from_iter_unordered(iter: impl Iterator<Item = (K, V)>) -> Self {
        iter.sorted_by_key(|(k, _)| *k)
            .enumerate()
            .map(|(expected_id, (current_id, value))| {
                assert_eq!(K::from(expected_id), current_id);
                value
            })
            .collect()
    }
}
impl<K: Id> TVec<K, bool> {
    pub(crate) fn into_remap_table(self) -> TVec<K, Option<K>> {
        let mut next = 0;
        self.map(|keep| {
            keep.then(|| {
                next += 1;
                K::from(next - 1)
            })
        })
    }
}

impl<K, V> From<Vec<V>> for TVec<K, V> {
    fn from(x: Vec<V>) -> Self {
        Self {
            x,
            _marker: PhantomData,
        }
    }
}

impl<K: Id, V> std::ops::Index<K> for TVec<K, V> {
    type Output = V;

    fn index(&self, idx: K) -> &Self::Output {
        &self.x[idx.into()]
    }
}
impl<K: Id, V> std::ops::Index<&K> for TVec<K, V> {
    type Output = V;

    fn index(&self, idx: &K) -> &Self::Output {
        self.index(*idx)
    }
}
impl<K: Id, V> std::ops::Index<std::ops::Range<K>> for TVec<K, V> {
    type Output = [V];

    fn index(&self, idx: std::ops::Range<K>) -> &Self::Output {
        &self.x[idx.start.into()..idx.end.into()]
    }
}
impl<K: Id, V> std::ops::IndexMut<K> for TVec<K, V> {
    fn index_mut(&mut self, idx: K) -> &mut Self::Output {
        &mut self.x[idx.into()]
    }
}
impl<K: Id, V> std::ops::IndexMut<&K> for TVec<K, V> {
    fn index_mut(&mut self, idx: &K) -> &mut Self::Output {
        self.index_mut(*idx)
    }
}
impl<K: Id + Debug, V: Debug> Debug for TVec<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter_enumerate()).finish()
    }
}
impl<K: Id, V> FromIterator<V> for TVec<K, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut x = Self::new();
        x.extend(iter);
        x
    }
}
impl<K: Id, V> Extend<V> for TVec<K, V> {
    fn extend<T: IntoIterator<Item = V>>(&mut self, iter: T) {
        self.x.extend(iter);
    }
}
impl<K: Id, V> IntoIterator for TVec<K, V> {
    type Item = V;

    type IntoIter = <Vec<V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.x.into_iter()
    }
}
impl<'a, K: Id, V> IntoIterator for &'a TVec<K, V> {
    type Item = &'a V;

    type IntoIter = <&'a Vec<V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.x.iter()
    }
}
impl<'a, K: Id, V> IntoIterator for &'a mut TVec<K, V> {
    type Item = &'a mut V;

    type IntoIter = <&'a mut Vec<V> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.x.iter_mut()
    }
}
