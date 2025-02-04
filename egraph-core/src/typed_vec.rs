use std::cmp::Eq;
use std::fmt::Debug;
use std::marker::PhantomData;

use crate::ids::Id;

/// Vec with typed indexes.
#[derive(Clone, PartialEq, Eq, Default)]
pub(crate) struct TVec<K, V> {
    x: Vec<V>,
    _marker: PhantomData<K>,
}
impl<K, V> Extend<V> for TVec<K, V> {
    fn extend<T: IntoIterator<Item = V>>(&mut self, iter: T) {
        self.x.extend(iter);
    }
}

impl<K: Id, V: Clone> TVec<K, V> {
    pub(crate) fn new_with_size(n: usize, default: V) -> Self {
        Self {
            x: vec![default; n],
            _marker: PhantomData,
        }
    }
}
impl<K, V> TVec<K, V> {
    pub(crate) fn new() -> Self {
        Self {
            x: Vec::new(),
            _marker: PhantomData,
        }
    }
    pub(crate) fn len(&self) -> usize {
        self.x.len()
    }
}
impl<K: Id, V> TVec<K, V> {
    pub(crate) fn iter(&self) -> impl Iterator<Item=&V> {
        self.x.iter()
    }
    pub(crate) fn indexed_iter(&self) -> impl Iterator<Item = (K, &V)> {
        (0..).map(K::from).zip(self.x.iter())
    }
    pub(crate) fn push(&mut self, expected_id: K, v: V) {
        assert_eq!(self.x.len(), expected_id.into());
        self.x.push(v);
    }
    pub(crate) fn add(&mut self, v: V) -> K {
        let id = self.x.len().into();
        self.x.push(v);
        id
    }
    pub(crate) fn all(&self) -> Vec<K> {
        (0..self.x.len()).map(Into::into).collect()
    }
    pub(crate) fn inner(&self) -> &Vec<V> {
        &self.x
    }
}
impl<K: Id, V> std::ops::Index<K> for TVec<K, V> {
    type Output = V;

    fn index(&self, idx: K) -> &Self::Output {
        &self.x[idx.into()]
    }
}
impl<K: Id, V> std::ops::IndexMut<K> for TVec<K, V> {
    fn index_mut(&mut self, idx: K) -> &mut Self::Output {
        &mut self.x[idx.into()]
    }
}
impl<K, V: Debug> Debug for TVec<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.x.fmt(f)
    }
}
impl<K, V> FromIterator<V> for TVec<K, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut x = Self::new();
        x.extend(iter);
        x
    }
}
