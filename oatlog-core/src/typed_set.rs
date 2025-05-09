use crate::{ids::Id, typed_vec::TVec};
use std::collections::{BTreeMap, BTreeSet};

/// Set with typed index handles.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub(crate) struct TSet<K: Id, V: Ord + Clone> {
    v: TVec<K, V>,
    h: BTreeMap<V, K>,
}

impl<K: Id, V: Ord + Clone> TSet<K, V> {
    pub(crate) fn new() -> Self {
        Self {
            v: TVec::new(),
            h: BTreeMap::new(),
        }
    }
    pub(crate) fn len(&self) -> usize {
        self.v.len()
    }
    pub(crate) fn as_tvec(&self) -> &TVec<K, V> {
        &self.v
    }
    pub(crate) fn to_set(&self) -> BTreeSet<V> {
        self.v.iter().cloned().collect()
    }
    pub(crate) fn insert(&mut self, v: V) -> K {
        if let Some(&id) = self.h.get(&v) {
            id
        } else {
            let id = self.v.push(v.clone());
            self.h.insert(v, id);
            id
        }
    }
}
impl<K: Id, V: Ord + Clone> Extend<V> for TSet<K, V> {
    fn extend<T: IntoIterator<Item = V>>(&mut self, iter: T) {
        for t in iter {
            self.insert(t);
        }
    }
}
impl<K: Id, V: Ord + Clone> FromIterator<V> for TSet<K, V> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut x = Self::new();
        x.extend(iter);
        x
    }
}
