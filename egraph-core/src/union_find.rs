use crate::ids::Id;
use std::{
    cell::Cell,
    mem,
    ops::{Index, IndexMut},
};

#[derive(Debug)]
pub(crate) enum Uninhabited {}

/// Type alias for union-find without data.
pub(crate) type UF<T> = UFData<T, ()>;

/// Union Find with optional attached data.
///
/// Indexing canonicalizes the index and returns the associated data for that index.
#[derive(Clone, Debug, Default)]
pub(crate) struct UFData<K: Id, V> {
    inner: Vec<UFElement<K, V>>,
}
#[derive(Clone, Debug)]
enum UFElement<K: Id, V> {
    Root { value: V, set: Vec<K> },
    Child { parent: Cell<K> },
}
impl<K: Id, V: Clone> UFData<K, V> {
    pub(crate) fn new_with_size(n: usize, default: V) -> Self {
        Self {
            inner: (0..n)
                .map(|i| UFElement::Root {
                    value: default.clone(),
                    set: vec![K::from(i)],
                })
                .collect(),
        }
    }
}

impl<K: Id, V> UFData<K, V> {
    pub(crate) fn new() -> Self {
        Self { inner: Vec::new() }
    }
    pub(crate) fn find(&self, i: K) -> K {
        self.find_root(i).0
    }
    fn find_root(&self, i: K) -> (K, &V, &[K]) {
        match &self.inner[i.into()] {
            UFElement::Root { value, set } => (i, &value, &set),
            UFElement::Child { parent } => {
                let ret = self.find_root(parent.get());
                parent.set(ret.0);
                ret
            }
        }
    }
    /// The set that this element belongs to
    pub(crate) fn set(&self, i: K) -> &[K] {
        self.find_root(i).2
    }

    /// Iterate the root representatives and their data
    pub(crate) fn iter_roots(&self) -> impl Iterator<Item = (K, &V)> + use<'_, K, V> {
        self.inner
            .iter()
            .enumerate()
            .filter_map(|(i, el)| match el {
                UFElement::Root { value, .. } => Some((i.into(), value)),
                UFElement::Child { .. } => None,
            })
    }
    pub(crate) fn iter_sets(&self) -> impl Iterator<Item = &[K]> {
        self.inner.iter().filter_map(|el| match el {
            UFElement::Root { set, .. } => Some(set.as_slice()),
            UFElement::Child { .. } => None,
        })
    }
    /// Iterate the sets that contain > 1 element.
    pub(crate) fn iter_merged_sets(&self) -> impl Iterator<Item = &[K]> {
        self.iter_sets().filter(|s| s.len() > 1)
    }
    /// Iterate all entries, including non-root.
    ///
    /// Iterator element is (id, find(id), find(id).value)
    pub(crate) fn iter_all(&self) -> impl Iterator<Item = (K, K, &V)> + use<'_, K, V> {
        (0..self.inner.len()).map(K::from).map(|i| {
            let (parent, value, _) = self.find_root(i);
            (i, parent, value)
        })
    }

    /// Add a new entry
    pub(crate) fn push(&mut self, data: V) -> K {
        let id: K = self.inner.len().into();
        self.inner.push(UFElement::Root {
            value: data,
            set: vec![id],
        });
        id
    }

    /// Union a and b, calls `merge` if a and b are different
    ///
    /// Merge returns a result, if Err, it means it is not possible to merge
    /// the two data values and the union is canceled
    // TODO erik for loke: I don't get why the user provided merge function needs to
    // provide `target_value`, src_value`.
    pub(crate) fn try_union_merge<E, F: FnMut(V, V) -> Result<V, (E, V, V)>>(
        &mut self,
        i: K,
        j: K,
        mut merge: F,
    ) -> Result<Option<(K, K)>, E> {
        let ((mut target, _, target_keys), (mut src, _, src_keys)) =
            (self.find_root(i), self.find_root(j));
        if src == target {
            return Ok(None);
        }
        if target_keys.len() < src_keys.len() {
            (target, src) = (src, target);
        }
        // TODO erik for loke: do we *need* a placeholder here? Can't we just do the lookup again
        // later?
        // The way it is written, it looks like the Err path mutates the collection, (even though
        // it does not).
        let placeholder = || UFElement::Child {
            parent: Cell::new(K::from(usize::MAX)),
        };
        let UFElement::Root {
            value: target_value,
            set: mut target_set,
        } = mem::replace(&mut self.inner[target.into()], placeholder())
        else {
            unreachable!()
        };
        let UFElement::Root {
            value: src_value,
            set: src_set,
        } = mem::replace(&mut self.inner[src.into()], placeholder())
        else {
            unreachable!()
        };
        match merge(target_value, src_value) {
            Ok(value) => {
                self.inner[target.into()] = UFElement::Root {
                    value,
                    set: {
                        target_set.extend(src_set);
                        target_set
                    },
                };
                self.inner[src.into()] = UFElement::Child {
                    parent: Cell::new(target),
                };
                Ok(Some((target, src)))
            }
            Err((err, target_value, src_value)) => {
                self.inner[target.into()] = UFElement::Root {
                    value: target_value,
                    set: target_set,
                };
                self.inner[src.into()] = UFElement::Root {
                    value: src_value,
                    set: src_set,
                };
                Err(err)
            }
        }
    }
    pub(crate) fn union_merge<F: FnMut(V, V) -> V>(&mut self, i: K, j: K, mut merge: F) {
        let Ok(_) = self.try_union_merge::<Uninhabited, _>(i, j, |a, b| Ok(merge(a, b)));
    }
}

impl<K: Id, V: Eq> UFData<K, V> {
    pub(crate) fn union_eq(&mut self, i: K, j: K) -> Result<Option<(K, K)>, ()> {
        self.try_union_merge(i, j, |a, b| if a == b { Ok(a) } else { Err(((), a, b)) })
    }
}

impl<K: Id> UF<K> {
    pub(crate) fn union(&mut self, i: K, j: K) -> Option<(K, K)> {
        let res: Result<_, Uninhabited> = self.try_union_merge(i, j, |(), ()| Ok(()));
        let Ok(res) = res;
        res
    }
    pub(crate) fn union_groups(&mut self, iter: impl Iterator<Item = Vec<K>>) {
        for x in iter {
            for w in x.windows(2) {
                self.union(w[0], w[1]);
            }
        }
    }
}

impl<K: Id, V> FromIterator<V> for UFData<K, V> {
    fn from_iter<I: IntoIterator<Item = V>>(iter: I) -> Self {
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
        &self.find_root(i).1
    }
}
impl<K: Id, V> IndexMut<K> for UFData<K, V> {
    fn index_mut(&mut self, i: K) -> &mut Self::Output {
        let idx = self.find(i).into();
        let UFElement::Root { value, .. } = &mut self.inner[idx] else {
            unreachable!()
        };
        value
    }
}
