use crate::{ids::id_wrap, typed_vec::*};
use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    cmp::Ordering,
    collections::{BTreeMap, BTreeSet},
    convert::{Infallible, TryFrom},
    fmt::{self, Debug, Display},
    hash::Hash,
    iter::FromIterator,
    ops::{BitOr, Deref, DerefMut, Index, IndexMut},
    str::FromStr,
};

type Map<K, V> = BTreeMap<K, V>;
type BiMap<K, V> = (BTreeMap<K, V>, BTreeMap<V, K>);
type Set<K> = BTreeSet<K>;

id_wrap!(EclassId, "e", "id for an Eclass");
id_wrap!(Slot, "s", "id for a slot");

fn apply_perm<K: Copy + Ord>(src: &Map<K, K>, trans: &Map<K, K>) -> Map<K, K> {
    src.into_iter()
        .filter_map(|(k, v)| Some((*k, trans.get(v).copied()?)))
        .collect()
}

struct Eclass {/* .. */}

struct Egraph {
    eclasses: TVec<EclassId, Eclass>,
}

struct UnionFind {
    parents: TVec<EclassId, (EclassId, Map<Slot, Slot>)>,
}
impl UnionFind {
    // NOTE: if we always do path compression, then we could return a map reference instead,
    // although this is a bit tricky to handle with the borrow checker.
    fn find(&self, x: EclassId) -> (EclassId, Map<Slot, Slot>) {
        Self::find_inner(&self.parents, x)
    }
    fn find_inner(
        parents: &TVec<EclassId, (EclassId, Map<Slot, Slot>)>,
        x: EclassId,
    ) -> (EclassId, Map<Slot, Slot>) {
        let parent = parents[x].0;
        let map = &parents[x].1;

        if parent == x {
            return (parent, map.clone());
        }

        let (next_parent, next_map) = Self::find_inner(parents, parent);

        let map = &parents[x].1;
        let rval = (next_parent, apply_perm(&map, &next_map));

        rval
    }
}
