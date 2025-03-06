#![allow(unused_parens, unused_variables)]

use std::{
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
    mem::take,
};

// TODO: store uprooted IN the UF?
struct UnionFind<T> {
    // repr: Vec<Cell<i32>>
    _marker: PhantomData<T>,
}
impl<T> UnionFind<T> {
    fn find(&self, t: T) -> T {
        todo!()
    }
    // returns uprooted
    // TODO: make sure this does smaller-to-larger in terms of e-nodes
    // we can update e-class size estimates when inserting/removing from back-references.
    // we then get accurate counts for, for each e-class, how many rows use it.
    fn union(&mut self, a: T, b: T) -> Option<T> {
        todo!()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Math(u32);
impl Math {
    const MAX: Math = Math(u32::MAX);
    const MIN: Math = Math(u32::MIN);
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
struct Other(u32);
impl Other {
    const MAX: Other = Other(u32::MAX);
    const MIN: Other = Other(u32::MIN);
}

// Math -> Other
#[derive(Default)]
struct NegRelation {
    // ======== TRANSIENT STATE =============
    uprooted: Vec<(Math, Other)>,

    // ======== PERSISTENT STATE =============
    new: Vec<(Math, Other)>,

    all_index_0_1: BTreeSet<(Math, Other)>,
    all_index_1_0: BTreeSet<(Other, Math)>,

    // back_refs can skip one of the columns since we know it already contains the key.
    // we have different back-references per column

    // If 3 is removed from 3 -> (3, 4)
    // then 3 also needs to be removed from 4 -> (3, 4)
    back_refs_math: BTreeMap<Math, BTreeSet<(Other)>>,
    back_refs_other: BTreeMap<Other, BTreeSet<(Math)>>,
}

#[rustfmt::skip]
impl NegRelation {
    fn new() -> Self { Default::default() }

    fn clear_new(&mut self) { self.new.clear() }
    fn iter_new(&self) -> impl Iterator<Item = (Math, Other)> + use<'_> { self.new.iter().copied() }

    fn iter(&self) -> impl Iterator<Item = (Math, Other)> + use<'_> { self.all_index_0_1.iter().copied() }
    fn iter_0(&self, x0: Math) -> impl Iterator<Item = (Other)> + use<'_> { self.all_index_0_1.range((x0, Other::MIN)..=(x0, Other::MAX)).map(|(x0, x1)| (*x1)) }
    fn iter_1(&self, x1: Other) -> impl Iterator<Item = (Math)> + use<'_> { self.all_index_1_0.range((x1, Math::MIN)..=(x1, Math::MAX)).map(|(x1, x0)| (*x0)) }
    fn iter_0_1(&self, x0: Math, x1: Other) -> impl Iterator<Item = ()> + use<'_> { self.all_index_1_0.range((x1, x0)..=(x1, x0)).map(|(x1, x0)| ()) }

    fn check_0(&self, x0: Math) -> bool { self.iter_0(x0).next().is_some() }
    fn check_1(&self, x1: Other) -> bool { self.iter_1(x1).next().is_some() }
    fn check_0_1(&self, x0: Math, x1: Other) -> bool { self.iter_0_1(x0, x1).next().is_some() }

    // =========== Simple API =================

    // insert into old and new if not already present
    fn insert(&mut self, x0: Math, x1: Other) {
        if self.all_index_0_1.contains(&(x0,x1)) { 
            return 
        }
        self.all_index_0_1.insert((x0, x1));
        self.all_index_1_0.insert((x1, x0));
                             
        self.back_refs_math.entry(x0).or_default().insert(x1);
        self.back_refs_other.entry(x1).or_default().insert(x0);

        self.new.push((x0, x1));
    }

    // uproot these E-classes
    fn uproot(&mut self, uproot_math: &[Math], uproot_other: &[Other]) {
        for x0 in uproot_math.iter().copied() {
            for x1 in self.back_refs_math.remove(&x0).into_iter().flatten() {
                if let Some(entry) = self.back_refs_other.get_mut(&x1) { entry.remove(&x0); }
                if !self.all_index_0_1.contains(&(x0, x1)) { 
                    // only a concern if back_refs has duplicates, which is only possible if the
                    // inner type is a vec instead of a set.
                    continue;
                }
                self.uprooted.push((x0, x1));
                self.all_index_0_1.remove(&(x0, x1));
                self.all_index_1_0.remove(&(x1, x0));
            }
        }
        for x1 in uproot_other.iter().copied() {
            for x0 in self.back_refs_other.remove(&x1).into_iter().flatten() {
                if let Some(entry) = self.back_refs_other.get_mut(&x1) { entry.remove(&x0); }
                if !self.all_index_0_1.contains(&(x0, x1)) {
                    // only a concern if back_refs has duplicates, which is only possible if the
                    // inner type is a vec instead of a set.
                    continue;
                }
                self.uprooted.push((x0, x1));
                self.all_index_0_1.remove(&(x0, x1));
                self.all_index_1_0.remove(&(x1, x0));
            }
        }
    }

    // insert uprooted E-classes
    fn insert_uprooted(&mut self, uf_math: &UnionFind<Math>, uf_other: &UnionFind<Other>) {
        for (x0, x1) in take(&mut self.uprooted) {
            let x0 = uf_math.find(x0);
            let x1 = uf_other.find(x1);
            self.insert(x0, x1);
        }
    }

}

struct Egraph {
    // ======== TRANSIENT STATE =============

    // to be inserted into neg_relation
    neg_relation_delta: Vec<(Math, Other)>,

    // to be unified
    math_unify_delta: Vec<(Math, Math)>,
    other_unify_delta: Vec<(Other, Other)>,

    // to be uprooted
    math_uproot: Vec<Math>,
    other_uproot: Vec<Other>,

    // ======== PERSISTENT STATE =============
    neg_relation: NegRelation,

    uf_math: UnionFind<Math>,
    uf_other: UnionFind<Other>,
}
impl Egraph {
    fn new() -> Self {
        todo!()
    }
    fn run_until_condition(&mut self) {
        let is_done = || true;
        while !is_done() {
            self.step();
        }
    }
    fn step(&mut self) {
        self.apply_rules();
        self.neg_relation.clear_new();
        self.clear_transient();
    }
    fn clear_transient(&mut self) {
        for (a, b) in self.math_unify_delta.drain(..) {
            if let Some(x) = self.uf_math.union(a, b) {
                self.math_uproot.push(x);
            }
        }
        for (a, b) in self.other_unify_delta.drain(..) {
            if let Some(x) = self.uf_other.union(a, b) {
                self.other_uproot.push(x);
            }
        }

        for (x0, x1) in self.neg_relation_delta.drain(..) {
            let x0 = self.uf_math.find(x0);
            let x1 = self.uf_other.find(x1);
            self.neg_relation.insert(x0, x1);
        }

        self.neg_relation
            .uproot(&self.math_uproot, &self.other_uproot);
        self.neg_relation
            .insert_uprooted(&self.uf_math, &self.uf_other);

        self.math_uproot.clear();
        self.other_uproot.clear();
    }
    fn apply_rules(&mut self) {
        // bi-directional implicit functionality
        // (rule ((= (Neg a) (Neg b))) ((= a b)))
        // (rule ((= a (Neg x)) (= b (Neg x))) ((= a b)))

        for (x0, x1) in self.neg_relation.iter_new() {
            for x2 in self.neg_relation.iter_0(x0) {
                self.other_unify_delta.push((x1, x2));
            }
            for x2 in self.neg_relation.iter_1(x1) {
                self.math_unify_delta.push((x0, x2));
            }
        }
    }

    /* ... user functions to insert ... */
}
