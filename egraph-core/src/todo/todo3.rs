#![allow(unused_parens, unused_variables)]

use std::{
    collections::{BTreeMap, BTreeSet},
    marker::PhantomData,
    mem::{swap, take},
};

trait Clear: Sized {
    fn clear(&mut self);
    /// set self to other and clear other without allocations
    fn take_scratch(&mut self, other: &mut Self) {
        self.clear();
        swap(self, other);
    }
}
impl<T> Clear for Vec<T> {
    fn clear(&mut self) {
        Vec::clear(self);
    }
}

trait Eclass: Copy + Clone + Eq + PartialEq + Ord + PartialOrd {
    // const MINX: Self = Self::from(0);
    fn new(value: u32) -> Self;
    fn inner(self) -> u32;
    fn min() -> Self {
        Self::new(0)
    }
    fn max() -> Self {
        Self::new(u32::MAX)
    }
}

#[derive(Default)]
struct UnionFind<T> {
    // TODO: maybe merge repr and size.
    repr: Vec<u32>,
    /// reprs that should be uprooted.
    dirty: Vec<T>,
    /// (Approximate) number of *e-nodes* this set contains.
    size: Vec<u32>,
    _marker: PhantomData<T>,
}
impl<T: Eclass> UnionFind<T> {
    fn find(&mut self, t: T) -> T {
        T::new(self.find_inner(t.inner()))
    }
    fn find_inner(&mut self, i: u32) -> u32 {
        if self.repr[i as usize] == i {
            i
        } else {
            let root = self.find_inner(self.repr[i as usize]);
            self.repr[i as usize] = root;
            root
        }
    }
    // returns uprooted
    // TODO: make sure this does smaller-to-larger in terms of e-nodes
    // we can update e-class size estimates when inserting/removing from back-references.
    // we then get accurate counts for, for each e-class, how many rows use it.
    fn union(&mut self, a: T, b: T) -> Option<T> {
        todo!()
        // dirty.push(...)
    }
    fn dirty(&mut self) -> &mut Vec<T> {
        &mut self.dirty
    }
    fn add_eclass(&mut self) -> T {
        let id = self.repr.len() as u32;
        self.repr.push(id);
        self.size.push(1);
        T::new(id)
    }
    fn inc_eclass(&mut self, t: T, delta: usize) {
        let idx = self.find_inner(t.inner());
    }
    fn dec_eclass(&mut self, t: T, delta: usize) {
        todo!()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
struct Math(u32);
impl Math {
    const MAX: Math = Math(u32::MAX);
    const MIN: Math = Math(u32::MIN);
}
#[rustfmt::skip]
impl From<u32> for Math { fn from(value: u32) -> Self { Self(value) } }
#[rustfmt::skip]
impl From<Math> for u32 { fn from(val: Math) -> Self { val.0 } }
impl Eclass for Math {
    fn new(value: u32) -> Self {
        todo!()
    }

    fn inner(self) -> u32 {
        todo!()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
struct Other(u32);
impl Other {
    const MAX: Other = Other(u32::MAX);
    const MIN: Other = Other(u32::MIN);
}
#[rustfmt::skip]
impl From<u32> for Other { fn from(value: u32) -> Self { Self(value) } }
#[rustfmt::skip]
impl From<Other> for u32 { fn from(val: Other) -> Self { val.0 } }
impl Eclass for Other {
    fn new(value: u32) -> Self {
        todo!()
    }

    fn inner(self) -> u32 {
        todo!()
    }
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
}

impl NegRelation {
    // =========== More optimized API =================

    // imagine uprooted as a queue with generational ids for each push.
    //

    // I think the main invariant for UF is that everyone is notified of uprooted.
    // since we push to new, which is not indexed, we can not iterate to fixpoint.
    fn update(
        &mut self,
        insert: &[(Math, Other)],
        uproot_math: &[Math],
        uproot_other: &[Other],
        uf_math: &mut UnionFind<Math>,
        uf_other: &mut UnionFind<Other>,
    ) {
        let mut op_insert: Vec<_> = insert
            .iter()
            .copied()
            .map(|(x0, x1)| (uf_math.find(x0), uf_other.find(x1)))
            .collect();

        let mut op_delete: Vec<_> = Vec::new();

        // =============== TOUCH back_refs_math and back_refs_other  ===============
        for x0 in uproot_math.iter().copied() {
            for x1 in self.back_refs_math.remove(&x0).into_iter().flatten() {
                if let Some(entry) = self.back_refs_other.get_mut(&x1) {
                    entry.remove(&x0);
                }
                op_insert.push((uf_math.find(x0), uf_other.find(x1)));
                op_delete.push((x0, x1));
            }
        }
        for x1 in uproot_other.iter().copied() {
            for x0 in self.back_refs_other.remove(&x1).into_iter().flatten() {
                if let Some(entry) = self.back_refs_math.get_mut(&x0) {
                    entry.remove(&x1);
                }
                op_insert.push((uf_math.find(x0), uf_other.find(x1)));
                op_delete.push((x0, x1));
            }
        }

        op_insert.sort();
        op_insert.dedup();

        op_delete.sort();
        op_delete.dedup();

        // =========== START OF DATABASE INCONSISTENCY =============
        // FIND is now forbidden to remain consistent

        // =========== TOUCH all_index_0_1 ========================

        for (x0, x1) in op_delete.iter().copied() {
            self.all_index_0_1.remove(&(x0, x1));
        }

        // implicit functionality
        op_insert.retain(|(x0, x1)| {
            if let Some(old_x1) = self.iter_0(*x0).next() {
                uf_other.union(*x1, old_x1);
                // cancel insert if it already exists or would just be uprooted later.
                return false;
            }
            true
        });

        // =========== TOUCH all_index_1_0 ========================

        for (x0, x1) in op_delete.iter().copied() {
            self.all_index_1_0.remove(&(x1, x0));
        }

        // implicit functionality and entry
        // after this, we know that the insert will complete
        op_insert.retain(|(x0, x1)| {
            if let Some(old_x0) = self.iter_1(*x1).next() {
                uf_math.union(*x0, old_x0);
                return false;
            }
            self.all_index_1_0.insert((*x1, *x0));
            true
        });

        // =========== TOUCH all_index_0_1 again :( ========================
        for (x0, x1) in op_insert.iter().copied() {
            self.all_index_0_1.insert((x0, x1));
        }

        // =============== TOUCH back_refs_math again ===============

        for (x0, x1) in op_insert.iter().copied() {
            self.back_refs_math.entry(x0).or_default().insert(x1);
        }

        // =============== TOUCH back_refs_other again ===============

        for (x0, x1) in op_insert.iter().copied() {
            self.back_refs_other.entry(x1).or_default().insert(x0);
        }

        // =============== TOUCH new ===========================
        self.new.clear();
        self.new.extend(op_insert.iter().copied());

        // =========== END OF DATABASE INCONSISTENCY =============
    }
}

#[derive(Default)]
struct Egraph {
    // ======== TRANSIENT STATE =============

    // to be inserted into neg_relation
    neg_relation_delta: Vec<(Math, Other)>,

    new_uproot_math: Vec<Math>,
    new_uproot_other: Vec<Other>,

    // ======== PERSISTENT STATE =============

    // persisted because we can not clear it without invalidating new.
    // to be uprooted
    math_uproot: Vec<Math>,
    other_uproot: Vec<Other>,

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

        // clearing new is safe after rules have been applied.
        self.neg_relation.clear_new();

        self.clear_transient();
    }
    fn clear_transient(&mut self) {
        // call update on all relations.
        self.neg_relation.update(
            &self.neg_relation_delta,
            &self.math_uproot,
            &self.other_uproot,
            &mut self.uf_math,
            &mut self.uf_other,
        );
        self.neg_relation_delta.clear();

        // collect newly uprooted.

        self.math_uproot.take_scratch(self.uf_math.dirty());
        self.other_uproot.take_scratch(self.uf_other.dirty());
    }
    fn apply_rules(&mut self) {
        // bi-directional implicit functionality
        // (rule ((= (Neg a) (Neg b))) ((= a b)))
        // (rule ((= a (Neg x)) (= b (Neg x))) ((= a b)))

        for (x0, x1) in self.neg_relation.iter_new() {
            for x2 in self.neg_relation.iter_0(x0) {
                if let Some(x) = self.uf_other.union(x1, x2) {
                    self.other_uproot.push(x);
                }
            }
            for x2 in self.neg_relation.iter_1(x1) {
                if let Some(x) = self.uf_math.union(x0, x2) {
                    self.math_uproot.push(x);
                }
            }
        }
    }

    /* ... user functions to insert ... */
}
