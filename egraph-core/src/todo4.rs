#![allow(unused_parens, unused_variables)]
#![allow(clippy::must_use_candidate, dead_code, reason = "noise until finished")]

use std::{
    collections::BTreeSet,
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

/// Must be produced from a `UnionFind`<T>
trait Eclass: Copy + Clone + Eq + PartialEq + Ord + PartialOrd {
    // const MINX: Self = Self::from(0);
    fn new(value: u32) -> Self;
    fn inner(self) -> u32;
}

/// Per eclass state.
#[derive(Default)]
struct UnionFind<T> {
    // TODO: maybe merge repr and size.
    repr: Vec<u32>,
    /// reprs that should be uprooted.
    dirty: Vec<T>,
    /// if canonicalized, about how many memory locations will need to be modified?
    size: Vec<u32>,
    _marker: PhantomData<T>,
    // forall semi-naive:
    // old: BTreeSet<T>,
    // new: Vec<T>,
    // delta: Vec<T>,
}
impl<T: Eclass> UnionFind<T> {
    //
    fn new() -> Self {
        Self {
            repr: Vec::new(),
            dirty: Vec::new(),
            size: Vec::new(),
            _marker: PhantomData,
        }
    }
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
    fn union(&mut self, a: T, b: T) {
        let a = self.find_inner(a.inner());
        let b = self.find_inner(b.inner());
        let (root, uprooted) = if self.size[a as usize] > self.size[b as usize] {
            (a, b)
        } else {
            (b, a)
        };
        self.repr[uprooted as usize] = self.repr[root as usize];
        self.dirty.push(T::new(uprooted));
    }
    fn dirty(&mut self) -> &mut Vec<T> {
        &mut self.dirty
    }
    /// INVARIANT: also add to delta
    fn add_eclass(&mut self) -> T {
        let id = u32::try_from(self.repr.len()).expect("out of u32 ids");
        self.repr.push(id);
        self.size.push(1);
        T::new(id)
    }
    // inc/dec this possibly non-canonicalized e-class
    // can be no-op if not repr.
    fn inc_eclass(&mut self, t: T, delta: u32) {
        self.size[t.inner() as usize] += delta;
    }
    fn dec_eclass(&mut self, t: T, delta: u32) {
        self.size[t.inner() as usize] -= delta;
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
struct Math(u32);
#[rustfmt::skip]
impl Eclass for Math { fn new(value: u32) -> Self { Self(value) } fn inner(self) -> u32 { self.0 } }

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default)]
struct Other(u32);
#[rustfmt::skip]
impl Eclass for Other { fn new(value: u32) -> Self { Self(value) } fn inner(self) -> u32 { self.0 } }

// trait Relation {
//     const COST: u32;
//
//     fn new() -> Self;
//     fn iter_new<'a>(&'a self) -> impl Iterator<Item = ...>;
//     fn iter_*(&self) -> impl Iterator<Item = ...>;
//     fn check_*(&self) -> impl Iterator<Item = ...>;
// }

// to be added
#[derive(Default)]
struct Delta {
    // add rows
    add_delta: Vec<(<AddRelation as Relation>::Row)>,
    // new math eclasses
    math_delta: Vec<Math>,
}
impl Delta {
    fn make_math(&mut self, uf_math: &mut UnionFind<Math>) -> Math {
        let id = uf_math.add_eclass();
        self.math_delta.push(id);
        id
    }
    fn insert_add(&mut self, x: (Math, Math, Math)) {
        self.add_delta.push(x);
    }
}

trait Relation {
    type Row;
}

#[derive(Default)]
struct AddRelation {
    // ======== PERSISTENT STATE =============
    // relation is ONLY mutated in update()
    new: Vec<(Math, Math, Math)>,
    all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
    all_index_0_2_1: BTreeSet<(Math, Math, Math)>,
    all_index_1_2_0: BTreeSet<(Math, Math, Math)>,
    all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
}
#[rustfmt::skip]
impl AddRelation {
    // permute: ident -> perm
    // inverse: perm -> ident
    const COST: u32 = 12;
    fn new() -> Self { Default::default() }
    fn iter_new(&self) -> impl Iterator<Item = (Math, Math, Math)> { None.into_iter() }
    fn iter_0(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> { None.into_iter() }
    fn iter_0_1(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math)> { None.into_iter() }
    fn iter_0_2(&self, x0: Math, x2: Math) -> impl Iterator<Item = (Math)> { None.into_iter() }
    fn iter_1_2(&self, x0: Math, x2: Math) -> impl Iterator<Item = (Math)> { None.into_iter() }
    fn iter_1(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> { None.into_iter() }
    fn iter_2(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> { None.into_iter() }
}
impl Relation for AddRelation {
    type Row = (Math, Math, Math);
}
impl AddRelation {
    fn update(
        &mut self,
        uproot_math: &[Math],
        uf_math: &mut UnionFind<Math>,
        delta: &mut Vec<(Math, Math, Math)>,
    ) {
        self.new.clear();

        let mut op_insert = take(delta);

        for (x0, x1, x2) in op_insert.iter_mut() {
            *x0 = uf_math.find(*x0);
            *x1 = uf_math.find(*x1);
            *x2 = uf_math.find(*x2);
        }

        let mut op_delete = Vec::new();

        for x0 in uproot_math.iter().copied() {
            for (x1, x2) in self.iter_0(x0) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x1 in uproot_math.iter().copied() {
            for (x0, x2) in self.iter_1(x1) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x2 in uproot_math.iter().copied() {
            for (x0, x1) in self.iter_2(x2) {
                op_delete.push((x0, x1, x2));
            }
        }

        for (x0, x1, x2) in op_delete {
            if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                //
                // ========== SEMANTIC DELETE =============
                //
                uf_math.dec_eclass(x0, Self::COST);
                uf_math.dec_eclass(x1, Self::COST);
                uf_math.dec_eclass(x2, Self::COST);

                self.all_index_0_2_1.remove(&(x0, x2, x1));
                self.all_index_1_2_0.remove(&(x1, x2, x0));
                self.all_index_2_0_1.remove(&(x2, x0, x1));

                op_insert.push((uf_math.find(x0), uf_math.find(x1), uf_math.find(x2)));
            }
        }

        op_insert.retain(|&(x0, x1, x2)| {
            // bi-directional implicit functionality.
            if let Some(old_x2) = self.iter_0_1(x0, x1).next() {
                uf_math.union(x2, old_x2);
                return false;
            }
            if let Some(old_x0) = self.iter_1_2(x1, x2).next() {
                uf_math.union(x0, old_x0);
                return false;
            }
            if let Some(old_x1) = self.iter_0_2(x0, x2).next() {
                uf_math.union(x1, old_x1);
                return false;
            }
            if !self.all_index_0_1_2.contains(&(x0, x1, x2)) {
                return false;
            }
            //
            // ========== SEMANTIC INSERT =============
            //
            self.all_index_0_1_2.insert((x0, x1, x2));
            self.all_index_0_2_1.insert((x0, x2, x1));
            self.all_index_1_2_0.insert((x1, x2, x0));
            self.all_index_2_0_1.insert((x2, x0, x1));

            uf_math.inc_eclass(x0, Self::COST);
            uf_math.inc_eclass(x1, Self::COST);
            uf_math.inc_eclass(x2, Self::COST);

            true
        });

        self.new = op_insert;
    }
}

// Math -> Other
#[derive(Default)]
struct NegRelation {
    // ======== TRANSIENT STATE =============
    // (cleared on update)
    delta: Vec<(Math, Other)>,

    // ======== PERSISTENT STATE =============
    new: Vec<(Math, Other)>,

    all_index_0_1: BTreeSet<(Math, Other)>,
    all_index_1_0: BTreeSet<(Other, Math)>,
    // back_refs can skip one of the columns since we know it already contains the key.
    // we have different back-references per column

    // If 3 is removed from 3 -> (3, 4)
    // then 3 also needs to be removed from 4 -> (3, 4)
    // back_refs_math: BTreeMap<Math, BTreeSet<(Other)>>,
    // back_refs_other: BTreeMap<Other, BTreeSet<(Math)>>,
}

#[rustfmt::skip]
impl NegRelation {
    // cost if inserting/removing a row
    // 2 elements per index * 2 indexes = 4
    const COST: u32 = 4;
    fn new() -> Self { Self::default() }

    fn iter_new(&self) -> impl Iterator<Item = (Math, Other)> + use<'_> { self.new.iter().copied() }

    fn iter(&self) -> impl Iterator<Item = (Math, Other)> + use<'_> { self.all_index_0_1.iter().copied() }
    fn iter_0(&self, x0: Math) -> impl Iterator<Item = (Other)> + use<'_> { self.all_index_0_1.range((x0, Other(u32::MIN))..=(x0, Other(u32::MAX))).map(|(x0, x1)| (*x1)) }
    fn iter_1(&self, x1: Other) -> impl Iterator<Item = (Math)> + use<'_> { self.all_index_1_0.range((x1, Math(u32::MIN))..=(x1, Math(u32::MAX))).map(|(x1, x0)| (*x0)) }
    fn iter_0_1(&self, x0: Math, x1: Other) -> impl Iterator<Item = ()> + use<'_> { self.all_index_1_0.range((x1, x0)..=(x1, x0)).map(|(x1, x0)| ()) }

    fn check_0(&self, x0: Math) -> bool { self.iter_0(x0).next().is_some() }
    fn check_1(&self, x1: Other) -> bool { self.iter_1(x1).next().is_some() }
    fn check_0_1(&self, x0: Math, x1: Other) -> bool { self.iter_0_1(x0, x1).next().is_some() }

    /// Schedule an insert
    fn insert_delta(&mut self, x0: Math, x1: Other) { self.delta.push((x0, x1)); }
}

impl NegRelation {
    // =========== More optimized API =================

    // imagine uprooted as a queue with generational ids for each push.
    //

    // I think the main invariant for UF is that everyone is notified of uprooted.
    // since we push to new, which is not indexed, we can not iterate to fixpoint.
    /// Call exactly once after rules have been applied.
    fn update(
        &mut self,
        uproot_math: &[Math],
        uproot_other: &[Other],
        uf_math: &mut UnionFind<Math>,
        uf_other: &mut UnionFind<Other>,
    ) {
        // safe because rules have been applied.
        self.new.clear();

        let mut op_insert: Vec<_> = take(&mut self.delta);

        for (x0, x1) in &mut op_insert {
            *x0 = uf_math.find(*x0);
            *x1 = uf_other.find(*x1);
        }

        let mut op_delete: Vec<_> = Vec::new();

        for x0 in uproot_math.iter().copied() {
            op_delete.extend(self.iter_0(x0).map(|x1| (x0, x1)));
        }
        for x1 in uproot_other.iter().copied() {
            op_delete.extend(self.iter_1(x1).map(|(x0)| (x0, x1)));
        }
        for (x0, x1) in op_delete {
            if self.all_index_0_1.remove(&(x0, x1)) {
                // ========== TRUE DELETE ========================
                // at this point the delete will be completed.
                // NOTE: the decrements here are only needed for the x0 or x1 that is
                // canonical.
                uf_math.dec_eclass(x0, NegRelation::COST);
                uf_other.dec_eclass(x1, NegRelation::COST);

                self.all_index_1_0.remove(&(x1, x0));
                op_insert.push((uf_math.find(x0), uf_other.find(x1)));
            }
        }

        // bi-directional implicit functionality
        op_insert.retain(|(x0, x1)| {
            if let Some(old_x1) = self.iter_0(*x0).next() {
                uf_other.union(*x1, old_x1);
                return false;
            }
            true
        });
        op_insert.retain(|(x0, x1)| {
            if let Some(old_x0) = self.iter_1(*x1).next() {
                uf_math.union(*x0, old_x0);
                return false;
            }
            // ========== TRUE INSERT ========================
            // at this point the insert will be completed.
            self.all_index_1_0.insert((*x1, *x0));

            uf_math.inc_eclass(*x0, NegRelation::COST);
            uf_other.inc_eclass(*x1, NegRelation::COST);

            true
        });
        for (x0, x1) in op_insert.iter().copied() {
            self.all_index_0_1.insert((x0, x1));
        }

        self.new.take_scratch(&mut op_insert);
    }
}

#[derive(Default)]
pub struct Egraph {
    // ======== TRANSIENT STATE =============
    delta: Delta,

    // ======== PERSISTENT STATE =============
    add_relation: AddRelation,

    // to be uprooted
    // persisted because we can not clear it without invalidating new.
    math_uproot: Vec<Math>,
    other_uproot: Vec<Other>,

    uf_math: UnionFind<Math>,
    uf_other: UnionFind<Other>,
}
impl Egraph {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn run_until_condition(&mut self) {
        let is_done = || true;
        while !is_done() {
            self.step();
        }
    }
    fn step(&mut self) {
        self.apply_rules();

        self.clear_transient();
    }
    fn clear_transient(&mut self) {
        // call update on all relations.
        // self.neg_relation.update(
        //     &self.math_uproot,
        //     &self.other_uproot,
        //     &mut self.uf_math,
        //     &mut self.uf_other,
        // );

        let math_uproot = take(self.uf_math.dirty());

        self.add_relation
            .update(&math_uproot, &mut self.uf_math, &mut self.delta.add_delta);

        // collect newly uprooted.

        self.math_uproot.take_scratch(self.uf_math.dirty());
        // self.other_uproot.take_scratch(self.uf_other.dirty());
    }
    fn apply_rules(&mut self) {
        // commutativity
        // (rule ((= e (Add a b))) ((= e (Add b a))))

        for (a, b, e) in self.add_relation.iter_new() {
            self.delta.insert_add((b, a, e));
        }

        // associativity
        // (rule ((= e (NewAdd a (Add b c)))) ((= e (Add (Add a b) c))))
        // (rule ((= e (Add a (NewAdd b c)))) ((= e (Add (Add a b) c))))

        for (a, t0, e) in self.add_relation.iter_new() {
            for (b, c) in self.add_relation.iter_2(t0) {
                let t1 = self.delta.make_math(&mut self.uf_math);
                self.delta.insert_add((a, b, t1));
                self.delta.insert_add((t1, c, e));
            }
        }

        for (b, c, t0) in self.add_relation.iter_new() {
            for (a, e) in self.add_relation.iter_2(t0) {
                let t1 = self.delta.make_math(&mut self.uf_math);
                self.delta.insert_add((a, b, t1));
                self.delta.insert_add((t1, c, e));
            }
        }

        // bi-directional implicit functionality
        // (rule ((= a (NewAdd b c) (= d (Add b c)))) (= a d))
        // (symmetric)
        for (b, c, a) in self.add_relation.iter_new() {
            for d in self.add_relation.iter_0_1(b, c) {
                self.uf_math.union(a, d);
            }
        }
        // (rule ((= a (NewAdd b c) (= a (Add b d)))) (= c d))
        // (symmetric)
        for (b, c, a) in self.add_relation.iter_new() {
            for d in self.add_relation.iter_0_2(b, a) {
                self.uf_math.union(c, d);
            }
        }
    }

    /* ... user functions to insert ... */
}
