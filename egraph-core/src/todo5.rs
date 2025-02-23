#![allow(unused)]
#![allow(dead_code)]
#![allow(private_interfaces)]
use crate::runtime::*;
use std::{collections::BTreeSet, mem::take};
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub struct I64(u32);
impl Eclass for I64 {
    fn new(value: u32) -> Self {
        Self(value)
    }
    fn inner(self) -> u32 {
        self.0
    }
}
impl RelationElement for I64 {
    const MIN_ID: Self = Self(0);
    const MAX_ID: Self = Self(u32::MAX);
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub struct F64(u32);
impl Eclass for F64 {
    fn new(value: u32) -> Self {
        Self(value)
    }
    fn inner(self) -> u32 {
        self.0
    }
}
impl RelationElement for F64 {
    const MIN_ID: Self = Self(0);
    const MAX_ID: Self = Self(u32::MAX);
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub struct String(u32);
impl Eclass for String {
    fn new(value: u32) -> Self {
        Self(value)
    }
    fn inner(self) -> u32 {
        self.0
    }
}
impl RelationElement for String {
    const MIN_ID: Self = Self(0);
    const MAX_ID: Self = Self(u32::MAX);
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub struct Bool(u32);
impl Eclass for Bool {
    fn new(value: u32) -> Self {
        Self(value)
    }
    fn inner(self) -> u32 {
        self.0
    }
}
impl RelationElement for Bool {
    const MIN_ID: Self = Self(0);
    const MAX_ID: Self = Self(u32::MAX);
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub struct Unit(u32);
impl Eclass for Unit {
    fn new(value: u32) -> Self {
        Self(value)
    }
    fn inner(self) -> u32 {
        self.0
    }
}
impl RelationElement for Unit {
    const MIN_ID: Self = Self(0);
    const MAX_ID: Self = Self(u32::MAX);
}
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub struct Math(u32);
impl Eclass for Math {
    fn new(value: u32) -> Self {
        Self(value)
    }
    fn inner(self) -> u32 {
        self.0
    }
}
impl RelationElement for Math {
    const MIN_ID: Self = Self(0);
    const MAX_ID: Self = Self(u32::MAX);
}
#[derive(Debug, Default)]
struct ForallMathRelation {
    new: BTreeSet<<Self as Relation>::Row>,
    all: BTreeSet<<Self as Relation>::Row>,
}
impl Relation for ForallMathRelation {
    type Row = (Math);
}
#[derive(Debug, Default)]
struct MulRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
    all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
    all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
}
impl Relation for MulRelation {
    type Row = (Math, Math, Math);
}
impl MulRelation {
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
        self.new.iter().copied()
    }
    fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, Math(u32::MIN), Math(u32::MIN))..=(x0, Math(u32::MAX), Math(u32::MAX)))
            .copied()
            .map(|(x0, x1, x2)| (x1, x2))
    }
    fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_1_0_2
            .range((x1, Math(u32::MIN), Math(u32::MIN))..=(x1, Math(u32::MAX), Math(u32::MAX)))
            .copied()
            .map(|(x1, x0, x2)| (x0, x2))
    }
    fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_2_0_1
            .range((x2, Math(u32::MIN), Math(u32::MIN))..=(x2, Math(u32::MAX), Math(u32::MAX)))
            .copied()
            .map(|(x2, x0, x1)| (x0, x1))
    }
    fn check1_0_1_2(&self, x0: Math) -> bool {
        self.iter1_0_1_2(x0).next().is_some()
    }
    fn check1_1_0_2(&self, x1: Math) -> bool {
        self.iter1_1_0_2(x1).next().is_some()
    }
    fn check1_2_0_1(&self, x2: Math) -> bool {
        self.iter1_2_0_1(x2).next().is_some()
    }
    fn update(
        &mut self,
        math_uprooted: &[Math],
        math_uf: &mut UnionFind<Math>,
        delta: &mut Vec<<Self as Relation>::Row>,
    ) {
        self.new.clear();
        let mut op_insert = take(delta);
        for (x0, x1, x2) in op_insert.iter_mut() {
            *x0 = math_uf.find(*x0);
            *x1 = math_uf.find(*x1);
            *x2 = math_uf.find(*x2);
        }
        let mut op_delete = Vec::new();
        for x0 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x0);
            for (x1, x2) in self.iter1_0_1_2(x0) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x1 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x1);
            for (x0, x2) in self.iter1_1_0_2(x1) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x2 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x2);
            for (x0, x1) in self.iter1_2_0_1(x2) {
                op_delete.push((x0, x1, x2));
            }
        }
        for (x0, x1, x2) in op_delete {
            if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                self.all_index_1_0_2.remove(&(x1, x0, x2));
                self.all_index_2_0_1.remove(&(x2, x0, x1));
                math_uf.dec_eclass(x0, Self::COST);
                math_uf.dec_eclass(x1, Self::COST);
                math_uf.dec_eclass(x2, Self::COST);
                println!("delete: {:?}", [x0, x1, x2]);
                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
            }
        }
        op_insert.retain(|&(x0, x1, x2)| {
            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                return false;
            }
            println!("insert: {:?}", [x0, x1, x2]);
            math_uf.inc_eclass(x0, Self::COST);
            math_uf.inc_eclass(x1, Self::COST);
            math_uf.inc_eclass(x2, Self::COST);
            self.all_index_1_0_2.insert((x1, x0, x2));
            self.all_index_2_0_1.insert((x2, x0, x1));
            true
        });
        self.new = op_insert;
    }
}
#[derive(Debug, Default)]
struct AddRelation {
    new: Vec<<Self as Relation>::Row>,
    all_index_0_1_2: BTreeSet<(Math, Math, Math)>,
    all_index_1_0_2: BTreeSet<(Math, Math, Math)>,
    all_index_2_0_1: BTreeSet<(Math, Math, Math)>,
}
impl Relation for AddRelation {
    type Row = (Math, Math, Math);
}
impl AddRelation {
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn iter_new(&self) -> impl Iterator<Item = <Self as Relation>::Row> + use<'_> {
        self.new.iter().copied()
    }
    fn iter1_2_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_2_0_1
            .range((x2, Math(u32::MIN), Math(u32::MIN))..=(x2, Math(u32::MAX), Math(u32::MAX)))
            .copied()
            .map(|(x2, x0, x1)| (x0, x1))
    }
    // for things that are supposed to return exactly one thing, perform an "get or default"
    // operation.
    // TODO: range query trait
    fn entry2_0_1_2(
        &self,
        delta: &mut Delta,
        math_uf: &mut UnionFind<Math>,
        x0: Math,
        x1: Math,
    ) -> (Math) {
        if let Some(x2) = self.all_index_0_1_2.query((x0, x1)).next() {
            return (x2);
        }
        let x2 = math_uf.add_eclass();
        delta.forall_math_relation_delta.push(x2);
        delta.add_relation_delta.push((x0, x1, x2));
        return (x2);
    }
    fn iter1_0_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_0_1_2
            .range((x0, Math(u32::MIN), Math(u32::MIN))..=(x0, Math(u32::MAX), Math(u32::MAX)))
            .copied()
            .map(|(x0, x1, x2)| (x1, x2))
    }
    fn iter1_1_0_2(&self, x1: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.all_index_1_0_2
            .range((x1, Math(u32::MIN), Math(u32::MIN))..=(x1, Math(u32::MAX), Math(u32::MAX)))
            .copied()
            .map(|(x1, x0, x2)| (x0, x2))
    }
    fn check1_2_0_1(&self, x2: Math) -> bool {
        self.iter1_2_0_1(x2).next().is_some()
    }
    fn check1_0_1_2(&self, x0: Math) -> bool {
        self.iter1_0_1_2(x0).next().is_some()
    }
    fn check1_1_0_2(&self, x1: Math) -> bool {
        self.iter1_1_0_2(x1).next().is_some()
    }
    fn update(
        &mut self,
        math_uprooted: &[Math],
        math_uf: &mut UnionFind<Math>,
        delta: &mut Vec<<Self as Relation>::Row>,
    ) {
        self.new.clear();
        let mut op_insert = take(delta);
        for (x0, x1, x2) in op_insert.iter_mut() {
            *x0 = math_uf.find(*x0);
            *x1 = math_uf.find(*x1);
            *x2 = math_uf.find(*x2);
        }
        let mut op_delete = Vec::new();
        for x0 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x0);
            for (x1, x2) in self.iter1_0_1_2(x0) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x1 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x1);
            for (x0, x2) in self.iter1_1_0_2(x1) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x2 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x2);
            for (x0, x1) in self.iter1_2_0_1(x2) {
                op_delete.push((x0, x1, x2));
            }
        }
        for (x0, x1, x2) in op_delete {
            if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                self.all_index_1_0_2.remove(&(x1, x0, x2));
                self.all_index_2_0_1.remove(&(x2, x0, x1));
                math_uf.dec_eclass(x0, Self::COST);
                math_uf.dec_eclass(x1, Self::COST);
                math_uf.dec_eclass(x2, Self::COST);
                println!("delete: {:?}", [x0, x1, x2]);
                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
            }
        }
        op_insert.retain(|&(x0, x1, x2)| {
            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                return false;
            }
            println!("insert: {:?}", [x0, x1, x2]);
            math_uf.inc_eclass(x0, Self::COST);
            math_uf.inc_eclass(x1, Self::COST);
            math_uf.inc_eclass(x2, Self::COST);
            self.all_index_1_0_2.insert((x1, x0, x2));
            self.all_index_2_0_1.insert((x2, x0, x1));
            true
        });
        self.new = op_insert;
    }
    // relation.clear_new();
    // fixpoint {
    //      let uprooted = uf.dirty()
    //      relation.update_safe(uprooted, &mut uf)
    // }
    // relation.update_finalize();

    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn update_safe(
        &mut self,
        math_uprooted: &[Math],
        math_uf: &mut UnionFind<Math>,
        delta: &mut Vec<<Self as Relation>::Row>,
    ) {
        let mut op_insert = take(delta);
        for (x0, x1, x2) in op_insert.iter_mut() {
            *x0 = math_uf.find(*x0);
            *x1 = math_uf.find(*x1);
            *x2 = math_uf.find(*x2);
        }
        let mut op_delete = Vec::new();
        for x0 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x0);
            for (x1, x2) in self.iter1_0_1_2(x0) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x1 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x1);
            for (x0, x2) in self.iter1_1_0_2(x1) {
                op_delete.push((x0, x1, x2));
            }
        }
        for x2 in math_uprooted.iter().copied() {
            println!("uproot: {:?}", x2);
            for (x0, x1) in self.iter1_2_0_1(x2) {
                op_delete.push((x0, x1, x2));
            }
        }
        for (x0, x1, x2) in op_delete {
            if self.all_index_0_1_2.remove(&(x0, x1, x2)) {
                self.all_index_1_0_2.remove(&(x1, x0, x2));
                self.all_index_2_0_1.remove(&(x2, x0, x1));
                math_uf.dec_eclass(x0, Self::COST);
                math_uf.dec_eclass(x1, Self::COST);
                math_uf.dec_eclass(x2, Self::COST);
                println!("delete: {:?}", [x0, x1, x2]);
                op_insert.push((math_uf.find(x0), math_uf.find(x1), math_uf.find(x2)));
            }
        }
        op_insert.retain(|&(x0, x1, x2)| {
            if !self.all_index_0_1_2.insert((x0, x1, x2)) {
                return false;
            }
            println!("insert: {:?}", [x0, x1, x2]);
            math_uf.inc_eclass(x0, Self::COST);
            math_uf.inc_eclass(x1, Self::COST);
            math_uf.inc_eclass(x2, Self::COST);
            self.all_index_1_0_2.insert((x1, x0, x2));
            self.all_index_2_0_1.insert((x2, x0, x1));
            true
        });
        self.new.extend(op_insert);
    }

    fn update_finalize(&mut self, math_uf: &mut UnionFind<Math>) {
        for (x0, x1, x2) in self.new.iter_mut() {
            *x0 = math_uf.find(*x0);
            *x1 = math_uf.find(*x1);
            *x2 = math_uf.find(*x2);
        }
        self.new.sort();
        self.new.dedup();
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug)]
pub struct MathSet(u32);
impl Eclass for MathSet {
    fn new(value: u32) -> Self {
        Self(value)
    }
    fn inner(self) -> u32 {
        self.0
    }
}
impl RelationElement for MathSet {
    const MIN_ID: Self = Self(0);
    const MAX_ID: Self = Self(u32::MAX);
}

// somewhere:
// mathset_uf: UnionFind<MathSetHandle>,

#[derive(Debug, Default)]
struct MathSetContext {
    set_to_handle: BTreeMap<Vec<Math>, MathSet>,
    handle_to_set: BTreeMap<MathSet, Vec<Math>>,

    math_to_handles: BTreeMap<Math, BTreeSet<MathSet>>,

    // this is actually just a relation...
    set_insert: BTreeMap<(MathSet, Math), MathSet>,
}
impl MathSetContext {
    fn update(
        &mut self,
        sets_uprooted: &[MathSet],
        math_uprooted: &[Math],
        sets_uf: &mut UnionFind<MathSet>,
    ) {
        let mut op_delete: Vec<(Vec<Math>, MathSet)> = Vec::new();

        for handle in sets_uprooted {
            let set = &self.handle_to_set[handle];
            op_delete.push((set.clone(), *handle));
        }
        for elem in math_uprooted {
            for handle in &self.math_to_handles[elem] {
                let set = &self.handle_to_set[handle];
                op_delete.push((set.clone(), *handle));
            }
        }
        let mut op_insert_mathvec: Vec<(Vec<Math>, MathSet)> = Vec::new();
        let mut op_insert_insert: Vec<(Vec<Math>, MathSet)> = Vec::new();
        for (set, handle) in op_delete {
            // set_to_handle
            if self.set_to_handle.remove(&set).is_none() {
                continue;
            }
            // handle_to_set
            self.handle_to_set.remove(&handle);
            // math_to_handles
            for x in set.iter() {
                self.math_to_handles.remove(x);
            }
            // set_insert
            // the op_insert later will fix what set_insert points to.

            // self.set_insert.remove(
        }
    }
}
use std::collections::BTreeMap;

// to be part of delta
struct MathSetDelta {}

#[derive(Debug, Default)]
struct MathSetInsert;
impl Relation for MathSetInsert {
    type Row = (MathSet, Math, MathSet);
}
impl MathSetInsert {
    fn new() -> Self {
        Self::default()
    }
    // mutation here is fine since it is not observable?
    // iterator invalidation due to mutation impossible because of use<>.
    fn iter2_0_1_2(
        &self,
        ctx: &mut MathSetContext,
        math_set_uf: &mut UnionFind<MathSet>,
        handle: MathSet,
        elem: Math,
    ) -> impl Iterator<Item = (MathSet)> + use<> {
        let set = &ctx.handle_to_set[&handle];
        if set.contains(&elem) {
            std::iter::once(handle)
        } else {
            let new_handle = *ctx.set_insert.entry((handle, elem)).or_insert_with(|| {
                let mut set = ctx.handle_to_set[&handle].clone();
                set.push(elem);
                set.sort();

                *ctx.set_to_handle.entry(set.clone()).or_insert_with(|| {
                    let handle = math_set_uf.add_eclass();
                    for x in set.iter().copied() {
                        ctx.math_to_handles.entry(x).or_default().insert(handle);
                    }
                    ctx.handle_to_set.insert(handle, set);
                    handle
                })
            });
            std::iter::once(new_handle)
        }
    }
}

#[derive(Debug, Default)]
pub struct Delta {
    forall_math_relation_delta: Vec<<ForallMathRelation as Relation>::Row>,
    mul_relation_delta: Vec<<MulRelation as Relation>::Row>,
    add_relation_delta: Vec<<AddRelation as Relation>::Row>,
}
impl Delta {
    fn new() -> Self {
        Self::default()
    }
    pub fn make_math(&mut self, math_uf: &mut UnionFind<Math>) -> Math {
        let id = math_uf.add_eclass();
        self.forall_math_relation_delta.push(id);
        id
    }
    pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
        self.mul_relation_delta.push(x);
    }
    pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
        self.add_relation_delta.push(x);
    }
}
#[derive(Debug, Default)]
pub struct Theory {
    delta: Delta,
    i64_uf: UnionFind<I64>,
    f64_uf: UnionFind<F64>,
    string_uf: UnionFind<String>,
    bool_uf: UnionFind<Bool>,
    unit_uf: UnionFind<Unit>,
    math_uf: UnionFind<Math>,
    forall_math_relation: ForallMathRelation,
    mul_relation: MulRelation,
    add_relation: AddRelation,
}
impl Theory {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn step(&mut self) {
        println!("step start");
        self.apply_rules();
        self.clear_transient();
        println!("step end");
    }
    fn apply_rules(&mut self) {
        for (a, b, p2) in self.add_relation.iter_new() {
            for (c, p4) in self.mul_relation.iter1_0_1_2(p2) {
                let a3 = self.delta.make_math(&mut self.math_uf);
                let a4 = self.delta.make_math(&mut self.math_uf);
                self.delta.insert_mul((a, c, a3));
                self.delta.insert_mul((b, c, a4));
                self.delta.insert_add((a3, a4, p4));
            }
        }
        for (p2, c, p4) in self.mul_relation.iter_new() {
            for (a, b) in self.add_relation.iter1_2_0_1(p2) {
                let a3 = self.delta.make_math(&mut self.math_uf);
                let a4 = self.delta.make_math(&mut self.math_uf);
                self.delta.insert_mul((a, c, a3));
                self.delta.insert_mul((b, c, a4));
                self.delta.insert_add((a3, a4, p4));
            }
        }
    }
    fn clear_transient(&mut self) {
        let i64_uprooted = take(self.i64_uf.dirty());
        let f64_uprooted = take(self.f64_uf.dirty());
        let string_uprooted = take(self.string_uf.dirty());
        let bool_uprooted = take(self.bool_uf.dirty());
        let unit_uprooted = take(self.unit_uf.dirty());
        let math_uprooted = take(self.math_uf.dirty());
        let _ = "todo: update forall";
        self.mul_relation.update(
            &math_uprooted,
            &mut self.math_uf,
            &mut self.delta.mul_relation_delta,
        );
        self.add_relation.update(
            &math_uprooted,
            &mut self.math_uf,
            &mut self.delta.add_relation_delta,
        );
    }
}
impl std::ops::Deref for Theory {
    type Target = Delta;
    fn deref(&self) -> &Self::Target {
        &self.delta
    }
}
impl std::ops::DerefMut for Theory {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.delta
    }
}
