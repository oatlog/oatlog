use crate::runtime::{
    Clear as _, Eclass, EclassProvider as _, RangeQuery as _, Relation, RelationElement, UnionFind,
};
use std::{collections::BTreeSet, mem::take};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug, Hash)]
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

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Default, Debug, Hash)]
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
            for x in &set {
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
        Self
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
