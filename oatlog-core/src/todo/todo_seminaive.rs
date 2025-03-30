//! Figure out how to run some rules more often.
//!

use crate::ids::*;
use crate::runtime::UnionFind;
use crate::typed_vec::*;
use crate::{
    eclass_wrapper_ty, relation_element_wrapper_ty,
    runtime::{Eclass, RelationElement},
};
id_wrap!(PhiRuleId, "p", "phi rule id");

// Phirule is a set of symbolic rules that can share their (semi-naive) state (TimeStamp)
// (Phi is an arbitrary name to avoid overloading ruleset)
// the set of all PhiRules do not have any overlapping rules.
//
// ruleset1:    [x, _, _, _, x, x, x, x, x, x]
// ruleset2:    [x, x, x, x, x, x, _, _, _, _]
// phi1         [x, _, _, _, x, x, _, _, _, _]
// phi2         [_, _, _, _, _, _, x, x, x, x]
// phi3         [_, x, x, x, _, _, _, _, _, _]

trait HasTimeStamp {
    fn timestamp(&self) -> TimeStamp;
}

// Timestamp points to the next new to process.
//
//       timestamp
//           v
// [0, 1, 2, 3, 4, 5, 6]
//  ^     ^  ^        ^
//  |-----|  |--------|
//    old        new
//
#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
struct TimeStamp(u32);

#[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Default)]
struct PhiRuleState {
    add_: Option<TimeStamp>,
    mul_: Option<TimeStamp>,
    const_: Option<TimeStamp>,
    // ...

    // to be used to enable GC even if a `check` exists somewhere.
    // may_run: bool
}
impl PhiRuleState {
    // NOTE: need separate apply per phirule.
    fn apply(&mut self /* ... */) {
        for () in [()] {
            // ...
        }
    }
    fn min(x: impl Iterator<Item = Self>) -> Self {
        fn min(a: Option<TimeStamp>, b: Option<TimeStamp>) -> Option<TimeStamp> {
            match (a, b) {
                (None, None) => None,
                (None, Some(x)) | (Some(x), None) => Some(x),
                (Some(a), Some(b)) => Some(a.min(b)),
            }
        }
        x.reduce(|a, b| Self {
            add_: min(a.add_, b.add_),
            mul_: min(a.mul_, b.mul_),
            const_: min(a.const_, b.const_),
        })
        .unwrap_or(Default::default())
    }
    fn bump_to(self, set_to: TimeStamp) -> Self {
        let Self { add_, mul_, const_ } = self;
        Self {
            add_: add_.map(|_| set_to),
            mul_: mul_.map(|_| set_to),
            const_: const_.map(|_| set_to),
        }
    }
}

struct Theory {
    rule_states: Vec<PhiRuleState>,
    relations: Relations,
    uf: Unification,
    delta: Delta,
    // when calling canonicalize, rows inserted into the database will be assigned this timestamp,
    // update timestamp in canonicalize.
    next_timestamp: TimeStamp,
}
impl Theory {
    // NOTE: it is now fine to call canonicalize however much you want.
    pub fn canonicalize(&mut self) {
        // apply delta to DB
        // remove outdated e-classes

        while self.uf.has_new_uproots() || self.delta.has_new_inserts() {
            self.relations.update(&mut self.uf, &mut self.delta);
        }

        // NOTE: no "update_finalize" because it's only purpose was to clean new, but we do that
        // when rules are applied instead (strip_stale)

        self.next_timestamp = TimeStamp(
            self.next_timestamp
                .0
                .checked_add(1)
                .expect("Ran out of timestmp ids"),
        );
    }
    pub fn apply_ruleset0(&mut self) {
        // NOTE: PhiRule 3 is shared between these
        self.apply_phirules(&[0, 3]);
    }
    pub fn apply_ruleset1(&mut self) {
        // NOTE: PhiRule 3 is shared between these
        self.apply_phirules(&[3, 7]);
    }
    // canonicalize probably needs to be run before this.
    fn apply_phirules(&mut self, phi_rules: &[usize]) {
        self.strip_stale(phi_rules);
        self.run_rules(phi_rules);
    }
    // remove outdated rows from the new that will be read.
    fn strip_stale(&mut self, phi_rules: &[usize]) {
        // the oldest timestams that will be read.
        let lower_bound = PhiRuleState::min(phi_rules.iter().copied().map(|x| self.rule_states[x]));
        if let Some(starting_at) = lower_bound.add_ {
            self.relations.add_.strip_stale(&mut self.uf, starting_at);
        }
        if let Some(starting_at) = lower_bound.mul_ {
            self.relations.mul_.strip_stale(&mut self.uf, starting_at);
        }
    }
    // pre: strip stale is done.
    fn run_rules(&mut self, phi_rules: &[usize]) {
        for &i in phi_rules {
            let rule_state = &mut self.rule_states[i];
            match i {
                0 => {
                    for AddRow { .. } in self.relations.add_.iter_new(rule_state.add_.unwrap()) {
                        /* ... */
                    }
                }
                1 => {}
                2 => {}
                _ => panic!(),
            }
            *rule_state = rule_state.bump_to(self.next_timestamp);
        }
    }
    // if needed we can remove outdated rows from new.
    fn gc_old_timestams(&mut self) {
        let lowest_valid = PhiRuleState::min(self.rule_states.iter().copied());
        if let Some(starting_at) = lowest_valid.add_ {
            self.relations.add_.gc(starting_at);
        }
    }
}

struct Relations {
    add_: AddRelation,
    mul_: MulRelation,
}
impl Relations {
    fn update(&mut self, uf: &mut Unification, delta: &mut Delta) {
        self.add_.update(uf, delta);
        // self.mul_.update(uf, delta);
    }
}

struct Delta {
    add_: Vec<AddRow>,
    // mul: ..
}
impl Delta {
    fn has_new_inserts(&self) -> bool {
        !self.add_.is_empty()
    }
}

// just to make it easier to read in scratch.
#[derive(Copy, Clone)]
struct AddRow {
    a: Math,
    b: Math,
    res: Math,
    timestamp: TimeStamp,
}

impl HasTimeStamp for AddRow {
    fn timestamp(&self) -> TimeStamp {
        self.timestamp
    }
}

struct AddRelation {
    // old -> new
    new: Vec<AddRow>,
}
impl AddRelation {
    // remove rows from new containing outdated (non-root) e-classes starting at timestamp.
    fn strip_stale(&mut self, uf: /* only find */ &mut Unification, starting_at: TimeStamp) {
        let start_idx = self.new.partition_point(|x| x.timestamp() < starting_at);
        retain_suffix(&mut self.new, start_idx, |x| {
            uf.math_.is_root(x.a) && uf.math_.is_root(x.b) && uf.math_.is_root(x.res)
        })
    }
    fn iter_new(&self, starting_at: TimeStamp) -> impl Iterator<Item = AddRow> {
        let start_idx = self.new.partition_point(|x| x.timestamp() < starting_at);
        self.new[start_idx..].iter().copied()
    }
    // if needed we can remove outdated rows.
    fn gc(&mut self, lowest_valid: TimeStamp) {
        self.new.retain(|x| x.timestamp() >= lowest_valid);
    }
    fn update(&mut self, uf: &mut Unification, delta: &mut Delta) {
        todo!()
    }
}

/// `vec[start_idx..].retain(f)`
fn retain_suffix<T: Copy>(vec: &mut Vec<T>, start_idx: usize, mut f: impl FnMut(T) -> bool) {
    let mut write_head = start_idx;
    for read_head in start_idx..vec.len() {
        if f(vec[read_head]) {
            vec[write_head] = vec[read_head];
            write_head += 1;
        }
    }
    vec.truncate(write_head);
}

eclass_wrapper_ty!(Math);

struct Unification {
    math_: UnionFind<Math>,
}
impl Unification {
    fn has_new_uproots(&self) -> bool {
        self.math_.has_new_uproots()
    }
    fn snapshot_all_uprooted(&mut self) {
        self.math_.create_uprooted_snapshot();
    }
}

struct MulRelation {}
impl MulRelation {
    fn strip_stale(&mut self, uf: /* only find */ &mut Unification, starting_at: TimeStamp) {
        todo!()
    }
}
