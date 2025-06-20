//! Figure out how to run some rules more often.
//!

use crate::ids::*;
use crate::runtime::UnionFind;
use crate::runtime::*;
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

// Timestamp points to the next new to process.
//
//       timestamp
//           v
// [0, 1, 2, 3, 4, 5, 6]
//  ^     ^  ^        ^
//  |-----|  |--------|
//    old        new
//
// #[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
// struct TimeStamp(u32);

relation_element_wrapper_ty!(TimeStamp);

mod v0 {
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
        fn merge_min(a: PhiRuleState, b: PhiRuleState) -> PhiRuleState {
            fn merge(a: Option<TimeStamp>, b: Option<TimeStamp>) -> Option<TimeStamp> {
                match (a, b) {
                    (None, None) => None,
                    (None, Some(x)) | (Some(x), None) => Some(x),
                    (Some(a), Some(b)) => Some(a.min(b)),
                }
            }
            Self {
                add_: merge(a.add_, b.add_),
                mul_: merge(a.mul_, b.mul_),
                const_: merge(a.const_, b.const_),
            }
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
    use super::*;
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
            let lower_bound = phi_rules
                .iter()
                .copied()
                .map(|x| self.rule_states[x])
                .reduce(PhiRuleState::merge_min)
                .unwrap();

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
                        for AddRow { .. } in self.relations.add_.iter_new(rule_state.add_.unwrap())
                        {
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
            let lowest_valid = self
                .rule_states
                .iter()
                .copied()
                .reduce(PhiRuleState::merge_min)
                .unwrap();
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
        lower_bound_: Vec<(Math, i64)>,
        upper_bound_: Vec<(Math, i64, TimeStamp)>,
        // mul: ..
    }
    impl Delta {
        fn has_new_inserts(&self) -> bool {
            !self.add_.is_empty()
        }
    }
    // just to make the code easier to read in scratch
    #[derive(Copy, Clone)]
    struct MulRow {
        a: Math,
        b: Math,
        res: Math,
        timestamp: TimeStamp,
    }

    impl HasTimeStamp for MulRow {
        fn timestamp(&self) -> TimeStamp {
            self.timestamp
        }
    }

    // just to make the code easier to read in scratch
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

    // mod scratch {
    //     use super::*;
    //
    //     // just to check if we accidentally had type errors here.
    //     eclass_wrapper_ty!(MathA);
    //     eclass_wrapper_ty!(MathB);
    //     eclass_wrapper_ty!(MathRes);
    //     eclass_wrapper_ty!(Math);
    //
    //     // timestamp is always part of the "value" part and merge just takes the largest (or smallest?) timestamp.
    //     // => timestamp is a weird lattice variable :(
    //
    //     // decl_row!(Row4_0_1<T0 first 0, T1, T2, T3> (T0 0, T1 1) (T2 2, T3 3));
    //
    //     decl_row!(Row3_0_1<T0 first 0, T1, T2> (T0 0, T1 1) (T2 2));
    //     decl_row!(Row3_1_0_2<T0, T1 first 1, T2> (T1 1, T0 0, T2 2) ());
    //     decl_row!(Row3_2_0_1<T0, T1, T2 first 2> (T2 2, T0 0, T1 1) ());
    //
    //     struct AddRelation {
    //         new: Vec<<Self as Relation>::Row>,
    //         all_index_0_1_2: IndexImpl<StdSortCtx<Row3_0_1<Math, Math, Math>>>,
    //         all_index_1_0_2: IndexImpl<StdSortCtx<Row3_1_0_2<Math, Math, Math>>>,
    //         all_index_2_0_1: IndexImpl<StdSortCtx<Row3_2_0_1<Math, Math, Math>>>,
    //     }
    //     impl Relation for AddRelation {
    //         type Row = (Math, Math, Math);
    //     }
    // }
    decl_row!(Row2_0<T0 first 0, T1> (T0 0) (T1 1));

    struct LowerBoundRelation {
        new: Vec<(Math, i64)>,
        all_index_0_1: IndexImpl<StdSortCtx<Row2_0<Math, i64>>>,
    }
    impl LowerBoundRelation {
        fn update(&mut self, uf: &mut Unification, delta: &mut Delta) {
            let mut inserts: Vec<(Math, i64)> = take(&mut delta.lower_bound_);
            let orig_inserts = inserts.len();
            self.all_index_0_1.first_column_uproots(
                uf.math_.get_uprooted_snapshot(),
                |deleted_rows| {
                    inserts.extend(deleted_rows);
                },
            );

            inserts[orig_inserts..].sort_unstable();
            dedup_suffix(&mut inserts, orig_inserts); // <- just a perf thing.

            // pretend we have more than 1 index
            self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);

            inserts.iter_mut().for_each(|row| {
                row.0 = uf.math_.find(row.0);
            });

            self.all_index_0_1
                .insert_many(&mut inserts, |mut old, mut new| {
                    let (x1,) = old.value_mut();
                    let (y1,) = new.value_mut();
                    *x1 = i64::max(*x1, *y1);
                    old
                });

            self.new.extend_from_slice(&inserts);
        }
        fn update_finalize(&mut self, uf: &mut Unification) {
            self.new.sort_unstable();
            self.new.dedup();
            self.new.retain(|(x0, x1)| {
                if !uf.math_.is_root(*x0) {
                    return false;
                }
                true
            })
        }
    }

    decl_row!(Row3_0<T0 first 0, T1, T2> (T0 0) (T1 1, T2 2));

    // NOTE: it is actually fine for all indexes to have a lattice variable if we do this:
    // (a, b) -> (lattice)
    // (b, a) -> (lattice)
    //
    //
    //
    // (a, b) -> (timestamp)
    // (b, a) -> (timestamp)
    //
    // only requirement is that the lattice variable is in the "key" part of the index.

    struct UpperBoundRelation {
        new: Vec<(Math, i64, TimeStamp)>,
        all_index_0_1: IndexImpl<StdSortCtx<Row3_0<Math, i64, TimeStamp>>>,
    }
    impl UpperBoundRelation {
        fn update(&mut self, uf: &mut Unification, delta: &mut Delta, next_timestamp: TimeStamp) {
            //
            //                  orig_inserts
            //                       v
            // inserts: [_, _, _, _, _, _, _, _, _, _]
            //           ^        ^  ^              ^
            //           |--------|  |--------------|
            //           from delta     uprooted
            //               ^
            //               |
            // TODO: If this has rows already in ALL, then they still get passed to new.
            // Inserts of things that are already in the database should be a no-op.
            //

            let mut inserts: Vec<(Math, i64, TimeStamp)> = take(&mut delta.upper_bound_);
            let orig_inserts = inserts.len();
            self.all_index_0_1.first_column_uproots(
                uf.math_.get_uprooted_snapshot(),
                |deleted_rows| {
                    inserts.extend(deleted_rows);
                },
            );

            inserts[orig_inserts..].sort_unstable();
            dedup_suffix(&mut inserts, orig_inserts); // <- just a perf thing.

            // (pretend we have more than 1 index so this makes sense)
            self.all_index_0_1.delete_many(&mut inserts[orig_inserts..]);

            inserts.iter_mut().for_each(|row| {
                row.0 = uf.math_.find(row.0);
                row.2 = next_timestamp;
            });

            self.all_index_0_1
                .insert_many(&mut inserts, |mut old, mut new| {
                    let (x1, x2) = old.value_mut();
                    let (y1, y2) = new.value_mut();
                    let mut mutation_occured = false;
                    if *x1 != *y1 {
                        let z1 = i64::max(*x1, *y1);
                        // TODO: handle timestamp, should this go to new?
                        // it's fine for max, but for set-union, it should probably go to new.
                        if z1 != *x1 && z1 != *y1 {
                            mutation_occured = true;
                        }
                        let x1 = *x1;
                    }

                    if mutation_occured {
                        // we have semantically mutated the database, so update timestamp and push to
                        // new.
                        *x2 = next_timestamp;
                        self.new.push(old.inner());
                    } else {
                        // lower timestamps are always safe, and taking min forms a lattice.
                        *x2 = TimeStamp::min(*x2, *y2);
                    }
                    old
                });

            self.new.extend_from_slice(&inserts);
        }
        fn update_finalize(&mut self, uf: &mut Unification, starting_at: TimeStamp) {
            // sort + dedup of everything from latest timestamp.
            let start_idx = self.new.partition_point(|x| x.2 < starting_at);

            // sort WRT one of the indexes, then filter WRT that index.

            type Idx = IndexImpl<StdSortCtx<Row3_0<Math, i64, TimeStamp>>>;
            <Idx as Index>::RowCtx::sort(<Idx as Index>::Row::from_inner_slice_mut(
                &mut self.new[start_idx..],
            ));
            dedup_suffix(&mut self.new, start_idx);
            retain_suffix(&mut self.new, start_idx, |x| {
                // if the value (including WRT lattice, timestamp) is not in all, then it was invalidated
                if self.all_index_0_1.range(x..=x).next().is_none() {
                    return false;
                }
                true
            });

            // fn filter_existing(&self, potential_inserts: &mut Vec<<Self::Row as IndexRow>::Repr>) {
            //     Self::RowCtx::sort(Self::Row::from_inner_slice_mut(potential_inserts));
            //     potential_inserts.retain(|&x| self.range(x..=x).next().is_none());
            // }
        }
    }
}

mod v1 {
    #[derive(Copy, Clone, PartialOrd, Ord, PartialEq, Eq, Default)]
    struct PhiRuleState {
        add: Option<TimeStamp>,
        // mul_: Option<TimeStamp>,
        // const_: Option<TimeStamp>,
        // ...

        // to be used to enable GC even if a `check` exists somewhere.
        // may_run: bool
    }
    impl PhiRuleState {
        fn merge_min(a: PhiRuleState, b: PhiRuleState) -> PhiRuleState {
            fn merge(a: Option<TimeStamp>, b: Option<TimeStamp>) -> Option<TimeStamp> {
                match (a, b) {
                    (None, None) => None,
                    (None, Some(x)) | (Some(x), None) => Some(x),
                    (Some(a), Some(b)) => Some(a.min(b)),
                }
            }
            Self {
                add: merge(a.add, b.add),
            }
        }
        fn bump_to(self, set_to: TimeStamp) -> Self {
            let Self { add } = self;
            Self {
                add: add.map(|_| set_to),
            }
        }
    }
    // for MVP, we only need to put timestamps in new.

    use super::*;

    eclass_wrapper_ty!(Math);

    decl_row!(Row3_0_1  <T0 first 0, T1, T2> (T0 0, T1 1) (T2 2));
    decl_row!(Row3_0_2_1<T0 first 0, T1, T2> (T0 0, T2 2, T1 1) ());
    decl_row!(Row3_1_0_2<T0, T1 first 1, T2> (T1 1, T0 0, T2 2) ());
    decl_row!(Row3_2_0_1<T0, T1, T2 first 2> (T2 2, T0 0, T1 1) ());

    struct Theory {
        rule_states: Vec<PhiRuleState>,
        relations: Relations,
        uf: Unification,
        delta: Delta,
        current_timestamp: TimeStamp,
    }
    struct Relations {
        add: AddRelation,
    }
    struct Unification {
        math: UnionFind<Math>,
    }
    struct Delta {
        add: Vec<(Math, Math, Math)>,
    }
    struct AddRelation {
        // putting it in a tuple is mostly fine for alignment because basically everything is a u32
        // anyways, and it makes the code significantly simpler.
        //
        // putting timestamp first makes it safe to sort an arbitrary slice of new.
        new: Vec<(TimeStamp, (Math, Math, Math))>,
        all_index_0_1_2: IndexImpl<StdSortCtx<Row3_0_1<Math, Math, Math>>>,
        all_index_0_2_1: IndexImpl<StdSortCtx<Row3_0_2_1<Math, Math, Math>>>,
        all_index_1_0_2: IndexImpl<StdSortCtx<Row3_1_0_2<Math, Math, Math>>>,
        all_index_2_0_1: IndexImpl<StdSortCtx<Row3_2_0_1<Math, Math, Math>>>,
    }

    impl Theory {
        fn canonicalize(&mut self) {
            // timestamp is updated before new rows are introduced
            self.current_timestamp = TimeStamp(
                self.current_timestamp
                    .0
                    .checked_add(1)
                    .expect("ran out of timestamp ids"),
            );

            while self.uf.has_new_uproots() || self.delta.has_new_inserts() {
                self.relations
                    .update(&mut self.uf, &mut self.delta, self.current_timestamp);
            }
        }
        pub fn apply_ruleset0(&mut self) {
            self.apply_phirules(&[0, 3]);
        }
        pub fn apply_ruleset1(&mut self) {
            self.apply_phirules(&[3, 7]);
        }
        fn apply_phirules(&mut self, phi_rules: &[usize]) {
            self.clean_new(phi_rules);
            self.run_rules(phi_rules);
        }
        fn clean_new(&mut self, phi_rules: &[usize]) {
            let lower_bound = phi_rules
                .iter()
                .copied()
                .map(|x| self.rule_states[x])
                .reduce(PhiRuleState::merge_min)
                .unwrap();

            if let Some(starting_at) = lower_bound.add {
                self.relations.add.clean_new(&mut self.uf, starting_at);
            }
        }
        fn run_rules(&mut self, phi_rules: &[usize]) {
            for &i in phi_rules {
                let rule_state = &mut self.rule_states[i];
                match i {
                    0 => {
                        for (a, b, res) in self.relations.add.iter_new(self.current_timestamp) {
                            self.delta.add.push((b, a, res));
                        }
                    }
                    _ => panic!(),
                }
            }
        }
    }
    impl Relations {
        fn update(&mut self, uf: &mut Unification, delta: &mut Delta, next_timestamp: TimeStamp) {
            self.add.update(uf, delta, next_timestamp);
        }
    }
    impl Unification {
        fn has_new_uproots(&self) -> bool {
            self.math.has_new_uproots()
        }
    }
    impl Delta {
        fn has_new_inserts(&self) -> bool {
            !self.add.is_empty()
        }
    }

    impl AddRelation {
        fn update(&mut self, uf: &mut Unification, delta: &mut Delta, next_timestamp: TimeStamp) {
            //                  orig_inserts
            //                       v
            // inserts: [_, _, _, _, _, _, _, _, _, _]
            //           ^        ^  ^              ^
            //           |--------|  |--------------|
            //           from delta     uprooted
            let mut inserts = take(&mut delta.add);
            let orig_inserts = inserts.len();
            self.all_index_0_1_2
                .first_column_uproots(uf.math.get_uprooted_snapshot(), |deleted_rows| {
                    inserts.extend(deleted_rows)
                });
            self.all_index_1_0_2
                .first_column_uproots(uf.math.get_uprooted_snapshot(), |deleted_rows| {
                    inserts.extend(deleted_rows)
                });
            self.all_index_2_0_1
                .first_column_uproots(uf.math.get_uprooted_snapshot(), |deleted_rows| {
                    inserts.extend(deleted_rows)
                });

            inserts[orig_inserts..].sort_unstable();
            dedup_suffix(&mut inserts, orig_inserts);

            self.all_index_0_1_2
                .delete_many(&mut inserts[orig_inserts..]);
            self.all_index_0_2_1
                .delete_many(&mut inserts[orig_inserts..]);
            self.all_index_1_0_2
                .delete_many(&mut inserts[orig_inserts..]);
            self.all_index_2_0_1
                .delete_many(&mut inserts[orig_inserts..]);

            inserts.iter_mut().for_each(|row| {
                row.0 = uf.math.find(row.0);
                row.1 = uf.math.find(row.1);
                row.2 = uf.math.find(row.2);
            });

            {
                //
                //          |-----------------------|
                //          |                       |
                //          |                       |
                // EXISTING |                       |
                //    ALL   |     |-----------------------|
                //          |     |                 |     |
                //          |     | Exclude in new  |     |
                //          |     |                 |     |
                //          |-----|-----------------|     | INSERTS
                //                |                       |
                //                |     Include in new    |
                //                |                       |
                //                |-----------------------|
                //
                // We want to filter what goes into new, otherwise new will contain rows that where
                // already in new in a previous iteration.
                //

                self.all_index_0_1_2.filter_existing(&mut inserts);
            }

            self.all_index_0_1_2
                .insert_many(&mut inserts, |mut old, mut new| {
                    let (x2,) = old.value_mut();
                    let (y2,) = new.value_mut();
                    uf.math.union_mut(x2, y2);
                    old
                });
            self.all_index_0_2_1
                .insert_many(&mut inserts, |_, _| panic!());
            self.all_index_1_0_2
                .insert_many(&mut inserts, |_, _| panic!());
            self.all_index_2_0_1
                .insert_many(&mut inserts, |_, _| panic!());

            self.new
                .extend(inserts.iter().copied().map(|x| (next_timestamp, x)));
        }
        fn update_finalize(&mut self, uf: &mut Unification, next_timestamp: TimeStamp) {
            // here, we want to:
            // 1) dedup
            // 2) remove outdated
            // 3) remove anything that is not new for this timestamp
            //
            // But 3) requires that ALL has indexes, so that has to occur in update.
            //
            // NOTE: there is nothing stopping us from calling update_finalize lazily too, if we
            // are OK with moving dedup to clean_new.

            let start_idx = self.new.partition_point(|x| x.0 < next_timestamp);
            self.new[start_idx..].sort();
            dedup_suffix(&mut self.new, start_idx);
            retain_suffix(&mut self.new, start_idx, |(_, x)| {
                uf.math.is_root(x.0) && uf.math.is_root(x.1) && uf.math.is_root(x.2)
            });
        }
        fn clean_new(&mut self, uf: &mut Unification, starting_at: TimeStamp) {
            let start_idx = self.new.partition_point(|x| x.0 < starting_at);

            // Anything we remove from ALL is safe to remove from NEW. We know that outdated
            // e-classes are removed from ALL, so they can be removed from NEW.
            //
            // For lattice variables, we would have to query ALL.
            //
            // Here, we are doing 3 random accesses, which is assumed to be cheaper than querying
            // ALL.

            retain_suffix(&mut self.new, start_idx, |(_, x)| {
                uf.math.is_root(x.0) && uf.math.is_root(x.1) && uf.math.is_root(x.2)
            });
        }
        fn iter_new(&self, starting_at: TimeStamp) -> impl Iterator<Item = (Math, Math, Math)> {
            let start_idx = self.new.partition_point(|x| x.0 < starting_at);
            self.new[start_idx..].iter().copied().map(|(_, x)| x)
        }
        fn gc(&mut self, lowest_valid: TimeStamp) {}

        // old iteration functions iterate through all.
    }
}
