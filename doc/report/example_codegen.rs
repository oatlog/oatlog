//! ```
//! (datatype Math
//!   (Mul Math Math)
//!   (Add Math Math)
//! )
//! (rewrite (Add a b) (Add b a))
//! (rewrite (Mul a b) (Mul b a))
//! (rewrite (Add (Mul a b) (Mul a c)) (Mul a (Add b c)))
//! ```
//!
//! Compiled in relaxed mode

// == E-GRAPH STRUCTURE ==

use oatlog::runtime::{self, *};
eclass_wrapper_ty!(Math);
// Both `Math` and `TimeStamp` are newtypes around `u32`.

#[derive(Debug, Default)]
struct AddRelation {
    new: Vec<<Self as Relation>::Row>,
    all: Vec<(Math, Math, Math, TimeStamp)>,
    fd_index_0_1: runtime::HashMap<(Math, Math), (Math, TimeStamp)>,
    nofd_index_0: runtime::IndexedSortedList<(Math,), (Math, Math, TimeStamp)>,
    math_num_uprooted_at_latest_retain: usize,
}
#[derive(Debug, Default)]
struct MulRelation {
    new: Vec<<Self as Relation>::Row>,
    all: Vec<(Math, Math, Math, TimeStamp)>,
    fd_index_0_1: runtime::HashMap<(Math, Math), (Math, TimeStamp)>,
    nofd_index_0: runtime::IndexedSortedList<(Math,), (Math, Math, TimeStamp)>,
    nofd_index_0_2: runtime::IndexedSortedList<(Math, Math), (Math, TimeStamp)>,
    nofd_index_2: runtime::IndexedSortedList<(Math,), (Math, Math, TimeStamp)>,
    math_num_uprooted_at_latest_retain: usize,
}
#[derive(Debug, Default)]
pub struct Delta {
    mul_: Vec<<MulRelation as Relation>::Row>,
    add_: Vec<<AddRelation as Relation>::Row>,
}
#[derive(Debug, Default)]
struct Unification {
    pub math_: UnionFind<Math>,
}
#[derive(Debug, Default)]
pub struct Theory {
    pub latest_timestamp: TimeStamp,
    pub delta: Delta,
    pub uf: Unification,
    pub mul_: MulRelation,
    pub add_: AddRelation,
}
impl Theory {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn step(&mut self) {
        self.apply_rules();
        self.canonicalize();
    }
}

// == E-MATCHING ==
impl Theory {
    pub fn apply_rules(&mut self) {
        for (a_2, b_2, v5) in self.mul_.iter_new() {
            self.delta.insert_mul((a_2, b_2, v5));
            self.delta.insert_mul((b_2, a_2, v5));
            if self.mul_.check_0(a_2) {
                for (v10, v11) in self.add_.iter_all_0_to_1_2(v5) {
                    for (c,) in self.mul_.iter_all_0_2_to_1(a_2, v10) {
                        #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                        let (v12,) =
                            self.add_
                                .entry_0_1_to_2(b_2, c, &mut self.delta, &mut self.uf);
                        self.delta.insert_mul((a_2, v12, v11));
                        self.delta.insert_mul((v12, a_2, v11));
                        self.delta.insert_add((c, b_2, v12));
                    }
                }
            }
        }
        for (a, b, v2) in self.add_.iter_new() {
            self.delta.insert_add((a, b, v2));
            self.delta.insert_add((b, a, v2));
            if self.mul_.check_2(a) {
                for (a_4, c_2) in self.mul_.iter_old_2_to_0_1(b, self.latest_timestamp) {
                    for (b_4,) in self.mul_.iter_old_0_2_to_1(a_4, a, self.latest_timestamp) {
                        #[doc = "( rewrite ( Add ( Mul a b ) ( Mul a c ) ) ( Mul a ( Add b c ) ) )"]
                        let (v19,) =
                            self.add_
                                .entry_0_1_to_2(b_4, c_2, &mut self.delta, &mut self.uf);
                        self.delta.insert_mul((a_4, v19, v2));
                        self.delta.insert_mul((v19, a_4, v2));
                        self.delta.insert_add((c_2, b_4, v19));
                    }
                }
            }
        }
    }
}
impl AddRelation {
    fn iter_all_0_1_to_2(&self, x0: Math, x1: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.fd_index_0_1
            .get(&(x0, x1))
            .into_iter()
            .copied()
            .map(|(x2, _timestamp)| (x2,))
    }
    fn entry_0_1_to_2(
        &self,
        x0: Math,
        x1: Math,
        delta: &mut Delta,
        uf: &mut Unification,
    ) -> (Math,) {
        if let Some((x2,)) = self.iter_all_0_1_to_2(x0, x1).next() {
            return (x2,);
        }
        let x2 = uf.math_.add_eclass();
        delta.add_.push((x0, x1, x2));
        (x2,)
    }
    fn iter_all_0_to_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.nofd_index_0
            .iter((x0,))
            .map(|(x1, x2, _timestamp)| (x1, x2))
    }
}
impl MulRelation {
    fn iter_all_0_to_1_2(&self, x0: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.nofd_index_0
            .iter((x0,))
            .map(|(x1, x2, _timestamp)| (x1, x2))
    }
    fn check_0(&self, x0: Math) -> bool {
        self.iter_all_0_to_1_2(x0).next().is_some()
    }
    fn iter_all_0_2_to_1(&self, x0: Math, x2: Math) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.nofd_index_0_2
            .iter((x0, x2))
            .map(|(x1, _timestamp)| (x1,))
    }
    fn iter_old_0_2_to_1(
        &self,
        x0: Math,
        x2: Math,
        latest_timestamp: TimeStamp,
    ) -> impl Iterator<Item = (Math,)> + use<'_> {
        self.nofd_index_0_2
            .iter((x0, x2))
            .filter_map(move |(x1, timestamp)| (timestamp < latest_timestamp).then_some((x1,)))
    }
    fn iter_all_2_to_0_1(&self, x2: Math) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.nofd_index_2
            .iter((x2,))
            .map(|(x0, x1, _timestamp)| (x0, x1))
    }
    fn iter_old_2_to_0_1(
        &self,
        x2: Math,
        latest_timestamp: TimeStamp,
    ) -> impl Iterator<Item = (Math, Math)> + use<'_> {
        self.nofd_index_2
            .iter((x2,))
            .filter_map(move |(x0, x1, timestamp)| {
                (timestamp < latest_timestamp).then_some((x0, x1))
            })
    }
    fn check_2(&self, x2: Math) -> bool {
        self.iter_all_2_to_0_1(x2).next().is_some()
    }
}

// == CANONICALIZATION ==
impl Theory {
    pub fn canonicalize(&mut self) {
        self.latest_timestamp.0 += 1;
        self.mul_.clear_new();
        self.add_.clear_new();
        if !self.delta.has_new_inserts() && self.uf.num_uprooted() == 0 {
            return;
        }
        self.mul_
            .update_begin(&self.delta.mul_, &mut self.uf, self.latest_timestamp);
        self.add_
            .update_begin(&self.delta.add_, &mut self.uf, self.latest_timestamp);
        self.delta.mul_.clear();
        self.delta.add_.clear();
        loop {
            let mut progress = false;
            progress |= self
                .mul_
                .update(&mut self.delta.mul_, &mut self.uf, self.latest_timestamp);
            progress |= self
                .add_
                .update(&mut self.delta.add_, &mut self.uf, self.latest_timestamp);
            if !progress {
                break;
            }
        }
        self.mul_
            .update_finalize(&mut self.uf, self.latest_timestamp);
        self.add_
            .update_finalize(&mut self.uf, self.latest_timestamp);
        self.uf.reset_num_uprooted();
    }
}
impl Relation for AddRelation {
    type Row = (Math, Math, Math);
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn update_begin(
        &mut self,
        insertions: &[Self::Row],
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) {
        for &(mut x0, mut x1, mut x2) in insertions {
            match self
                .fd_index_0_1
                .entry((uf.math_.find(x0), uf.math_.find(x1)))
            {
                runtime::HashMapEntry::Occupied(mut entry) => {
                    let (y2, timestamp) = entry.get_mut();
                    let old_val = *y2;
                    let changed = (old_val != uf.math_.union_mut(&mut x2, y2));
                    if changed {
                        *timestamp = latest_timestamp;
                    }
                }
                runtime::HashMapEntry::Vacant(entry) => {
                    entry.insert((uf.math_.find(x2), latest_timestamp));
                }
            }
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) -> bool {
        if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
            return false;
        }
        self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
        assert!(insertions.is_empty());
        self.fd_index_0_1
            .retain(|&(x0, x1), &mut (x2, _timestamp)| {
                if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                    true
                } else {
                    insertions.push((x0, x1, x2));
                    false
                }
            });
        self.update_begin(&insertions, uf, latest_timestamp);
        insertions.clear();
        true
    }
    fn update_finalize(&mut self, uf: &mut Unification, latest_timestamp: TimeStamp) {
        assert!(self.new.is_empty());

        self.new.extend(
            self.fd_index_0_1
                .iter()
                .filter_map(|(&(x0, x1), &(x2, timestamp))| {
                    if timestamp == latest_timestamp {
                        Some((x0, x1, x2))
                    } else {
                        None
                    }
                }),
        );
        RadixSortable::wrap(&mut self.new).voracious_sort();
        self.all.clear();
        self.all.extend(
            self.fd_index_0_1
                .iter()
                .map(|(&(x0, x1), &(x2, timestamp))| (x0, x1, x2, timestamp)),
        );

        RowSort100::sort(&mut self.all);
        unsafe {
            self.nofd_index_0.reconstruct(
                &mut self.all,
                |(x0, x1, x2, timestamp)| (x0,),
                |(x0, x1, x2, timestamp)| (x1, x2, timestamp),
            );
        }
        self.math_num_uprooted_at_latest_retain = 0;
    }
}
impl Relation for MulRelation {
    type Row = (Math, Math, Math);
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn update_begin(
        &mut self,
        insertions: &[Self::Row],
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) {
        for &(mut x0, mut x1, mut x2) in insertions {
            match self
                .fd_index_0_1
                .entry((uf.math_.find(x0), uf.math_.find(x1)))
            {
                runtime::HashMapEntry::Occupied(mut entry) => {
                    let (y2, timestamp) = entry.get_mut();
                    let old_val = *y2;
                    let changed = (old_val != uf.math_.union_mut(&mut x2, y2));
                    if changed {
                        *timestamp = latest_timestamp;
                    }
                }
                runtime::HashMapEntry::Vacant(entry) => {
                    entry.insert((uf.math_.find(x2), latest_timestamp));
                }
            }
        }
    }
    fn update(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) -> bool {
        if self.math_num_uprooted_at_latest_retain == uf.math_.num_uprooted() {
            return false;
        }
        assert!(insertions.is_empty());
        self.math_num_uprooted_at_latest_retain = uf.math_.num_uprooted();
        self.fd_index_0_1
            .retain(|&(x0, x1), &mut (x2, _timestamp)| {
                if uf.math_.is_root(x0) & uf.math_.is_root(x1) & uf.math_.is_root(x2) {
                    true
                } else {
                    insertions.push((x0, x1, x2));
                    false
                }
            });
        self.update_begin(&insertions, uf, latest_timestamp);
        insertions.clear();
        true
    }
    fn update_finalize(&mut self, uf: &mut Unification, latest_timestamp: TimeStamp) {
        assert!(self.new.is_empty());
        self.new.extend(
            self.fd_index_0_1
                .iter()
                .filter_map(|(&(x0, x1), &(x2, timestamp))| {
                    if timestamp == latest_timestamp {
                        Some((x0, x1, x2))
                    } else {
                        None
                    }
                }),
        );
        RadixSortable::wrap(&mut self.new).voracious_sort();
        self.all.clear();
        self.all.extend(
            self.fd_index_0_1
                .iter()
                .map(|(&(x0, x1), &(x2, timestamp))| (x0, x1, x2, timestamp)),
        );

        RowSort100::sort(&mut self.all);
        unsafe {
            self.nofd_index_0.reconstruct(
                &mut self.all,
                |(x0, x1, x2, timestamp)| (x0,),
                |(x0, x1, x2, timestamp)| (x1, x2, timestamp),
            );
        }

        RowSort101::sort(&mut self.all);
        unsafe {
            self.nofd_index_0_2.reconstruct(
                &mut self.all,
                |(x0, x1, x2, timestamp)| (x0, x2),
                |(x0, x1, x2, timestamp)| (x1, timestamp),
            );
        }

        RowSort001::sort(&mut self.all);
        unsafe {
            self.nofd_index_2.reconstruct(
                &mut self.all,
                |(x0, x1, x2, timestamp)| (x2,),
                |(x0, x1, x2, timestamp)| (x0, x1, timestamp),
            );
        }

        self.math_num_uprooted_at_latest_retain = 0;
    }
}

// MISC
impl Delta {
    fn has_new_inserts(&self) -> bool {
        !(self.mul_.is_empty() && self.add_.is_empty())
    }
    pub fn insert_mul(&mut self, x: <MulRelation as Relation>::Row) {
        self.mul_.push(x);
    }
    pub fn insert_add(&mut self, x: <AddRelation as Relation>::Row) {
        self.add_.push(x);
    }
}
impl Unification {
    fn num_uprooted(&mut self) -> usize {
        self.math_.num_uprooted()
    }
    fn reset_num_uprooted(&mut self) {
        self.math_.reset_num_uprooted();
    }
}
impl EclassProvider<Math> for Theory {
    fn make(&mut self) -> Math {
        self.uf.math_.add_eclass()
    }
    fn find(&mut self, t: Math) -> Math {
        self.uf.math_.find(t)
    }
    fn union(&mut self, a: Math, b: Math) {
        self.uf.math_.union(a, b);
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
