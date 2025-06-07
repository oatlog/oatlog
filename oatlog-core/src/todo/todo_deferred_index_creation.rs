use crate::runtime::{self, *};
eclass_wrapper_ty!(Math);
fn i64_add012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
    a.checked_add(b).map(|x| (x,)).into_iter()
}
fn i64_mul012(a: i64, b: i64) -> impl Iterator<Item = (i64,)> {
    a.checked_mul(b).map(|x| (x,)).into_iter()
}

struct Unification {
    pub math_: UnionFind<Math>,
}

#[derive(Default)]
struct AddRelation {
    new: Vec<<Self as Relation>::Row>,
    all: Vec<(Math, Math, Math, TimeStamp)>,
    fd_index_0_1: runtime::HashMap<(Math, Math), (Math, TimeStamp)>,
    nofd_index_0: runtime::IndexedSortedList<(Math,), (Math, Math, TimeStamp)>,
    nofd_index_1: runtime::IndexedSortedList<(Math,), (Math, Math, TimeStamp)>,
    math_num_uprooted_at_latest_retain: usize,
    need_deferred: bool,
}
impl Relation for AddRelation {
    type Row = (Math, Math, Math);
    type Unification = Unification;
    const COST: u32 = 9u32;
    fn new() -> Self {
        Self::default()
    }
    fn has_new(&self) -> bool {
        !self.new.is_empty()
    }
    fn clear_new(&mut self) {
        self.new.clear();
    }
    fn iter_new(&self) -> impl '_ + Iterator<Item = Self::Row> {
        self.new.iter().copied()
    }
    fn len(&self) -> usize {
        self.fd_index_0_1.len()
    }
    fn emit_graphviz(&self, buf: &mut String) {
        use std::fmt::Write;
        for (i, ((x0, x1), (x2, _timestamp))) in self
            .fd_index_0_1
            .iter()
            .map(|(k, v)| ((*k), (*v)))
            .enumerate()
        {
            writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x0).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x1).unwrap();
            writeln!(buf, "{}_{i} -> {}_{};", "add", "math", x2).unwrap();
            writeln!(buf, "{}_{i} [shape = box];", "add").unwrap();
        }
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
                    let changed = false;
                    let old_val = *y2;
                    let changed = changed | (old_val != uf.math_.union_mut(&mut x2, y2));
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
        let offset = insertions.len();
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
        self.update_begin(&insertions[offset..], uf, latest_timestamp);
        true
    }
    fn update_finalize(
        &mut self,
        insertions: &mut Vec<Self::Row>,
        uf: &mut Unification,
        latest_timestamp: TimeStamp,
    ) {
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
        insertions.clear();

        self.need_deferred = true;

        self.math_num_uprooted_at_latest_retain = 0;
    }
}
impl AddRelation {
    fn deferred_reconstruct(&mut self) {
        if self.need_deferred {
            self.need_deferred = false;

            RowSort100::sort(&mut self.all);
            unsafe {
                self.nofd_index_0.reconstruct(
                    &mut self.all,
                    |(x0, x1, x2, timestamp)| (x0,),
                    |(x0, x1, x2, timestamp)| (x1, x2, timestamp),
                );
            }
            RowSort010::sort(&mut self.all);
            unsafe {
                self.nofd_index_1.reconstruct(
                    &mut self.all,
                    |(x0, x1, x2, timestamp)| (x1,),
                    |(x0, x1, x2, timestamp)| (x0, x2, timestamp),
                );
            }
        }
    }
}
