//! THIS IS A DRAFT FILE FOR RELATION IMPLEMENTATION

use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    marker::PhantomData,
    mem,
};

struct UnionFind<T> {
    repr: Vec<i32>,
    _marker: PhantomData<T>,
}
trait Eclass: Clone + Copy + PartialEq + Eq {}
impl<T: Eclass> UnionFind<T> {
    /// returning uprooted
    fn union(&mut self, a: T, b: T) -> Option<T> {
        todo!()
    }
    fn find(&mut self, k: T) -> T {
        todo!()
    }
    fn find_tagged(&mut self, k: T) -> (T, bool) {
        let ret = self.find(k);
        (ret, ret != k)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Math(u32);
impl Eclass for Math {}

/// two-way implicit functionality, 0->1 and 1->0
#[derive(Default)]
struct NegRelation {
    all_0_1: BTreeMap<u32, u32>,
    all_1_0: BTreeMap<u32, u32>,
    // ALL QUERIES WILL BEGIN WITH LOOPING ALL NEW IN A RELATION OR LOOPING ALL NEW OF A TYPE. INDICES ON NEW ARE USELESS
    //new_0_1: BTreeMap<u32, u32>,
    //new_1_0: BTreeMap<u32, u32>,
    new: Vec<(u32, u32)>,
    // TODO: AFTER REMOVING A ROW (3,4) due to canonicalizing 3, WE MUST ALSO
    math_index: HashMap<Math, Vec<(Math, Math)>>,
}
const MATH_REFERRERS: usize = 3;
const MATH_REFERRER_INDEX: usize = 1;

impl NegRelation {
    // ignore querying
    fn bulk_update<'a>(
        &mut self,
        insertions: impl 'a + Iterator<Item = (Math, Math)>,
        math_self_uprooted: &mut Vec<Math>,
        math_others_uprooted: impl 'a + Iterator<Item = Math>,
        math_uf: &mut UnionFind<Math>,
    ) {
        // Both insertions and existing data may contain currently uprooted elements.

        // Insertions are canonicalized whenever possible.
        // Removals must never be canonicalized.

        fn uprooted_into_ops<'a>(
            math_index: &'a mut HashMap<Math, Vec<(Math, Math)>>,
            math_uf: &'a mut UnionFind<Math>,
            uprooted: impl 'a + Iterator<Item = Math>,
        ) -> impl 'a + Iterator<Item = (bool, Math, Math)> {
            uprooted
                .filter_map(|x| math_index.remove(&x))
                .flatten()
                .flat_map(|(a, b)| [(true, math_uf.find(a), math_uf.find(b)), (false, a, b)])
        }

        let mut ops_insertions_else_removals: Vec<(bool, Math, Math)> = Iterator::chain(
            insertions.map(|(a, b)| (true, a, b)),
            uprooted_into_ops(&mut self.math_index, math_uf, math_others_uprooted),
        )
        .collect();

        assert!(!ops_insertions_else_removals.is_empty());

        let mut new_ops_insertions_else_removals: Vec<(bool, Math, Math)> = Vec::new();

        math_self_uprooted.clear();
        loop {
            let math_self_uprooted_offset = math_self_uprooted.len();

            for (insertion, a, b) in ops_insertions_else_removals.iter_mut() {
                if !*insertion {
                    continue;
                }
                *a = math_uf.find(*a);
                *b = math_uf.find(*b);
                // NOTE: Canonicalization invariant. Whatever is inserted here must be exactly the
                // same row as is inserted into the indices. This means no calling `math_uf::find`
                // within the outer loop.
                self.math_index.entry(*a).or_default().push((*a, *b));
            }

            batch_api(
                &mut self.all_0_1,
                &mut ops_insertions_else_removals,
                math_uf,
                math_self_uprooted,
            );
            batch_api(
                &mut self.all_1_0,
                &mut ops_insertions_else_removals,
                math_uf,
                math_self_uprooted,
            );

            new_ops_insertions_else_removals.extend(uprooted_into_ops(
                &mut self.math_index,
                math_uf,
                math_self_uprooted[math_self_uprooted_offset..]
                    .iter()
                    .copied(),
            ));
            if new_ops_insertions_else_removals.is_empty() {
                break;
            } else {
                mem::swap(
                    &mut new_ops_insertions_else_removals,
                    &mut ops_insertions_else_removals,
                );
                new_ops_insertions_else_removals.clear();
            }
        }
    }
}

fn batch_api(
    index: &mut BTreeMap<u32, u32>,
    // Permuted but otherwise not modified
    ops_insertions_else_removals: &mut [(bool, Math, Math)],
    math_uf: &mut UnionFind<Math>,
    // Insertions triggering implicit functionality do not mutate the `index`, instead emitting `uprooted` here.
    new_uprooted: &mut Vec<Math>,
) {
    // NOTE: This should really be a recursive bulk operation on the BTree
    // That partitions the ops array around pivots similar to quicksort
    for &(insertion, Math(a), Math(b)) in ops_insertions_else_removals.iter() {
        if insertion {
            index
                .entry(a)
                .and_modify(|b_old| {
                    math_uf
                        .union(Math(b), Math(*b_old))
                        .inspect(|uprooted| new_uprooted.push(*uprooted));
                })
                .or_insert(b);
        } else {
            index.remove(&a).unwrap();
        }
    }
}

#[derive(Clone, Copy)]
struct Others<'a, T>(&'a [Vec<T>], &'a [Vec<T>]);
impl<'a, T: Copy> Others<'a, T> {
    fn is_empty(&self) -> bool {
        self.0.iter().chain(self.1.iter()).all(|v| v.is_empty())
    }
    fn get(self) -> impl 'a + Iterator<Item = T> {
        self.0.iter().chain(self.1.iter()).flatten().copied()
    }
}
fn uprooted_self_and_others<const N: usize, T: Copy>(
    arr: &mut [Vec<T>; N],
    i: usize,
) -> (&mut Vec<T>, Others<'_, T>) {
    assert!(i < arr.len());
    let (prefix, rest) = arr.split_at_mut(i);
    let (mid, suffix) = rest.split_first_mut().unwrap();
    (mid, Others(&*prefix, &*suffix))
}
