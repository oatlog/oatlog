//! What could a free join-like impl look like?

#![allow(unsafe_code)]
#![allow(unused)]
#![allow(dead_code)]
#![allow(private_interfaces)]
#![allow(non_snake_case)]
use crate::{
    eclass_wrapper_ty,
    ids::{RelationId, VariableId},
    runtime::*,
};
use std::{
    cell::Cell,
    collections::{BTreeSet, HashMap, HashSet, hash_map::Entry},
    marker::PhantomData,
    mem::take,
};

trait TrieNode {}
impl<A, B> TrieNode for Trie<A, B> {}

struct Trie<A, B> {
    map: HashMap<A, B>,
}
impl<A: RelationElement, B> Trie<A, B> {
    // get
    // iter
    fn get(&self, a: A) -> Option<&B> {
        self.map.get(&a)
    }
}

impl<A: RelationElement, B: RelationElement> Trie<A, B> {
    fn iter(&self) -> impl Iterator<Item = (&A, &B)> {
        self.map.iter()
    }
}

type Map<A, B> = HashMap<A, B>;
type Set<T> = HashSet<T>;

impl<A: RelationElement, B: TrieNode> Trie<A, B> {}
mod query_plan {
    use super::*;

    struct Atom {
        relation: RelationId,
        variables: u32,
    }

    // The sets of subatoms for an atom are disjoint sets of the variables in the atom.
    struct SubAtom {
        relation: RelationId,
        variables: u32,
    }

    struct Node {
        // only the cover is allowed to introduce variables
        // (for loop)
        // but there can be multiple covers.
        // consider
        // [R(x, y), S(x, y)]
        // either of these can be a cover.
        // at runtime, the best cover has to be selected.
        subatoms: Vec<SubAtom>,

        preceeding: Option<Box<Node>>,
    }
    impl Node {
        // set of variables used by this node
        fn vs(&self) -> u32 {
            self.subatoms
                .iter()
                .map(|x| x.variables)
                .fold(0, |a, b| a ^ b)
        }
        // set of variables *made avaliable by* this node
        fn avs(&self) -> u32 {
            self.vs() | self.preceeding.as_ref().map(|x| x.avs()).unwrap_or(0)
        }
    }

    // ASSUME: without loss of generality, assume relationids are unique
    // ASSUME: functional_dependencies is closed.
    // ASSUME: u32 is a set of variable ids
    // ASSUME: provided order of atoms is the order we want.
    fn query_plan(mut atoms: Vec<(RelationId, u32)>, functional_dependencies: Vec<(u32, u32)>) {
        struct Step {
            cover: (RelationId, u32),
            constrain: Vec<(RelationId, u32)>,
            derived: Derived,
        }
        #[derive(Default)]
        struct Derived {
            introduces: u32,
            bound: u32,
        }

        // let mut plan = vec![];

        // while let Some(atom) = atoms.pop() {
        // }
    }
}

trait SliceExt<T> {
    /// pop front of slice
    fn pop_front(&mut self) -> Option<T>;
}
impl<T: Copy> SliceExt<T> for &[T] {
    fn pop_front(&mut self) -> Option<T> {
        if let [first, rest @ ..] = &*self {
            let first = *first;
            *self = rest;
            Some(first)
        } else {
            None
        }
    }
}

struct IndexMap<A, B> {
    map: HashMap<A, B>,
    rows: Vec<A>,
    free: Vec<u32>,
}
impl<K: Copy + Hash + Eq, V: Copy> IndexMap<K, V> {
    // F(old, new)
    fn insert<F: FnOnce(V, V) -> V>(&mut self, k: K, v: V, mut resolve: F) -> Option<u32> {
        match self.map.entry(k) {
            Entry::Occupied(mut entry) => {
                let entry = entry.get_mut();
                *entry = resolve(*entry, v);
                todo!()
            }
            Entry::Vacant(entry) => {
                todo!()
            }
        }
    }
}

mod ght {

    trait GHT {}
}

// (a, (b, (c, ())))
struct BinarySearchIndex<T> {
    range: Range,
    cols: T,
}
impl<T> BinarySearchIndex<T> {
    fn new(cols: T, elems: u32) -> Self {
        Self {
            range: Range::new(0, elems - 1),
            cols,
        }
    }
    fn iter(&self) -> impl Iterator<Item = u32> {
        self.range.iter()
    }
}
impl<A: RelationElement, B> BinarySearchIndex<(&[A], B)> {
    fn get(&self, a_start: A, a_end: A) -> Option<BinarySearchIndex<B>> {
        self.get_range(a_start, a_end)
    }
    fn get_range(&self, a_start: A, a_end: A) -> Option<BinarySearchIndex<B>> {
        todo!()
    }
}

// inclusive
// construct these by packing the slice of uprooted not seen by any relation yet.
#[derive(Copy, Clone)]
struct Range {
    start: u32,
    end: u32,
}
impl Range {
    fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
    fn iter(&self) -> impl Iterator<Item = u32> + use<> {
        self.start..=self.end
    }
}

// restrict slice `s` such that it only contains `x`
//
//
// [0, 0, 0, x, x, x, 999, 999, 999]
//           ^     ^
//            slice
fn binary_search<A: RelationElement>(s: &[A], x: Range) -> Option<Range> {
    todo!()
}

mod trie_index {

    use super::*;

    // (nodeid is not an eclass, this is just a convenience thing)
    eclass_wrapper_ty!(NodeId, Element);

    // For the levels that we do not care about the ranges (just want to check if something is
    // contained here), we can have a separate struct and a separate HashMap. Actually we could
    // reasonably have a separate HashMap for each level of the trie.
    struct NodeSmall {
        id: u32,
    }
    // Invariant: relevant range is sorted.
    struct Node {
        id: NodeId,
        // exclusive range.
        start: u32,
        end: u32,
        // Have we sorted/filled in HashMap entries yet? This is needed because we can't tell the
        // difference between something not existing in the map because it's lazily generated or
        // because it does not exist.
        lazy: Cell<bool>,
    }

    fn test() {
        let mut id_alloc = {
            let mut id = 0;
            move || {
                let x = id;
                id += 1;
                NodeId(x)
            }
        };
        {
            struct Trie {
                map: HashMap<Element, Trie>,
            }
        }

        // all of our indexes
        let mut trie: HashMap<(NodeId, Element), Node> = HashMap::new();
        // trie.insert

        let col_a: Vec<Element> = vec![/* ... */];
        let col_b: Vec<Element> = vec![/* ... */];
        let col_c: Vec<Element> = vec![/* ... */];

        // A -> B -> C
        let n = col_a.len();

        let mut arr_abc: Vec<u32> = vec![];

        let root__abc = &Node {
            id: id_alloc(),
            start: 0,
            end: n as u32,
            lazy: false.into(),
        };

        for a in [Element(0)] {
            let Some(root_a_bc) = ({
                if !root__abc.lazy.replace(true) {

                    // NOTE: if we are at the innermost level, we can inline data into the array.
                    // Actually, the steps for sorting and trie insertion should be separate.

                    // We need to fill in the trie. Note that the sorting step can be skipped
                    // depending on how the original relation is sorted. Then we just have row ids
                    // implicitly.

                    // did we allocate abc?
                    if col_a.len() != arr_abc.len() {
                        arr_abc = (0..(n as u32)).collect();
                    }

                    let subslice =
                        &mut arr_abc[(root__abc.start as usize)..(root__abc.end as usize)];
                    subslice.sort_by_key(|&i| col_a[i as usize]);

                    let n = subslice.len();

                    let mut i = 0;
                    while i < n {
                        let e = col_a[subslice[i] as usize];
                        let mut j = i + 1;
                        while j < n && col_a[subslice[j] as usize] == e {
                            j += 1;
                        }
                        // batch i..j
                        {
                            let node = Node {
                                id: id_alloc(),
                                start: i as u32,
                                end: j as u32,
                                lazy: false.into(),
                            };
                            trie.insert((root__abc.id, e), node);
                        }
                        i = j;
                    }
                }
                // trie is filled in, so the lookup can be performed.
                // actually returning a reference is problematic because hashmap inserts can move
                // entries.
                trie.get(&(root__abc.id, a))
            }) else {
                continue;
            };
        }
    }
}
mod theory7 {
    use super::*;
    use smallvec::SmallVec;

    // NOTE: all row ids are unstable.
    struct AddRelation {
        // ======== TRANSIENT STATE ========
        deleted: Vec<bool>,
        num_deleted: usize,

        // ======== PERSISTENT STATE ========

        // any -> rowid
        // TODO: can we justify making this a btreeset?
        // BTreeSet<(eclass, rowid)>
        // or a sorted array?
        // Vec<(eclass, rowid)>,
        //
        // the latter avoids allocations.
        back_references: Vec<SmallVec<[u32; 4]>>,

        // rowid -> row
        // some rows are dead.
        col_a: Vec<u32>,
        col_b: Vec<u32>,
        col_c: Vec<u32>,

        // (a, b) -> c
        map: HashMap<(u32, u32), u32>,

        // TODO: could this be rowids?
        // it would work but need to take care for multiple calls to `update`
        new: Vec<(u32, u32, u32)>,
    }
    impl AddRelation {
        fn notify_eclasses(&mut self, num_eclasses: u32) {
            self.back_references
                .resize(num_eclasses as usize, SmallVec::new());
        }
        fn update(
            &mut self,
            uproot: &[Range],
            inserts: &mut Vec<(u32, u32, u32)>,
            uf: &mut UnionFind<u32>,
        ) {
            let mut uprooted = Vec::new();
            for i in uproot.iter().copied().flat_map(|r| r.start..=r.end) {
                // drain not needed because back_references will be reconstructed.
                uprooted.extend(self.back_references[i as usize].iter().copied())
            }
            // sort-dedup is needed for in-place manipulation for reusing row-ids.
            uprooted.sort();
            uprooted.dedup();

            for i in uprooted.iter().copied() {
                let a = self.col_a[i as usize];
                let b = self.col_b[i as usize];
                let c = self.col_c[i as usize];

                let a_new = uf.find(a);
                let b_new = uf.find(b);
                let c_new = uf.find(c);

                assert!(a_new != a || b_new != b || c_new != c);

                // We reconstruct back_references so removing all back references is not necessary.
                // if a != a_new { self.back_references[a as usize].retain(|x| *x != a); }
                // if b != b_new { self.back_references[b as usize].retain(|x| *x != b); }
                // if c != c_new { self.back_references[c as usize].retain(|x| *x != c); }

                // ======= SEMANTIC REMOVE =======
                {
                    // since it is in the uprooted set, we know for sure that it should be deleted.
                    // it is *possible* that just c is modified though, in that case we should be
                    // doing in-place manipulation.

                    // we are guaranteed that this row is not canon.
                    if !self.map.remove(&(a, b)).is_some() {
                        // this row was already deleted because not everything was removed from
                        // back_references
                        continue;
                    }
                }

                match self.map.entry((a_new, b_new)) {
                    Entry::Occupied(mut entry) => {
                        uf.union(*entry.get(), c_new);

                        // Occupied => element is already in relation. => this row is to be
                        // deleted. Whatever row the map referred to will be deleted in the next
                        // call to `update`.
                        self.deleted[i as usize] = true;
                        self.num_deleted += 1;
                    }
                    Entry::Vacant(entry) => {
                        // Vacant => row is new and we can update in-place.

                        // ======= SEMANTIC INSERT =======
                        self.col_a[i as usize] = a_new;
                        self.col_b[i as usize] = b_new;
                        self.col_c[i as usize] = c_new;
                        entry.insert(c_new);

                        self.new.push((a_new, b_new, c_new));

                        self.back_references[a_new as usize].push(i);
                        self.back_references[b_new as usize].push(i);
                        self.back_references[c_new as usize].push(i);
                    }
                }
            }

            for (a, b, c) in inserts.drain(..) {
                let a = uf.find(a);
                let b = uf.find(b);
                let c = uf.find(c);

                match self.map.entry((a, b)) {
                    Entry::Occupied(entry) => {
                        uf.union(*entry.get(), c);
                        continue;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(c);
                    }
                }

                let rid = self.col_a.len() as u32;
                self.col_a.push(a);
                self.col_b.push(b);
                self.col_c.push(c);
                self.deleted.push(false);

                self.new.push((a, b, c));

                self.back_references[a as usize].push(rid);
                self.back_references[b as usize].push(rid);
                self.back_references[c as usize].push(rid);
            }
        }

        fn update_finalize(&mut self, uf: &mut UnionFind<u32>) {
            // NOTE: rowids are stable up until this point.

            // re-sort relation and remove deleted rows.
            // TODO: do the optimized thing here.
            let n = self.col_a.len();
            let mut ordering: Vec<u32> = (0..(n as u32)).collect();
            ordering.sort_by_key(|&i| {
                let i = i as usize;
                // NOTE: we can pick any convenient sort order here.
                // this just uses A -> B -> C
                (self.deleted[i], self.col_a[i], self.col_b[i], self.col_c[i])
            });

            let mut col_a = self.col_a.clone();
            let mut col_b = self.col_b.clone();
            let mut col_c = self.col_c.clone();

            for (i, k) in ordering.iter().copied().enumerate() {
                col_a[i as usize] = self.col_a[k as usize];
                col_b[i as usize] = self.col_b[k as usize];
                col_c[i as usize] = self.col_c[k as usize];
            }

            self.col_a = col_a;
            self.col_b = col_b;
            self.col_c = col_c;

            let n = self.col_a.len() - self.num_deleted;
            self.num_deleted = 0;
            self.col_a.truncate(n);
            self.col_b.truncate(n);
            self.col_c.truncate(n);
            self.deleted.truncate(n);
            self.deleted.fill(false);

            self.back_references.fill(SmallVec::new());

            for i in 0..n {
                self.back_references[self.col_a[i] as usize].push(i as u32);
                self.back_references[self.col_b[i] as usize].push(i as u32);
                self.back_references[self.col_c[i] as usize].push(i as u32);
            }

            for (a, b, c) in self.new.iter_mut() {
                *a = uf.find(*a);
                *b = uf.find(*b);
                *c = uf.find(*c);
            }
            self.new.sort();
            self.new.dedup();
        }
    }
    struct Theory {
        add: AddRelation,
    }
    impl Theory {
        fn apply_rules(&mut self) {
            let add0 = &self.add;
            let add1 = &self.add;
            // ((a + b) + c) -> (a + (b + c))
            // Add0(a, b, d), Add1(d, c, e)

            for (a, b, d) in add0.new.iter().copied() {
                let Some(add1_rows) = binary_search(&add1.col_a, Range::new(d, d)) else {
                    continue;
                };

                for add1_row in add1_rows.start..=add1_rows.end {
                    let c = add1.col_b[add1_row as usize];
                    let e = add1.col_c[add1_row as usize];
                    emit((a, b, c, d, e));
                }
            }

            let mut add0_c: HashMap<u32, Vec<u32>> = HashMap::new();
            for i in (0..add0.col_a.len()) {
                add0_c.entry(add0.col_c[i]).or_default().push(i as u32);
            }

            for (d, c, e) in add1.new.iter().copied() {
                let Some(add0_rows) = add0_c.get(&d) else {
                    continue;
                };
                for add0_row in add0_rows {
                    let a = add0.col_a[*add0_row as usize];
                    let b = add0.col_b[*add0_row as usize];
                    emit((a, b, c, d, e));
                }
            }
        }
    }
}
mod theory6 {
    use super::*;
    use smallvec::SmallVec;

    // inclusive
    // construct these by packing the slice of uprooted not seen by any relation yet.
    #[derive(Copy, Clone)]
    struct Range {
        start: u32,
        end: u32,
    }

    // NOTE: all row ids are unstable.
    struct AddRelation {
        // ======== TRANSIENT STATE ========
        deleted: Vec<bool>,
        num_deleted: usize,

        // ======== PERSISTENT STATE ========

        // any -> rowid
        // TODO: can we justify making this a btreeset?
        // BTreeSet<(eclass, rowid)>
        // or a sorted array?
        // Vec<(eclass, rowid)>,
        //
        // the latter avoids allocations.
        back_references: Vec<SmallVec<[u32; 4]>>,

        // rowid -> row
        // some rows are dead.
        col_a: Vec<u32>,
        col_b: Vec<u32>,
        col_c: Vec<u32>,

        // (a, b) -> c
        map: HashMap<(u32, u32), u32>,

        // TODO: could this be rowids?
        // it would work but need to take care for multiple calls to `update`
        new: Vec<(u32, u32, u32)>,
    }
    impl AddRelation {
        fn notify_eclasses(&mut self, num_eclasses: u32) {
            self.back_references
                .resize(num_eclasses as usize, SmallVec::new());
        }
        fn update(
            &mut self,
            uproot: &[Range],
            mut inserts: Vec<(u32, u32, u32)>,
            uf: &mut UnionFind<u32>,
        ) {
            let mut uprooted = Vec::new();
            for i in uproot.iter().copied().flat_map(|r| r.start..=r.end) {
                // drain not needed because back_references will be reconstructed.
                uprooted.extend(self.back_references[i as usize].iter().copied())
            }
            // sort-dedup is needed for in-place manipulation for reusing row-ids.
            uprooted.sort();
            uprooted.dedup();

            for i in uprooted.iter().copied() {
                let a = self.col_a[i as usize];
                let b = self.col_b[i as usize];
                let c = self.col_c[i as usize];

                let a_new = uf.find(a);
                let b_new = uf.find(b);
                let c_new = uf.find(c);

                assert!(a_new != a || b_new != b || c_new != c);

                // We reconstruct back_references so removing all back references is not necessary.
                // if a != a_new { self.back_references[a as usize].retain(|x| *x != a); }
                // if b != b_new { self.back_references[b as usize].retain(|x| *x != b); }
                // if c != c_new { self.back_references[c as usize].retain(|x| *x != c); }

                // ======= SEMANTIC REMOVE =======
                {
                    // since it is in the uprooted set, we know for sure that it should be deleted.
                    // it is *possible* that just c is modified though, in that case we should be
                    // doing in-place manipulation.

                    // we are guaranteed that this row is not canon.
                    if !self.map.remove(&(a, b)).is_some() {
                        // this row was already deleted because not everything was removed from
                        // back_references
                        continue;
                    }
                }

                match self.map.entry((a_new, b_new)) {
                    Entry::Occupied(mut entry) => {
                        uf.union(*entry.get(), c_new);

                        // Occupied => element is already in relation. => this row is to be
                        // deleted. Whatever row the map referred to will be deleted in the next
                        // call to `update`.
                        self.deleted[i as usize] = true;
                        self.num_deleted += 1;
                    }
                    Entry::Vacant(entry) => {
                        // Vacant => row is new and we can update in-place.

                        // ======= SEMANTIC INSERT =======
                        self.col_a[i as usize] = a_new;
                        self.col_b[i as usize] = b_new;
                        self.col_c[i as usize] = c_new;
                        entry.insert(c_new);

                        self.new.push((a_new, b_new, c_new));

                        self.back_references[a_new as usize].push(i);
                        self.back_references[b_new as usize].push(i);
                        self.back_references[c_new as usize].push(i);
                    }
                }
            }

            for (a, b, c) in inserts {
                let a = uf.find(a);
                let b = uf.find(b);
                let c = uf.find(c);

                match self.map.entry((a, b)) {
                    Entry::Occupied(entry) => {
                        uf.union(*entry.get(), c);
                        continue;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(c);
                    }
                }

                let rid = self.col_a.len() as u32;
                self.col_a.push(a);
                self.col_b.push(b);
                self.col_c.push(c);
                self.deleted.push(false);

                self.new.push((a, b, c));

                self.back_references[a as usize].push(rid);
                self.back_references[b as usize].push(rid);
                self.back_references[c as usize].push(rid);
            }
        }

        fn update_finalize(&mut self, uf: &mut UnionFind<u32>) {
            // NOTE: rowids are stable up until this point.

            // re-sort relation and remove deleted rows.
            // TODO: do the optimized thing here.
            let n = self.col_a.len();
            let mut ordering: Vec<u32> = (0..(n as u32)).collect();
            ordering.sort_by_key(|&i| {
                let i = i as usize;
                // NOTE: we can pick any convenient sort order here.
                // this just uses A -> B -> C
                (self.deleted[i], self.col_a[i], self.col_b[i], self.col_c[i])
            });

            let mut col_a = self.col_a.clone();
            let mut col_b = self.col_b.clone();
            let mut col_c = self.col_c.clone();

            for (i, k) in ordering.iter().copied().enumerate() {
                col_a[i as usize] = self.col_a[k as usize];
                col_b[i as usize] = self.col_b[k as usize];
                col_c[i as usize] = self.col_c[k as usize];
            }

            self.col_a = col_a;
            self.col_b = col_b;
            self.col_c = col_c;

            let n = self.col_a.len() - self.num_deleted;
            self.num_deleted = 0;
            self.col_a.truncate(n);
            self.col_b.truncate(n);
            self.col_c.truncate(n);
            self.deleted.truncate(n);
            self.deleted.fill(false);

            self.back_references.fill(SmallVec::new());

            for i in 0..n {
                self.back_references[self.col_a[i] as usize].push(i as u32);
                self.back_references[self.col_b[i] as usize].push(i as u32);
                self.back_references[self.col_c[i] as usize].push(i as u32);
            }

            for (a, b, c) in self.new.iter_mut() {
                *a = uf.find(*a);
                *b = uf.find(*b);
                *c = uf.find(*c);
            }
            self.new.sort();
        }
    }
}
mod theory5 {
    use super::*;
    use smallvec::SmallVec;

    // NOTE: all row ids are unstable.
    struct AddRelation {
        // ======== TRANSIENT STATE ========
        deleted: Vec<bool>,
        num_deleted: usize,

        // ======== PERSISTENT STATE ========

        // any -> rowid
        // TODO: can we justify making this a btreeset?
        // BTreeSet<(eclass, rowid)>
        // or a sorted array?
        // Vec<(eclass, rowid)>,
        //
        // the latter avoids allocations.
        back_references: Vec<SmallVec<[u32; 4]>>,

        // rowid -> row
        // some rows are dead.
        col_a: Vec<u32>,
        col_b: Vec<u32>,
        col_c: Vec<u32>,

        // (a, b) -> c
        map: HashMap<(u32, u32), u32>,

        // TODO: could this be rowids?
        // it would work but need to take care for multiple calls to `update`
        new: Vec<(u32, u32, u32)>,
    }
    impl AddRelation {
        fn update(
            &mut self,
            uproot: &[u32],
            mut inserts: Vec<(u32, u32, u32)>,
            uf: &mut UnionFind<u32>,
        ) {
            let mut uprooted = Vec::new();
            for i in uproot.iter().copied() {
                uprooted.extend(self.back_references[i as usize].drain(..))
            }
            // sort-dedup is needed for in-place manipulation for reusing row-ids.
            uprooted.sort();
            uprooted.dedup();

            for i in uprooted.iter().copied() {
                let a = self.col_a[i as usize];
                let b = self.col_b[i as usize];
                let c = self.col_c[i as usize];

                let a_new = uf.find(a);
                let b_new = uf.find(b);
                let c_new = uf.find(c);

                assert!(a_new != a || b_new != b || c_new != c);

                if a != a_new {
                    self.back_references[a as usize].retain(|x| *x != a);
                }
                if b != b_new {
                    self.back_references[b as usize].retain(|x| *x != b);
                }
                if c != c_new {
                    self.back_references[c as usize].retain(|x| *x != c);
                }

                // ======= SEMANTIC REMOVE =======
                {
                    // since it is in the uprooted set, we know for sure that it should be deleted.
                    // it is *possible* that just c is modified though, in that case we should be
                    // doing in-place manipulation.
                    assert!(self.map.remove(&(a, b)).is_some());
                }

                match self.map.entry((a_new, b_new)) {
                    Entry::Occupied(mut entry) => {
                        uf.union(*entry.get(), c_new);

                        // Occupied => element is already in relation. => this row is to be
                        // deleted. Whatever row the map referred to will be deleted in the next
                        // call to `update`.
                        self.deleted[i as usize] = true;
                        self.num_deleted += 1;
                    }
                    Entry::Vacant(entry) => {
                        // Vacant => row is new and we can update in-place.

                        // ======= SEMANTIC INSERT =======
                        self.col_a[i as usize] = a_new;
                        self.col_b[i as usize] = b_new;
                        self.col_c[i as usize] = c_new;
                        entry.insert(c_new);

                        self.new.push((a_new, b_new, c_new));

                        self.back_references[a_new as usize].push(i);
                        self.back_references[b_new as usize].push(i);
                        self.back_references[c_new as usize].push(i);
                    }
                }
            }

            for (a, b, c) in inserts {
                let a = uf.find(a);
                let b = uf.find(b);
                let c = uf.find(c);

                match self.map.entry((a, b)) {
                    Entry::Occupied(entry) => {
                        uf.union(*entry.get(), c);
                        continue;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(c);
                    }
                }

                let rid = self.col_a.len() as u32;
                self.col_a.push(a);
                self.col_b.push(b);
                self.col_c.push(c);
                self.deleted.push(false);

                self.new.push((a, b, c));

                self.back_references[a as usize].push(rid);
                self.back_references[b as usize].push(rid);
                self.back_references[c as usize].push(rid);
            }
        }

        fn update_finalize(&mut self, uf: &mut UnionFind<u32>) {
            // re-sort relation and remove deleted rows.
            {
                // TODO: do the optimized thing here.
                let n = self.col_a.len();
                let mut ordering: Vec<u32> = (0..(n as u32)).collect();
                ordering.sort_by_key(|&i| {
                    let i = i as usize;
                    // NOTE: we can pick any convenient sort order here.
                    // this just uses A -> B -> C
                    (self.deleted[i], self.col_a[i], self.col_b[i], self.col_c[i])
                });

                let mut col_a = self.col_a.clone();
                let mut col_b = self.col_b.clone();
                let mut col_c = self.col_c.clone();

                for (i, k) in ordering.iter().copied().enumerate() {
                    col_a[i as usize] = self.col_a[k as usize];
                    col_b[i as usize] = self.col_b[k as usize];
                    col_c[i as usize] = self.col_c[k as usize];
                }

                self.col_a = col_a;
                self.col_b = col_b;
                self.col_c = col_c;

                let n = self.col_a.len() - self.num_deleted;
                self.num_deleted = 0;
                self.col_a.truncate(n);
                self.col_b.truncate(n);
                self.col_c.truncate(n);
                self.deleted.truncate(n);
                self.deleted.fill(false);
            }

            for (a, b, c) in self.new.iter_mut() {
                *a = uf.find(*a);
                *b = uf.find(*b);
                *c = uf.find(*c);
            }
            self.new.sort();
        }
    }
}
mod theory4 {
    use super::*;
    use smallvec::SmallVec;

    // NOTE: all row ids are unstable.
    struct AddRelation {
        // any -> rowid
        // TODO: can we justify making this a btreeset?
        // BTreeSet<(eclass, rowid)>
        back_references: Vec<SmallVec<[u32; 4]>>,

        // rowid -> row
        // some rows are dead.
        col_a: Vec<u32>,
        col_b: Vec<u32>,
        col_c: Vec<u32>,

        // avaliable rows
        // NOTE: this is problematic for iteration.
        free: Vec<u32>,

        // (a, b) -> res
        map: HashMap<(u32, u32), u32>,

        // TODO: could this be rowids?
        // it would work but need to take care for multiple calls to `update`
        new: Vec<(u32, u32, u32)>,
    }
    impl AddRelation {
        fn update(
            &mut self,
            uproot: &[u32],
            mut inserts: Vec<(u32, u32, u32)>,
            uf: &mut UnionFind<u32>,
        ) {
            // TODO: with a free list, couldn't we just do it in-place and keep a list of the "new"
            // rows?

            let mut uprooted = Vec::new();
            for i in uproot.iter().copied() {
                uprooted.extend(self.back_references[i as usize].drain(..))
            }
            // sort-dedup is needed for in-place manipulation for reusing row-ids.
            uprooted.sort();
            uprooted.dedup();

            for i in uprooted.iter().copied() {
                let a = self.col_a[i as usize];
                let b = self.col_b[i as usize];
                let c = self.col_c[i as usize];

                let a_new = uf.find(a);
                let b_new = uf.find(b);
                let c_new = uf.find(c);

                assert!(a_new != a || b_new != b || c_new != c);

                if a != a_new {
                    self.back_references[a as usize].retain(|x| *x != a);
                }
                if b != b_new {
                    self.back_references[b as usize].retain(|x| *x != b);
                }
                if c != c_new {
                    self.back_references[c as usize].retain(|x| *x != c);
                }

                // ======= SEMANTIC REMOVE =======
                {
                    // since it is in the uprooted set, we know for sure that it should be deleted.
                    // it is *possible* that just c is modified though, in that case we should be
                    // doing in-place manipulation.
                    assert!(self.map.remove(&(a, b)).is_some());
                }

                match self.map.entry((a_new, b_new)) {
                    Entry::Occupied(mut entry) => {
                        uf.union(*entry.get(), c_new);

                        // Occupied => element is already in relation. => this row is to be
                        // deleted. Whatever row the map referred to will be deleted in the next
                        // call to `update`.
                        self.free.push(i);
                    }
                    Entry::Vacant(entry) => {
                        // Vacant => row is new and we can update in-place.

                        // ======= SEMANTIC INSERT =======
                        self.col_a[i as usize] = a_new;
                        self.col_b[i as usize] = b_new;
                        self.col_c[i as usize] = c_new;
                        entry.insert(c_new);

                        self.new.push((a_new, b_new, c_new));

                        self.back_references[a_new as usize].push(i);
                        self.back_references[b_new as usize].push(i);
                        self.back_references[c_new as usize].push(i);
                    }
                }
            }

            for (a, b, c) in inserts {
                let a = uf.find(a);
                let b = uf.find(b);
                let c = uf.find(c);

                match self.map.entry((a, b)) {
                    Entry::Occupied(entry) => {
                        uf.union(*entry.get(), c);
                        continue;
                    }
                    Entry::Vacant(entry) => {
                        entry.insert(c);
                    }
                }

                let rid = if let Some(i) = self.free.pop() {
                    self.col_a[i as usize] = a;
                    self.col_b[i as usize] = b;
                    self.col_c[i as usize] = c;
                    i
                } else {
                    let rid = self.col_a.len() as u32;
                    self.col_a.push(a);
                    self.col_b.push(b);
                    self.col_c.push(c);
                    rid
                };

                self.new.push((a, b, c));

                self.back_references[a as usize].push(rid);
                self.back_references[b as usize].push(rid);
                self.back_references[c as usize].push(rid);
            }

            // TODO: handle holes for self.free.

            // at this point the uprooted rows are not used.
            self.free.extend(uprooted.iter().copied());
        }
    }
}
mod theory3 {
    use super::*;
    use smallvec::SmallVec;

    // NOTE: all row ids are unstable.
    struct AddRelation {
        // any -> rowid
        back_references: Vec<SmallVec<[u32; 4]>>,

        // rowid -> row
        // some rows are dead.
        col_a: Vec<u32>,
        col_b: Vec<u32>,
        col_c: Vec<u32>,

        // avaliable rows
        // NOTE: this is problematic for iteration.
        free: Vec<u32>,

        // (a, b) -> res
        map: HashMap<(u32, u32), u32>,

        // TODO: could this be rowids?
        // it would work but need to take care for multiple calls to `update`
        new: Vec<(u32, u32, u32)>,
    }
    impl AddRelation {
        fn update(&mut self, uproot: &[u32], uf: &mut UnionFind<u32>) {
            // TODO: with a free list, couldn't we just do it in-place and keep a list of the "new"
            // rows?

            let mut uprooted = Vec::new();
            for i in uproot.iter().copied() {
                uprooted.extend(self.back_references[i as usize].drain(..))
            }
            uprooted.sort();
            uprooted.dedup();

            for i in uprooted.iter().copied() {
                let a = self.col_a[i as usize];
                let b = self.col_b[i as usize];
                let c = self.col_c[i as usize];

                // ======= SEMANTIC REMOVE =======

                // since it is in the uprooted set, we know for sure that it should be deleted.
                self.map.remove(&(a, b));
            }
            for i in uprooted.iter().copied() {
                let a_new = uf.find(self.col_a[i as usize]);
                let b_new = uf.find(self.col_b[i as usize]);
                let c_new = uf.find(self.col_c[i as usize]);

                match self.map.entry((a_new, b_new)) {
                    Entry::Occupied(mut entry) => {
                        // NOTE: aren't we essentially guaranteed to have an incorrect value in the
                        // map?

                        let entry = entry.get_mut();
                        let old = *entry;
                        uf.union(old, c_new);

                        let c_new = uf.find(old);
                        if old != c_new {
                            // ======= SEMANTIC INSERT =======
                            self.new.push((a_new, b_new, c_new));

                            let rid = self.col_a.len() as u32;
                            self.back_references[a_new as usize].push(rid);
                            self.back_references[b_new as usize].push(rid);
                            self.back_references[c_new as usize].push(rid);
                            self.col_a.push(a_new);
                            self.col_b.push(b_new);
                            self.col_c.push(c_new);
                        }
                    }
                    Entry::Vacant(mut entry) => {
                        entry.insert(c_new);

                        // ======= SEMANTIC INSERT =======
                        self.new.push((a_new, b_new, c_new));

                        let rid = self.col_a.len() as u32;
                        self.back_references[a_new as usize].push(rid);
                        self.back_references[b_new as usize].push(rid);
                        self.back_references[c_new as usize].push(rid);
                        self.col_a.push(a_new);
                        self.col_b.push(b_new);
                        self.col_c.push(c_new);
                    }
                }
            }

            // at this point the uprooted rows are not used.
            self.free.extend(uprooted.iter().copied());
        }
    }
}
mod theory2 {
    use super::*;
    use crate::typed_vec::TVec;
    use std::mem::size_of;

    use smallvec::SmallVec;

    // TODO: pointer tagging, arena allocation, minimal allocation size/alignment, 32 bit capacity
    // could add another element to the smallvec.

    const _: () = if size_of::<Vec<u32>>() != size_of::<SmallVec<[u32; 4]>>() {
        panic!()
    } else {
        ()
    };

    // Add(Math, Math, Math)
    struct AddRelation {
        // eclass -> row

        // ==== per eclass ====
        back_references: Vec<SmallVec<[u32; 4]>>,

        // ==== per row ====
        col_a: Vec<u32>,
        col_b: Vec<u32>,
        col_res: Vec<u32>,

        // fd map
        // a, b -> res
        // maintains congruence closure.
        // maintains the set of elements in the relation.
        map: HashMap<(u32, u32), u32>,
    }
    impl AddRelation {
        fn push(&mut self, a: u32, b: u32, res: u32, uf: &mut UnionFind<u32>) {
            if let Some(&res2) = self.map.get(&(a, b)) {
                uf.union(res, res2);
                return;
            }
            self.col_a.push(a);
            self.col_b.push(b);
            self.col_res.push(res);
        }
        fn extend_back_references(&mut self, new_elems: usize) {
            self.back_references
                .extend((0..new_elems).map(|_| SmallVec::new()));
        }
        fn uproot(&mut self, uproot: &[u32]) {
            let mut uprooted_rows = Vec::new();
            for &i in uproot.iter() {
                uprooted_rows.extend(self.back_references[i as usize].drain(..));
            }
            uprooted_rows.sort();

            let mut queue: &[u32] = &uprooted_rows;

            let mut next_delete = queue.pop_front().unwrap_or(u32::MAX);

            let mut write_head = 0usize;
            for read_head in 0..self.col_a.len() {
                if read_head as u32 == next_delete {
                    self.col_a[write_head] = self.col_a[read_head];
                }
            }
        }
    }
}
mod colt2 {

    use super::*;

    trait GHT {
        fn new(v: Vec<usize>) -> Self;
    }
    use std::cell::UnsafeCell;
    enum QMapInner<A, B> {
        Rows(Vec<usize>),
        Map(HashMap<A, B>),
    }
    struct QMap<A, B>(UnsafeCell<QMapInner<A, B>>);
    impl<A: Copy + Hash + Eq, B: GHT> QMap<A, B> {
        /// Returns a mutable reference to the inner map.
        ///
        /// # Safety
        ///
        /// Only sound if no other shared references exist, and single thread.
        unsafe fn get_mut(&self) -> &mut QMapInner<A, B> {
            let ptr: *mut QMapInner<A, B> = self.0.get();
            let mut_ref: &mut QMapInner<A, B> = unsafe { &mut *ptr };
            mut_ref
        }
        fn iter<'a>(&'a self, column: &[A]) -> impl Iterator<Item = A> + use<'a, A, B> {
            unsafe { self.get_mut() }.iter(column)
        }
        fn get<'a>(&'a self, column: &[A], a: A) -> Option<&'a B> {
            unsafe { self.get_mut() }.get(column, a)
        }
    }

    fn test() {
        use itertools::Itertools;
        eclass_wrapper_ty!(A, B, C, D, E);

        let n = 100;

        let (ab, c, de): (Vec<_>, Vec<_>, Vec<_>) = (0..(n as u32))
            .map(|i| ((A(i), B(i)), (C(i)), (D(i), E(i))))
            .multiunzip();

        // R: [A, B, C, D, E]
        // schema: [(A, B), (C), (D, E)]
        let R: QMap<(A, B), QMap<(C), QMap<(D, E), ()>>> = QMap::new((0..n).collect());

        let R_ab = R.get(&ab, (A(0), B(0))).unwrap();

        let R_abc = R_ab.get(&c, C(0)).unwrap();
    }

    impl<A: Copy + Hash + Eq, B: GHT> QMapInner<A, B> {
        fn iter<'a>(&'a mut self, column: &[A]) -> impl Iterator<Item = A> + use<'a, A, B> {
            match self {
                QMapInner::Rows(v) => {
                    // self.force(column);
                    self.iter(column)
                }
                // this should not be reached, right?
                QMapInner::Map(map) => map.keys().copied(),
            }
        }
        fn get<'a>(&'a mut self, column: &[A], a: A) -> Option<&'a B> {
            match self {
                QMapInner::Rows(vec) => {
                    self.force(column);
                    self.get(column, a)
                }
                QMapInner::Map(map) => map.get(&a),
            }
        }
        fn force(&mut self, column: &[A]) {
            match self {
                QMapInner::Map(_) => {}
                QMapInner::Rows(v) => {
                    let mut map: HashMap<A, Vec<usize>> = HashMap::new();
                    for i in v.iter().copied() {
                        map.entry(column[i]).or_default().push(i);
                    }
                    *self = QMapInner::Map(map.into_iter().map(|(k, v)| (k, B::new(v))).collect())
                }
            }
        }
    }
    impl<A, B> GHT for QMap<A, B> {
        fn new(v: Vec<usize>) -> Self {
            Self(QMapInner::Rows(v).into())
        }
    }
}
mod colt {
    use super::*;

    use std::cell::UnsafeCell;

    trait GHT {
        fn new(v: Vec<usize>) -> Self;
    }
    // eclass_wrapper_ty!(X, Y, Z);

    // schema: [(A, B), (C), (D, E)]
    // ty:     LMap<(A, B), LMap<(C), Vec<(D, E)>>

    // QMap<(A, B), QMap<(C), QMap<(D, E), ()>>>

    // struct QMap<A, B>(UnsafeCell<QMapInner<A, B>>);
    enum QMap<A, B> {
        Rows(Vec<usize>),
        Map(HashMap<A, B>),
    }
    impl<A: Copy + Hash + Eq, B: GHT> QMap<A, B> {
        fn iter<'a>(&'a mut self, column: &[A]) -> impl Iterator<Item = A> + use<'a, A, B> {
            match self {
                QMap::Rows(v) => {
                    // self.force(column);
                    self.iter(column)
                }
                // this should not be reached, right?
                QMap::Map(map) => map.keys().copied(),
            }
        }
        fn get<'a>(&'a mut self, column: &[A], a: A) -> Option<&'a B> {
            match self {
                QMap::Rows(vec) => {
                    self.force(column);
                    self.get(column, a)
                }
                QMap::Map(map) => map.get(&a),
            }
        }
        fn force(&mut self, column: &[A]) {
            match self {
                QMap::Map(_) => {}
                QMap::Rows(v) => {
                    let mut map: HashMap<A, Vec<usize>> = HashMap::new();
                    for i in v.iter().copied() {
                        map.entry(column[i]).or_default().push(i);
                    }
                    *self = QMap::Map(map.into_iter().map(|(k, v)| (k, B::new(v))).collect())
                }
            }
        }
    }
    impl<A, B> GHT for QMap<A, B> {
        fn new(v: Vec<usize>) -> Self {
            Self::Rows(v)
        }
    }

    struct Schema {
        // columns
        variables: Vec<usize>,
    }
    impl Schema {
        fn pop(&self) -> Self {
            let mut variables = self.variables.clone();
            variables.remove(0);
            Self { variables }
        }
    }

    struct Colt {
        schema: Schema,
        ty: Ty,
    }
    enum Ty {
        Data(Vec<usize>),
        Map(HashMap<u32, Colt>),
    }
    impl Colt {
        fn new(relation: &Vec<Vec<usize>>, schema: Schema) -> Self {
            Self {
                schema,
                ty: Ty::Data((0..relation[0].len()).collect()),
            }
        }
        fn get(&mut self, relation: &Vec<Vec<usize>>, key: u32) -> Option<Colt> {
            self.force(relation);
            let Ty::Map(map) = &mut self.ty else {
                unreachable!()
            };
            todo!()
        }
        fn force(&mut self, relation: &Vec<Vec<usize>>) {
            todo!()
        }
    }

    // use std::cell::UnsafeCell;
    // struct LazyTrie(UnsafeCell<LazyTrieInner>);
    // enum LazyTrieInner {
    //     Borrowed {
    //         index: Arc<ColumnIndex>,
    //         map: HashMap<Value, LazyTrie>,
    //     },
    //     Delayed(SmallVec<[RowIdx; 4]>),
    //     Sparse(HashMap<Value, LazyTrie>),
    // }
}

fn emit<T>(_: T) {}

mod triangle_join {
    use super::*;
    eclass_wrapper_ty!(X, Y, Z);

    // [[R'(x, y), T(x)], [S(y, z), T(z)]]
    struct Db {
        R_new: Vec<(X, Y)>,
        S: Map<Y, Set<Z>>,
        T: Map<X, Set<Z>>,
    }
    impl Db {
        fn apply_rules(&self) {
            let Self { R_new, S, T } = self;

            for &(x, y) in R_new.iter() {
                let Some(T_x) = T.get(&x) else {
                    continue;
                };

                // ===========
                let Some(S_y) = S.get(&y) else {
                    continue;
                };
                for &z in S_y.iter() {
                    let Some(_) = T_x.get(&z) else {
                        continue;
                    };

                    emit((x, y, z));
                }
            }
        }
    }
}

trait Iter2 {
    type Inner;
    fn iter_(&self) -> impl Iterator<Item = Self::Inner>;
}

impl<A: RelationElement, B: RelationElement> Iter2 for Map<A, B> {
    type Inner = (A, B);

    fn iter_(&self) -> impl Iterator<Item = Self::Inner> {
        self.iter().map(|(a, b)| (*a, *b))
    }
}

// a * c + b * c => (a + b) * c
// Mul(a, c, d), Mul(b, c, e), Add(d, e, f)
// R(a, c, d), S(b, c, e), T(d, e, f)
// this is secretly just a triangle join.
mod distributive_law_inverse {
    use super::*;

    eclass_wrapper_ty!(A, B, C, D, E, F);

    // [R'(a, c, d)], [S(b, c, e)], [T(d, e, f)]
    // [R'(a, c, d), S(c), T(d)], [S(e, b), T(e, f)]
    struct Db {
        // FD: (D, E) -> F (all)
        // FD: (A, C) -> D (all)
        // FD: (B, C) -> E (all)

        // FD means inner values have cardinality 1, and Set can be removed.
        R_new: Vec<(A, C, D)>,
        S: Map<C, Map<E, B>>,
        T: Map<D, Map<E, F>>,
    }
    impl Db {
        fn apply_rules(&self) {
            let Self { R_new, S, T } = self;

            for &(a, c, d) in R_new.iter() {
                let Some((S_c)) = S.get(&c) else {
                    continue;
                };
                let Some((T_d)) = T.get(&d) else {
                    continue;
                };

                // ================================================
                for (e, b) in S_c.iter_() {
                    let Some(&f) = T_d.get(&e) else {
                        continue;
                    };

                    emit((a, b, c, d, e, f));
                }
            }
        }
    }
}

mod star_join {
    use super::*;
    eclass_wrapper_ty!(X, Y, Z, W);

    // R(x, y), S(x, z), T(x, w)
    // [[R'(x, y)], [S(x, z)], [T(x, w)]]
    // [[R'(x, y), S(x), T(x)], [S(z), T(w)]
    struct Db {
        R_new: Vec<(X, Y)>,
        S: Map<X, Set<Z>>,
        T: Map<X, Set<W>>,
    }
    impl Db {
        fn apply_rules(&self) {
            let Self { R_new, S, T } = self;
            for &(x, y) in R_new.iter() {
                let Some(S_x) = S.get(&x) else {
                    continue;
                };
                let Some(T_x) = T.get(&x) else {
                    continue;
                };
                for (&z) in S_x.iter() {
                    for (&w) in T_x.iter() {
                        emit((x, y, z, w));
                    }
                }
            }
        }
    }
}
