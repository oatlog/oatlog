use crate::runtime::IndexRow;
use std::{collections::BTreeSet, marker::PhantomData, ops::RangeInclusive};

#[cfg(test)]
mod proptest;

/// Conceptually a sorted set.
///
/// What these operations actually mean, in what ways their semantics are allowed to be relaxed for
/// performance etc is not yet defined. Run the oatlog-wide tests to see if you messed something
/// up.
pub trait Index {
    type Row: IndexRow;
    type RowCtx: RowCtx<Row = Self::Row>;

    fn new() -> Self;
    fn len(&self) -> usize;
    fn iter(&self) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr>;

    /// Iterate through a range.
    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Repr>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr>;

    /// Find all rows with `uproots` value in the first column.
    ///
    /// Implementations may
    /// - Remove uprooted rows from the index.
    fn first_column_uproots(
        &mut self,
        uproots: &[<Self::Row as IndexRow>::FirstColumn],
        on_delete: impl FnMut(&[<Self::Row as IndexRow>::Repr]),
    );

    /// Delete rows exactly matching `deletions`.
    fn delete_many(&mut self, deletions: &mut [<Self::Row as IndexRow>::Repr]);

    /// Insert rows on unique keys, performing `merge` on multiple values with identical keys.
    ///
    /// Implementations may
    /// - Insert existing rows into `insertions`.
    /// - Delete `merge`-eliminated rows from `insertions`.
    fn insert_many(
        &mut self,
        insertions: &mut Vec<<Self::Row as IndexRow>::Repr>,
        merge: impl FnMut(Self::Row, Self::Row) -> Self::Row,
    );

    #[cfg(test)]
    fn contents_sorted(&self) -> Vec<<Self::Row as IndexRow>::Repr> {
        let mut contents: Vec<_> = self.iter().collect();
        contents.sort();
        contents
    }

    /// Remove everything from new that is already in self.
    /// Motivation is to shrink the new set to stuff that is actually new.
    ///
    /// TODO: implementation is naive, should probably iterate index and new to avoid the O(log n) factor.
    fn filter_existing(&self, potential_inserts: &mut Vec<<Self::Row as IndexRow>::Repr>) {
        Self::RowCtx::sort(Self::Row::from_inner_slice_mut(potential_inserts));
        potential_inserts.dedup();
        potential_inserts.retain(|&x| self.range(x..=x).next().is_none());
    }
}

pub trait RowCtx: Default + std::fmt::Debug {
    type Row: IndexRow;
    fn sort(slice: &mut [Self::Row]);
}

#[derive(Default, Debug, Clone)]
pub struct StdSortCtx<R: Ord>(PhantomData<*const R>);
impl<R: IndexRow> RowCtx for StdSortCtx<R> {
    type Row = R;
    fn sort(slice: &mut [Self::Row]) {
        slice.sort_unstable();
    }
}

#[derive(Default, Debug, Clone)]
pub struct RadixSortCtx<
    R: Ord + Copy + voracious_radix_sort::Radixable<K>,
    K: Default + std::fmt::Debug + voracious_radix_sort::RadixKey,
>(PhantomData<*const (R, K)>);
impl<
    K: Default + std::fmt::Debug + voracious_radix_sort::RadixKey,
    R: IndexRow + voracious_radix_sort::Radixable<K>,
> RowCtx for RadixSortCtx<R, K>
{
    type Row = R;
    fn sort(slice: &mut [Self::Row]) {
        use voracious_radix_sort::RadixSort;
        slice.voracious_sort();
    }
}

// pub type IndexImpl<RC> = BTreeSetIndex<RC>;
pub type IndexImpl<RC> = SortedVec<RC>;

#[derive(Default, Debug)]
pub struct BTreeSetIndex<RC: RowCtx>(BTreeSet<RC::Row>, RC);
impl<RC: RowCtx> Index for BTreeSetIndex<RC> {
    type Row = RC::Row;
    type RowCtx = RC;

    fn new() -> Self {
        Self(BTreeSet::new(), RC::default())
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn iter(&self) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr> {
        self.0.iter().map(|r| r.inner())
    }

    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Repr>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr> {
        let r = Self::Row::new(*r.start())..=Self::Row::new(*r.end());
        self.0.range(r.clone()).map(move |ret| ret.inner())
    }

    fn first_column_uproots(
        &mut self,
        uproots: &[<Self::Row as IndexRow>::FirstColumn],
        mut on_delete: impl FnMut(&[<Self::Row as IndexRow>::Repr]),
    ) {
        for &uproot in uproots {
            let range = Self::Row::col_val_range(uproot);
            for &row in self.0.range(range) {
                on_delete(Self::Row::inner_slice(&[row]));
            }
        }
    }

    fn delete_many(&mut self, deletions: &mut [<Self::Row as IndexRow>::Repr]) {
        for d in Self::Row::from_inner_slice_mut(deletions) {
            self.0.remove(d);
        }
    }
    fn insert_many(
        &mut self,
        insertions: &mut Vec<<Self::Row as IndexRow>::Repr>,
        mut merge: impl FnMut(Self::Row, Self::Row) -> Self::Row,
    ) {
        let insertions = Self::Row::from_inner_slice_mut(insertions);
        // This is much slower than necessary, limited by the stable BTreeSet API.
        for &mut i in insertions {
            if self.0.contains(&i) {
                continue;
            }
            let mut new = i;
            let mut to_remove = BTreeSet::new();
            for &other in self.0.range(i..).take_while(|j| i.key() == j.key()) {
                to_remove.insert(other);
                new = merge(new, other);
            }
            for &other in self.0.range(..i).rev().take_while(|j| i.key() == j.key()) {
                to_remove.insert(other);
                new = merge(new, other);
            }
            for r in to_remove {
                self.0.remove(&r);
            }
            let ok = self.0.insert(new);
            assert!(ok);
        }
    }
}
#[derive(Default, Debug, Clone)]
pub struct SortedVec<RC: RowCtx>(Vec<RC::Row>, RC);
impl<RC: RowCtx> SortedVec<RC> {
    /// Arguments
    /// - insertions: permuted.
    /// - deferred_insertions: initially empty, pushed to when `already_canon(existing_row) == false`.
    /// - uf
    /// - scratch: initially and finally empty, used to avoid allocation.
    /// - already_canon: canonicalize a row and return whether it already was canonical.
    /// - merge
    // TODO: Maybe remove from `insertions` entries that already exist (can be done efficiently with some extra bookkeeping)
    pub fn sorted_vec_update<UF>(
        &mut self,
        insertions: &mut Vec<<RC::Row as IndexRow>::Repr>,
        deferred_insertions: &mut Vec<<RC::Row as IndexRow>::Repr>,
        scratch: &mut Vec<<RC::Row as IndexRow>::Repr>,
        uf: &mut UF,
        mut already_canon: impl FnMut(&mut UF, &mut <RC::Row as IndexRow>::Repr) -> bool,
        mut merge: impl FnMut(&mut UF, &mut RC::Row, RC::Row),
    ) {
        let insertions = RC::Row::from_inner_slice_mut(insertions);
        let deferred_insertions = RC::Row::from_inner_vec(deferred_insertions);
        let scratch = RC::Row::from_inner_vec(scratch);

        RC::sort(insertions);

        assert!(scratch.is_empty());

        let mut push_item = |mut row: RC::Row| {
            if already_canon(uf, RC::Row::inner_mut(&mut row)) {
                match scratch.last_mut() {
                    Some(head) if *head == row => {}
                    Some(head) if head.key() == row.key() => merge(uf, head, row),
                    _ => scratch.push(row),
                }
            } else {
                deferred_insertions.push(row);
            }
        };

        let mut i = 0;
        let mut j = 0;
        if insertions.len() > 0 && self.0.len() > 0 {
            'done: loop {
                while insertions[i] <= self.0[j] {
                    push_item(insertions[i]);
                    i += 1;
                    if i == insertions.len() {
                        break 'done;
                    }
                }
                while self.0[j] <= insertions[i] {
                    push_item(self.0[j]);
                    j += 1;
                    if j == self.0.len() {
                        break 'done;
                    }
                }
            }
        }
        self.0[j..].iter().copied().for_each(&mut push_item);
        insertions[i..].iter().copied().for_each(&mut push_item);

        std::mem::swap(&mut self.0, scratch);
        scratch.clear();
    }
    pub fn minus(&self, rhs: &Self) -> impl Iterator<Item = <RC::Row as IndexRow>::Repr> {
        let mut i = 0;
        self.0
            .iter()
            .copied()
            .filter(move |&row| {
                while i < rhs.0.len() && rhs.0[i] < row {
                    i += 1;
                }
                rhs.0.get(i).map_or(true, |&other| other != row)
            })
            .map(RC::Row::inner)
    }
    pub fn recreate_from(&mut self, other: &[<RC::Row as IndexRow>::Repr]) {
        assert_eq!(
            std::mem::size_of::<<RC::Row as IndexRow>::Value>(),
            0,
            "recreate_from requires the absence of implicit rules"
        );

        self.0.clear();
        self.0.extend_from_slice(RC::Row::from_inner_slice(other));
        RC::sort(&mut self.0);
    }
    pub fn as_slice(&self) -> &[<RC::Row as IndexRow>::Repr] {
        RC::Row::inner_slice(&self.0)
    }
}
impl<RC: RowCtx> Index for SortedVec<RC> {
    type Row = RC::Row;
    type RowCtx = RC;

    fn new() -> Self {
        Self(Vec::new(), RC::default())
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn iter(&self) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr> {
        self.0.iter().map(|r| r.inner())
    }

    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Repr>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Repr> {
        let start = self.0.partition_point(|&k| k < Self::Row::new(*r.start()));
        let end = self.0.partition_point(|&k| k <= Self::Row::new(*r.end()));

        //
        // start = 1
        // end = 4
        //
        // we have the invariant that elements are unique.
        //
        // k < start: [1, 0, 0, 0, 0, 0]
        // k <= end:  [1, 1, 1, 1, 1, 0]
        // arr:       [0, 1, 2, 3, 4, 5]
        //                ^        ^
        //                |--------|

        //println!("range len={} ans={}..{}", self.len(), start, end);
        self.0[start..end].iter().map(|r| r.inner())
    }

    /// NOTE: requires that uprots is sorted.
    fn first_column_uproots(
        &mut self,
        uproots: &[<Self::Row as IndexRow>::FirstColumn],
        mut on_delete: impl FnMut(&[<Self::Row as IndexRow>::Repr]),
    ) {
        debug_assert!(uproots.is_sorted());
        //println!("first_column_uproots len={} uproots.len()={}", self.len(), uproots.len());
        let mut uproots = uproots.iter().copied();

        let mut cursor_input = 0;
        let mut cursor_output = 0;
        loop {
            assert!(cursor_output <= cursor_input);
            let Some(uproot) = uproots.next() else { break };
            let i =
                cursor_input + self.0[cursor_input..].partition_point(|&k| k.first_col() < uproot);
            let j =
                cursor_input + self.0[cursor_input..].partition_point(|&k| k.first_col() <= uproot);

            assert!(cursor_input <= i);
            assert!(i <= j);
            if cursor_input != cursor_output {
                self.0.copy_within(cursor_input..i, cursor_output);
            }
            cursor_output += i - cursor_input;
            on_delete(Self::Row::inner_slice(&self.0[i..j]));
            cursor_input = j;
        }
        let len = self.0.len();
        if cursor_input != cursor_output {
            self.0.copy_within(cursor_input..len, cursor_output);
        }
        cursor_output += len - cursor_input;
        self.0.truncate(cursor_output);
    }

    fn delete_many(&mut self, deletions: &mut [<Self::Row as IndexRow>::Repr]) {
        //println!("delete_many len={} deletions.len()={}", self.len(), deletions.len());
        let deletions = Self::Row::from_inner_slice_mut(deletions);
        RC::sort(deletions);
        let mut deletions = deletions.iter().copied();

        let mut cursor_input = 0;
        let mut cursor_output = 0;
        let len = self.0.len();
        while cursor_input < len {
            assert!(cursor_output <= cursor_input);
            let Some(delete) = deletions.next() else {
                break;
            };
            if delete < self.0[cursor_input] {
                continue;
            }
            let i = {
                // Square-root decomposition-like search for next deletion point
                let mut i = cursor_input;

                let chunk = (len - cursor_input) / (deletions.len() + 2);
                if chunk >= 4 {
                    while i + chunk - 1 < len && self.0[i + chunk - 1] < delete {
                        i += chunk;
                    }
                }

                while i < len && self.0[i] < delete {
                    i += 1;
                }
                if i == len {
                    break;
                }
                i
            };
            let successful_deletion = self.0[i] == delete;

            assert!(cursor_input <= i);
            if cursor_input != i && cursor_input != cursor_output {
                self.0.copy_within(cursor_input..i, cursor_output);
            }
            cursor_output += i - cursor_input;
            cursor_input = i + successful_deletion as usize;
        }
        if cursor_input != cursor_output {
            self.0.copy_within(cursor_input..len, cursor_output);
        }
        cursor_output += len - cursor_input;
        self.0.truncate(cursor_output);
    }
    fn insert_many(
        &mut self,
        insertions: &mut Vec<<Self::Row as IndexRow>::Repr>,
        mut merge: impl FnMut(Self::Row, Self::Row) -> Self::Row,
    ) {
        //println!("insert_many len={} insertions.len()={}", self.len(), insertions.len());
        self.0
            .extend_from_slice(Self::Row::from_inner_slice_mut(insertions));
        //self.0.sort_unstable();
        RC::sort(&mut self.0);

        let mut cursor_input = 0;
        let mut cursor_output = 0;
        let len = self.0.len();
        while cursor_input < len {
            let i = {
                let mut i = cursor_input;
                while i + 1 < len && self.0[i].key() != self.0[i + 1].key() {
                    i += 1;
                }
                if i + 1 == len {
                    break;
                }
                i
            };
            assert!(self.0[i].key() == self.0[i + 1].key());
            let j = {
                let mut j = i + 2;
                while j < len && self.0[i].key() == self.0[j].key() {
                    j += 1;
                }
                j
            };
            let successor = self.0[i..j]
                .iter()
                .copied()
                // NOTE: we introduce a requirement here that all merge functions are commutative
                // and associative.
                // If we care about this, we should use a stable sort so that we get:
                // merge(merge(merge(old, new), new), new)
                // Even though this shouldn't matter, it might cause issues because we are
                // no longer unifying by size, so we introduce performance differences that might
                // just be due to merge order.
                .reduce(|a, b| if a == b { a } else { merge(a, b) })
                .unwrap();

            if cursor_input != cursor_output {
                self.0.copy_within(cursor_input..i, cursor_output);
            }
            cursor_output += i - cursor_input;
            self.0[cursor_output] = successor;
            cursor_output += 1;
            cursor_input = j;
        }
        if cursor_input != cursor_output {
            self.0.copy_within(cursor_input..len, cursor_output);
        }
        cursor_output += len - cursor_input;
        self.0.truncate(cursor_output);
        // if self.len() < insertions.len() {
        //     // panic!();
        //     insertions.clear();
        //     insertions.extend_from_slice(Self::Row::inner_slice(&self.0));
        //     //println!("insert_many shrunk insertions to insertions.len()={}", insertions.len());
        // }
    }
}

/// dedup `vec[offset..]`.
pub fn dedup_suffix<T: Copy + PartialOrd>(vec: &mut Vec<T>, offset: usize) {
    assert!(offset <= vec.len());
    let s = &mut vec[offset..];
    if s.len() <= 1 {
        return;
    }
    debug_assert!(s.is_sorted());
    let mut i = 0;
    let mut j = 1;
    loop {
        while j < s.len() && s[i] == s[j] {
            j += 1;
        }
        if j == s.len() {
            break;
        }
        i += 1;
        s[i] = s[j];
    }
    vec.truncate(offset + i + 1);
}
#[cfg(test)]
mod test {
    #[test]
    fn dedup_suffix_smoke() {
        let mut v = vec![10, 10, 2, 2, 3, 4, 4];
        super::dedup_suffix(&mut v, 2);
        assert_eq!(v, &[10, 10, 2, 3, 4]);
    }
    #[test]
    fn dedup_suffix_empty() {
        let mut v: Vec<i32> = vec![];
        super::dedup_suffix(&mut v, 0);
        assert_eq!(v, &[]);
    }
}
