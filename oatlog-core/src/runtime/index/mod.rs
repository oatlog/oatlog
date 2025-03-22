use crate::runtime::IndexRow;
use std::{collections::BTreeSet, ops::RangeInclusive};

/// Conceptually a sorted set.
///
/// What these operations actually mean, in what ways their semantics are allowed to be relaxed for
/// performance etc is not yet defined. Run the oatlog-wide tests to see if you messed something
/// up.
pub trait Index {
    type Row: IndexRow;

    fn new() -> Self;
    fn len(&self) -> usize;
    fn iter(&self) -> impl Iterator<Item = <Self::Row as IndexRow>::Inner>;

    /// Iterate through a range.
    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Inner>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Inner>;

    /// Find all rows with `uproots` value in the first column.
    ///
    /// Implementations may
    /// - Remove uprooted rows from the index.
    fn first_column_uproots(
        &mut self,
        uproots: &[<Self::Row as IndexRow>::FirstColumn],
        on_delete: impl FnMut(&[<Self::Row as IndexRow>::Inner]),
    );

    /// Delete rows exactly matching `deletions`.
    fn delete_many(&mut self, deletions: &mut [<Self::Row as IndexRow>::Inner]);

    /// Insert rows on unique keys, performing `merge` on multiple values with identical keys.
    ///
    /// Implementations may
    /// - Insert existing rows into `insertions`.
    /// - Delete `merge`-eliminated rows from `insertions`.
    fn insert_many(
        &mut self,
        insertions: &mut Vec<<Self::Row as IndexRow>::Inner>,
        merge: impl FnMut(Self::Row, Self::Row) -> Self::Row,
    );
}

//pub type IndexImpl<K> = BTreeSetIndex<K>;
pub type IndexImpl<K> = SortedVec<K>;

#[derive(Default, Debug)]
pub struct BTreeSetIndex<R: IndexRow>(BTreeSet<R>);
impl<R: IndexRow> Index for BTreeSetIndex<R> {
    type Row = R;

    fn new() -> Self {
        Self(BTreeSet::new())
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn iter(&self) -> impl Iterator<Item = <Self::Row as IndexRow>::Inner> {
        self.0.iter().map(|r| r.inner())
    }

    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Inner>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Inner> {
        let r = R::new(*r.start())..=R::new(*r.end());
        self.0.range(r.clone()).map(move |ret| ret.inner())
    }

    fn first_column_uproots(
        &mut self,
        uproots: &[<Self::Row as IndexRow>::FirstColumn],
        mut on_delete: impl FnMut(&[<Self::Row as IndexRow>::Inner]),
    ) {
        for &uproot in uproots {
            let range = R::col_val_range(uproot);
            for &row in self.0.range(range) {
                on_delete(R::inner_slice(&[row]));
            }
        }
    }

    fn delete_many(&mut self, deletions: &mut [<Self::Row as IndexRow>::Inner]) {
        for d in R::from_inner_slice_mut(deletions) {
            self.0.remove(d);
        }
    }
    fn insert_many(
        &mut self,
        insertions: &mut Vec<<Self::Row as IndexRow>::Inner>,
        mut merge: impl FnMut(Self::Row, Self::Row) -> Self::Row,
    ) {
        let insertions = R::from_inner_slice_mut(insertions);
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

#[derive(Default, Debug)]
pub struct SortedVec<R>(Vec<R>);
impl<R: IndexRow> Index for SortedVec<R> {
    type Row = R;

    fn new() -> Self {
        Self(Vec::new())
    }
    fn len(&self) -> usize {
        self.0.len()
    }
    fn iter(&self) -> impl Iterator<Item = <Self::Row as IndexRow>::Inner> {
        self.0.iter().map(|r| r.inner())
    }

    fn range(
        &self,
        r: RangeInclusive<<Self::Row as IndexRow>::Inner>,
    ) -> impl Iterator<Item = <Self::Row as IndexRow>::Inner> {
        let start = self.0.partition_point(|&k| k < R::new(*r.start()));
        let end = self.0.partition_point(|&k| k < R::new(*r.end()));
        //println!("range len={} ans={}..{}", self.len(), start, end);
        self.0[start..end].iter().map(|r| r.inner())
    }

    fn first_column_uproots(
        &mut self,
        uproots: &[<Self::Row as IndexRow>::FirstColumn],
        mut on_delete: impl FnMut(&[<Self::Row as IndexRow>::Inner]),
    ) {
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
            on_delete(R::inner_slice(&self.0[i..j]));
            cursor_input = j;
        }
        let len = self.0.len();
        if cursor_input != cursor_output {
            self.0.copy_within(cursor_input..len, cursor_output);
        }
        cursor_output += len - cursor_input;
        self.0.truncate(cursor_output);
    }

    fn delete_many(&mut self, deletions: &mut [<Self::Row as IndexRow>::Inner]) {
        //println!("delete_many len={} deletions.len()={}", self.len(), deletions.len());
        let deletions = R::from_inner_slice_mut(deletions);
        deletions.sort_unstable();
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
        insertions: &mut Vec<<Self::Row as IndexRow>::Inner>,
        mut merge: impl FnMut(Self::Row, Self::Row) -> Self::Row,
    ) {
        //println!("insert_many len={} insertions.len()={}", self.len(), insertions.len());
        self.0
            .extend_from_slice(R::from_inner_slice_mut(insertions));
        self.0.sort_unstable();

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
        if self.len() < insertions.len() {
            insertions.clear();
            insertions.extend_from_slice(R::inner_slice(&self.0));
            //println!("insert_many shrunk insertions to insertions.len()={}", insertions.len());
        }
    }
}

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
